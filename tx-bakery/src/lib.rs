use crate::error::{Error, Result};
use crate::metadata::TransactionMetadata;
use crate::time::time_range_into_slots;
use crate::wallet::Wallet;
use anyhow::anyhow;
use cardano_serialization_lib as csl;
use cardano_serialization_lib::fees::LinearFee;
use cardano_serialization_lib::tx_builder::mint_builder::{MintBuilder, MintWitness};
use cardano_serialization_lib::tx_builder::tx_inputs_builder::{
    PlutusScriptSource, PlutusWitness, TxInputsBuilder,
};
use cardano_serialization_lib::tx_builder::{
    TransactionBuilder, TransactionBuilderConfig, TransactionBuilderConfigBuilder,
};
use chain_query::{ChainQuery, EraSummary, Network, ProtocolParameters};
use chrono::{DateTime, Utc};
use csl::plutus::{PlutusScripts, Redeemers};
use csl::tx_builder::tx_inputs_builder::DatumSource;
use csl::TransactionWitnessSet;
use num_bigint::BigInt;
use plutus_ledger_api::plutus_data::IsPlutusData;
use plutus_ledger_api::v2::address::{Address, Credential};
use plutus_ledger_api::v2::crypto::PaymentPubKeyHash;
use plutus_ledger_api::v2::datum::{Datum, DatumHash, OutputDatum};
use plutus_ledger_api::v2::redeemer::Redeemer;
use plutus_ledger_api::v2::script::{MintingPolicyHash, ValidatorHash};
use plutus_ledger_api::v2::transaction::{
    ScriptPurpose, TransactionHash, TransactionInfo, TransactionInput, TransactionOutput, TxInInfo,
};
use plutus_ledger_api::v2::value::{CurrencySymbol, Value};
use std::collections::BTreeMap;
use submitter::Submitter;
use utils::pla_to_csl::{TransactionOutputExtraInfo, TryToCSL, TryToCSLWithDef};
use utils::script::ScriptOrRef;

pub mod chain_query;
pub mod error;
pub mod metadata;
pub mod submitter;
pub mod time;
pub mod tx_info_builder;
pub mod utils;
pub mod wallet;

/// Transaction builder
/// The sole purpose of this component is to convert a raw TransactionInfo (dough) into a fully
/// baked valid transaction
/// TxBakery won't change it's internal state once initialized
pub struct TxBakery {
    config: TransactionBuilderConfig,
    data_cost: csl::DataCost,
    cost_models: csl::plutus::Costmdls,
    system_start: DateTime<Utc>,
    era_summaries: Vec<EraSummary>,
    network_id: u8,
    collateral_amount: u64,
}

/// TransactionInfo with additional context required to build a valid transactions
#[derive(Clone, Debug)]
pub struct TxWithCtx<'a> {
    pub tx_info: &'a TransactionInfo,
    pub validators: &'a BTreeMap<ValidatorHash, ScriptOrRef>,
    pub minting_policies: &'a BTreeMap<MintingPolicyHash, ScriptOrRef>,
    pub collateral_strategy: &'a CollateralStrategy,
    pub change_strategy: &'a ChangeStrategy,
    pub metadata: Option<&'a TransactionMetadata>,
    pub ex_units_map:
        Option<&'a BTreeMap<(csl::plutus::RedeemerTag, csl::utils::BigNum), csl::plutus::ExUnits>>,
}

/// Options to deal with change outputs and collateral returns
#[derive(Clone, Debug)]
pub enum ChangeStrategy {
    /// Send all change to an address
    Address(Address),
    /// Use the last output of the TransactionInfo as change output (modify it's value)
    /// Collateral returns are following the address of the last output
    LastOutput,
}

impl<'a> TxWithCtx<'a> {
    pub fn new(
        tx_info: &'a TransactionInfo,
        validators: &'a BTreeMap<ValidatorHash, ScriptOrRef>,
        minting_policies: &'a BTreeMap<MintingPolicyHash, ScriptOrRef>,
        collateral_strategy: &'a CollateralStrategy,
        change_strategy: &'a ChangeStrategy,
    ) -> Self {
        TxWithCtx {
            tx_info,
            validators,
            minting_policies,
            collateral_strategy,
            change_strategy,
            metadata: None,
            ex_units_map: None,
        }
    }

    pub fn with_metadata(mut self, metadata: &'a TransactionMetadata) -> Self {
        self.metadata = Some(metadata);
        self
    }

    pub fn with_ex_units(
        mut self,
        ex_units_map: &'a BTreeMap<
            (csl::plutus::RedeemerTag, csl::utils::BigNum),
            csl::plutus::ExUnits,
        >,
    ) -> Self {
        self.ex_units_map = Some(ex_units_map);
        self
    }
}

/// Options to deal with collateral selection
#[derive(Clone, Debug)]
pub enum CollateralStrategy {
    /// Automatically pick a suitable UTxO from the transaction inputs
    Automatic,
    /// Explicitly set a UTxO (doesn't have to be an input UTxO)
    Explicit(TxInInfo),
    /// No collateral (for transaction without scripts)
    None,
}

impl TxBakery {
    /// Query all the parameters required to build a transaction and store it for later use.
    pub async fn init(chain_query: &impl ChainQuery) -> Result<Self> {
        let protocol_params = chain_query.query_protocol_params().await?;
        let system_start = chain_query.query_system_start().await?;
        let era_summaries = chain_query.query_era_summaries().await?;
        let network = chain_query.get_network();

        Self::init_with_config(&network, &protocol_params, system_start, era_summaries).await
    }

    /// Init TxBakey with the required configurations
    pub async fn init_with_config(
        network: &Network,
        protocol_params: &ProtocolParameters,
        system_start: DateTime<Utc>,
        era_summaries: Vec<EraSummary>,
    ) -> Result<Self> {
        let data_cost =
            csl::DataCost::new_coins_per_byte(&protocol_params.min_utxo_deposit_coefficient);

        let linear_fee = &LinearFee::new(
            &protocol_params.min_fee_coefficient,
            &protocol_params.min_fee_constant,
        );
        let config = TransactionBuilderConfigBuilder::new()
            .fee_algo(linear_fee)
            .pool_deposit(&protocol_params.stake_pool_deposit)
            .key_deposit(&protocol_params.stake_credential_deposit)
            .max_value_size(protocol_params.max_value_size.ok_or(
                Error::MissingProtocolParameter("max_value_size".to_string()),
            )?)
            .max_tx_size(protocol_params.max_transaction_size.ok_or(
                Error::MissingProtocolParameter("max_transaction_size".to_string()),
            )?)
            .coins_per_utxo_byte(&protocol_params.min_utxo_deposit_coefficient)
            .ex_unit_prices(&protocol_params.script_execution_prices.clone().ok_or(
                Error::MissingProtocolParameter("script_execution_prices".to_string()),
            )?)
            .prefer_pure_change(true)
            .build()
            .unwrap();

        let collateral_amount = 5_000_000;

        Ok(TxBakery {
            config,
            data_cost,
            cost_models: protocol_params
                .plutus_cost_models
                .clone()
                .ok_or(Error::MissingProtocolParameter("cost_models".to_string()))?,
            system_start,
            era_summaries,
            network_id: network.to_network_id(),
            collateral_amount,
        })
    }

    fn create_tx_builder(&self) -> TransactionBuilder {
        TransactionBuilder::new(&self.config)
    }

    fn mk_inputs(
        &self,
        inputs: &Vec<TxInInfo>,
        input_redeemers: &BTreeMap<TransactionInput, Redeemer>,
        input_datums: &BTreeMap<DatumHash, Datum>,
        scripts: &BTreeMap<ValidatorHash, ScriptOrRef>,
        ex_units_map: Option<
            &BTreeMap<(csl::plutus::RedeemerTag, csl::utils::BigNum), csl::plutus::ExUnits>,
        >,
    ) -> Result<TxInputsBuilder> {
        let mut tx_inputs_builder = TxInputsBuilder::new();

        inputs
            .iter()
            .enumerate()
            .map(|(idx, TxInInfo { reference, output })| {
                let redeemer = input_redeemers.get(reference);

                match redeemer {
                    None => {
                        tx_inputs_builder.add_input(
                            &output.address.try_to_csl_with(self.network_id)?,
                            &reference.try_to_csl()?,
                            &output.value.try_to_csl()?,
                        );
                        Ok(())
                    }
                    Some(redeemer) => {
                        let validator_hash = match &output.address.credential {
                            Credential::Script(validator_hash) => Ok(validator_hash),
                            _ => Err(Error::Internal(anyhow!(
                                "A public key transaction input should not have a redeemer."
                            ))),
                        }?;
                        let script_or_ref = scripts
                            .get(&validator_hash)
                            .ok_or(Error::MissingValidatorScript(validator_hash.clone()))?;

                        let csl_redeemer = redeemer.try_to_csl_with((
                            &csl::plutus::RedeemerTag::new_spend(),
                            idx as u64,
                        ))?;

                        let csl_redeemer = match ex_units_map {
                            Some(ex_units_map) => {
                                Self::apply_ex_units(&csl_redeemer, ex_units_map)?
                            }
                            None => csl_redeemer,
                        };

                        let script_source = match &script_or_ref {
                            ScriptOrRef::PlutusScript(script, _) => {
                                PlutusScriptSource::new(&script)
                            }
                            ScriptOrRef::RefScript(tx_in, _, lang) => {
                                PlutusScriptSource::new_ref_input_with_lang_ver(
                                    &validator_hash.0.try_to_csl()?,
                                    &tx_in.try_to_csl()?,
                                    &lang.into(),
                                )
                            }
                        };

                        let datum_source = match &output.datum {
                            OutputDatum::DatumHash(dh) => {
                                let Datum(input_datum) = input_datums
                                    .get(&dh)
                                    .ok_or(Error::MissingDatum(dh.clone()))?;

                                Some(DatumSource::new(&input_datum.try_to_csl()?))
                            }

                            OutputDatum::InlineDatum(Datum(input_datum)) => {
                                Some(match &script_or_ref {
                                    ScriptOrRef::PlutusScript(_, _) => {
                                        DatumSource::new(&input_datum.try_to_csl()?)
                                    }

                                    ScriptOrRef::RefScript(tx_in, _, _) => {
                                        DatumSource::new_ref_input(&tx_in.try_to_csl()?)
                                    }
                                })
                            }

                            OutputDatum::None => None,
                        };

                        let tx_input_witness = match datum_source {
                            Some(datum_source) => PlutusWitness::new_with_ref(
                                &script_source,
                                &datum_source,
                                &csl_redeemer,
                            ),
                            None => PlutusWitness::new_with_ref_without_datum(
                                &script_source,
                                &csl_redeemer,
                            ),
                        };

                        tx_inputs_builder.add_plutus_script_input(
                            &tx_input_witness,
                            &reference.try_to_csl()?,
                            &output.value.try_to_csl()?,
                        );

                        Ok(())
                    }
                }
            })
            .collect::<Result<_>>()?;

        Ok(tx_inputs_builder)
    }

    /// Add transaction outputs to the builder
    /// If the change strategy if LastOutput, then we are postponing the addition of this output
    /// to balancing
    fn mk_outputs(
        &self,
        outputs: &Vec<TransactionOutput>,
        change_strategy: &ChangeStrategy,
        minting_policies: &BTreeMap<MintingPolicyHash, ScriptOrRef>,
        validators: &BTreeMap<ValidatorHash, ScriptOrRef>,
    ) -> Result<Vec<csl::TransactionOutput>> {
        let normal_outputs = match change_strategy {
            ChangeStrategy::Address(_) => outputs.as_slice(),
            ChangeStrategy::LastOutput => &outputs[..(outputs.len() - 1)],
        };
        Ok(normal_outputs
            .iter()
            .map(|output| {
                output.try_to_csl_with(TransactionOutputExtraInfo {
                    minting_policies,
                    validators,
                    network_id: self.network_id,
                    data_cost: &self.data_cost,
                })
            })
            .collect::<std::result::Result<_, _>>()?)
    }

    fn mk_mints(
        tx_mint: &Value,
        mint_redeemers: &BTreeMap<MintingPolicyHash, Redeemer>,
        scripts: &BTreeMap<MintingPolicyHash, ScriptOrRef>,
        ex_units_map: Option<
            &BTreeMap<(csl::plutus::RedeemerTag, csl::utils::BigNum), csl::plutus::ExUnits>,
        >,
    ) -> Result<MintBuilder> {
        let mut mint_builder = MintBuilder::new();
        tx_mint
            .0
            .iter()
            .filter_map(|(cur_sym, assets)| match cur_sym {
                CurrencySymbol::NativeToken(minting_policy_hash) => {
                    Some((minting_policy_hash, assets))
                }
                CurrencySymbol::Ada => None,
            })
            .enumerate()
            .map(|(idx, (minting_policy_hash, assets))| {
                assets
                    .iter()
                    .map(|(token_name, amount)| {
                        let script_or_ref = scripts.get(&minting_policy_hash).ok_or(
                            Error::MissingMintingPolicyScript(minting_policy_hash.clone()),
                        )?;
                        let redeemer = mint_redeemers
                            .get(&minting_policy_hash)
                            .ok_or(Error::MissingMintRedeemer(minting_policy_hash.clone()))?;

                        let csl_redeemer = redeemer
                            .try_to_csl_with((&csl::plutus::RedeemerTag::new_mint(), idx as u64))?;

                        let csl_redeemer = match ex_units_map {
                            Some(ex_units_map) => {
                                Self::apply_ex_units(&csl_redeemer, ex_units_map)?
                            }
                            None => csl_redeemer,
                        };

                        let script_source = match &script_or_ref {
                            ScriptOrRef::PlutusScript(script, _) => {
                                PlutusScriptSource::new(&script)
                            }
                            ScriptOrRef::RefScript(tx_in, _, lang) => {
                                PlutusScriptSource::new_ref_input_with_lang_ver(
                                    &minting_policy_hash.0.try_to_csl()?,
                                    &tx_in.try_to_csl()?,
                                    &lang.into(),
                                )
                            }
                        };

                        let mint_witness =
                            MintWitness::new_plutus_script(&script_source, &csl_redeemer);
                        mint_builder.add_asset(
                            &mint_witness,
                            &token_name.try_to_csl()?,
                            &amount.try_to_csl()?,
                        );
                        Ok(())
                    })
                    .collect::<Result<()>>()
            })
            .collect::<Result<()>>()?;

        Ok(mint_builder)
    }

    /// Find a suitable UTxO to be used as a collateral. The UTxO has to meet the following
    /// criteria:
    /// - must be at a pub key address with
    /// - must have at least the configured amount of Ada
    ///         (TODO: we could calculate the exact minimum collateral amount using protocol params)
    ///
    fn find_collateral(&self, tx_inputs: &Vec<TxInInfo>) -> Option<TxInInfo> {
        tx_inputs
            .iter()
            .find(
                |TxInInfo {
                     reference: _,
                     output,
                 }| {
                    if let Credential::PubKey(_) = output.address.credential {
                        let ada_amount = output.value.get_ada_amount();
                        ada_amount > BigInt::from(self.collateral_amount)
                    } else {
                        false
                    }
                },
            )
            .cloned()
    }

    fn mk_collateral(&self, tx_input: &TxInInfo) -> Result<TxInputsBuilder> {
        let mut tx_inputs_builder = TxInputsBuilder::new();
        let TxInInfo { reference, output } = tx_input;
        tx_inputs_builder.add_input(
            &output.address.try_to_csl_with(self.network_id)?,
            &reference.try_to_csl()?,
            &output.value.try_to_csl()?,
        );

        Ok(tx_inputs_builder)
    }

    pub fn mk_tx_builder(&self, tx: &TxWithCtx<'_>) -> Result<csl::tx_builder::TransactionBuilder> {
        let mut tx_builder = self.create_tx_builder();

        let (input_redeemers, mint_redeemers) = tx.tx_info.redeemers.0.iter().fold(
            (BTreeMap::new(), BTreeMap::new()),
            |(mut input_reds, mut mint_reds), (purpose, red)| {
                match purpose {
                    ScriptPurpose::Spending(tx_in) => {
                        input_reds.insert(tx_in.clone(), red.clone());
                    }
                    ScriptPurpose::Minting(CurrencySymbol::NativeToken(mint_pol_hash)) => {
                        mint_reds.insert(mint_pol_hash.clone(), red.clone());
                    }

                    _ => {}
                };
                (input_reds, mint_reds)
            },
        );

        let input_datums = BTreeMap::from(
            tx.tx_info
                .datums
                .0
                .iter()
                .cloned()
                .collect::<BTreeMap<_, _>>(),
        );

        tx_builder.set_inputs(&self.mk_inputs(
            &tx.tx_info.inputs,
            &input_redeemers,
            &input_datums,
            &tx.validators,
            tx.ex_units_map,
        )?);

        tx.tx_info
            .reference_inputs
            .iter()
            .map(
                |TxInInfo {
                     reference,
                     output: _,
                 }| {
                    tx_builder.add_reference_input(&reference.try_to_csl()?);
                    Ok(())
                },
            )
            .collect::<Result<()>>()?;

        tx_builder.set_mint_builder(&TxBakery::mk_mints(
            &tx.tx_info.mint,
            &mint_redeemers,
            &tx.minting_policies,
            tx.ex_units_map,
        )?);

        let collateral_return_address = match tx.change_strategy {
            ChangeStrategy::Address(addr) => addr,
            ChangeStrategy::LastOutput => {
                &tx.tx_info
                    .outputs
                    .last()
                    .ok_or(Error::MissingChangeOutput)?
                    .address
            }
        };

        match &tx.collateral_strategy {
            CollateralStrategy::Automatic => {
                let tx_input = self
                    .find_collateral(&tx.tx_info.inputs)
                    .ok_or(Error::MissingCollateral)?;
                let collateral = self.mk_collateral(&tx_input)?;
                tx_builder.set_collateral(&collateral);
                tx_builder
                    .set_total_collateral_and_return(
                        &csl::utils::to_bignum(self.collateral_amount),
                        &collateral_return_address.try_to_csl_with(self.network_id)?,
                    )
                    .map_err(|source| Error::TransactionBuildError(anyhow!(source)))?;
            }
            CollateralStrategy::Explicit(tx_input) => {
                let collateral = self.mk_collateral(&tx_input)?;
                tx_builder.set_collateral(&collateral);
                tx_builder
                    .set_total_collateral_and_return(
                        &csl::utils::to_bignum(self.collateral_amount),
                        &collateral_return_address.try_to_csl_with(self.network_id)?,
                    )
                    .map_err(|source| Error::TransactionBuildError(anyhow!(source)))?;
            }
            CollateralStrategy::None => {}
        };

        self.mk_outputs(
            &tx.tx_info.outputs,
            tx.change_strategy,
            tx.minting_policies,
            tx.validators,
        )?
        .iter()
        .map(|tx_out| {
            tx_builder
                .add_output(&tx_out)
                .map_err(|source| Error::TransactionBuildError(anyhow!(source)))
        })
        .collect::<Result<_>>()?;

        let (validity_start, ttl) = time_range_into_slots(
            &self.era_summaries,
            &self.system_start,
            tx.tx_info.valid_range.clone(),
        )?;

        if let Some(validity_start) = validity_start {
            tx_builder.set_validity_start_interval_bignum(validity_start);
        }
        if let Some(ttl) = ttl {
            tx_builder.set_ttl_bignum(&ttl);
        }

        tx.tx_info
            .signatories
            .iter()
            .map(|PaymentPubKeyHash(pkh)| {
                tx_builder.add_required_signer(&pkh.try_to_csl()?);
                Ok(())
            })
            .collect::<Result<()>>()?;

        if let Some(metadata) = tx.metadata {
            tx_builder.set_metadata(&metadata.try_into()?);
        }

        Ok(tx_builder)
    }

    pub fn mk_tx_body(&self, tx: &TxWithCtx<'_>) -> Result<csl::TransactionBody> {
        let tx_builder = self.mk_tx_builder(tx)?;

        let (datums, redeemers) = TxBakery::extract_witnesses(&tx.tx_info)?;

        self.balance_transaction(tx, tx_builder, &datums, &redeemers)
    }

    /// Extract witness datums and redeemers from TransactionInfo
    /// Inline datums are embedded in the transaction body, so they won't appear here
    /// Redeemers execution units are set to 0
    fn extract_witnesses(
        tx_info: &TransactionInfo,
    ) -> Result<(Vec<csl::plutus::PlutusData>, Vec<csl::plutus::Redeemer>)> {
        let datums = tx_info
            .datums
            .0
            .iter()
            .map(|(_dh, Datum(d))| Ok(d.to_plutus_data().try_to_csl()?))
            .collect::<Result<_>>()?;

        let redeemers = tx_info
            .redeemers
            .0
            .iter()
            .filter_map(|(script_purpose, red)| match script_purpose {
                ScriptPurpose::Spending(reference) => tx_info
                    .inputs
                    .iter()
                    .enumerate()
                    .find(|(_idx, tx_input)| &tx_input.reference == reference)
                    .map(|(idx, _)| {
                        Ok(red.try_to_csl_with((
                            &csl::plutus::RedeemerTag::new_spend(),
                            idx as u64,
                        ))?)
                    }),
                ScriptPurpose::Minting(reference) => tx_info
                    .mint
                    .0
                    .iter()
                    .filter(|(cur_sym, _)| match cur_sym {
                        CurrencySymbol::NativeToken(_) => true,
                        CurrencySymbol::Ada => false,
                    })
                    .enumerate()
                    .find(|(_idx, (currency_symbol, _assets))| currency_symbol == &reference)
                    .map(|(idx, _)| {
                        Ok(red
                            .try_to_csl_with((&csl::plutus::RedeemerTag::new_mint(), idx as u64))?)
                    }),
                _ => Some(Err(Error::Unsupported(
                    "Only spending and minting redeemers are supported".to_string(),
                ))),
            })
            .collect::<Result<_>>()?;

        Ok((datums, redeemers))
    }

    /// Collect witness scripts (validators and mintig policies)
    fn witness_scripts(
        tx_info: &TransactionInfo,
        validators: &BTreeMap<ValidatorHash, ScriptOrRef>,
        minting_policies: &BTreeMap<MintingPolicyHash, ScriptOrRef>,
    ) -> Result<Vec<csl::plutus::PlutusScript>> {
        let validator_hashes = tx_info
            .inputs
            .iter()
            .filter_map(
                |TxInInfo {
                     reference: _,
                     output,
                 }| {
                    match &output.address {
                        Address {
                            credential: Credential::Script(vh),
                            staking_credential: _,
                        } => Some(vh),
                        _ => None,
                    }
                },
            )
            .collect::<Vec<&ValidatorHash>>();

        let minting_policy_hashes = tx_info
            .mint
            .0
            .iter()
            .filter_map(|(cs, _)| match cs {
                CurrencySymbol::NativeToken(mph) => Some(mph),
                _ => None,
            })
            .collect::<Vec<&MintingPolicyHash>>();

        Ok(validator_hashes
            .iter()
            .map(|vh| {
                validators
                    .get(vh)
                    .cloned()
                    .ok_or(Error::MissingValidatorScript((*vh).clone()))
            })
            .chain(minting_policy_hashes.iter().map(|mph| {
                minting_policies
                    .get(mph)
                    .cloned()
                    .ok_or(Error::MissingMintingPolicyScript((*mph).clone()))
            }))
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .filter_map(|script_or_ref| match script_or_ref {
                ScriptOrRef::PlutusScript(script, _) => Some(script),
                ScriptOrRef::RefScript(_, _, _) => None,
            })
            .collect())
    }

    /// Calculate script integrity hash
    fn calc_script_data_hash(
        &self,
        mut tx_builder: csl::tx_builder::TransactionBuilder,
        wit_datums: &Vec<csl::plutus::PlutusData>,
        wit_redeemers: &Vec<csl::plutus::Redeemer>,
    ) -> csl::tx_builder::TransactionBuilder {
        let mut redeemers = csl::plutus::Redeemers::new();
        wit_redeemers.iter().for_each(|red| redeemers.add(&red));

        if !wit_datums.is_empty() || !wit_redeemers.is_empty() {
            let datums = if wit_datums.is_empty() {
                None
            } else {
                let mut ds = csl::plutus::PlutusList::new();
                wit_datums.iter().for_each(|dat| ds.add(&dat));
                Some(ds)
            };

            let mut used_langs = csl::plutus::Languages::new();
            used_langs.add(csl::plutus::Language::new_plutus_v2());

            let script_data_hash = csl::utils::hash_script_data(
                &redeemers,
                &self.cost_models.retain_language_versions(&used_langs),
                datums,
            );

            let _ = tx_builder.set_script_data_hash(&script_data_hash);
        }
        tx_builder
    }

    /// Apply execution units to redeemers
    fn apply_ex_units(
        redeemer: &csl::plutus::Redeemer,
        ex_units_map: &BTreeMap<
            (csl::plutus::RedeemerTag, csl::utils::BigNum),
            csl::plutus::ExUnits,
        >,
    ) -> Result<csl::plutus::Redeemer> {
        let key = (redeemer.tag(), redeemer.index());
        let ex_units = ex_units_map.get(&key).ok_or(Error::MissingExUnits(key))?;
        Ok(csl::plutus::Redeemer::new(
            &redeemer.tag(),
            &redeemer.index(),
            &redeemer.data(),
            &ex_units,
        ))
    }

    /// Calculate script integrity hash, balance the transaction and add change if necessary
    fn balance_transaction(
        &self,
        tx: &TxWithCtx<'_>,
        mut tx_builder: csl::tx_builder::TransactionBuilder,
        wit_datums: &Vec<csl::plutus::PlutusData>,
        wit_redeemers: &Vec<csl::plutus::Redeemer>,
    ) -> Result<csl::TransactionBody> {
        let mut redeemers = csl::plutus::Redeemers::new();
        wit_redeemers.iter().for_each(|red| redeemers.add(&red));

        tx_builder = self.calc_script_data_hash(tx_builder, wit_datums, wit_redeemers);

        let (change_addr, change_datum) = match tx.change_strategy {
            ChangeStrategy::Address(addr) => (addr.try_to_csl_with(self.network_id)?, None),
            ChangeStrategy::LastOutput => {
                let last_output = tx
                    .tx_info
                    .outputs
                    .last()
                    .ok_or(Error::MissingChangeOutput)?;

                (
                    last_output.address.try_to_csl_with(self.network_id)?,
                    last_output.datum.try_to_csl()?,
                )
            }
        };

        match change_datum {
            None => tx_builder
                .add_change_if_needed(&change_addr)
                .map_err(|source| Error::TransactionBuildError(anyhow!(source)))?,
            Some(output_datum) => tx_builder
                .add_change_if_needed_with_datum(&change_addr, &output_datum)
                .map_err(|source| Error::TransactionBuildError(anyhow!(source)))?,
        };

        tx_builder
            .build()
            .map_err(|source| Error::TransactionBuildError(anyhow!(source)))
    }

    /// Convert a TransactionInfo into a valid TransactionBody and prepare all witnesses (except
    /// wallet signatures)
    /// If the transaction context does not include execution units, we use Ogmios to calculate
    /// those
    pub async fn bake_balanced_tx(
        &self,
        submitter: &impl Submitter,
        tx: TxWithCtx<'_>,
    ) -> Result<csl::Transaction> {
        let (datums, redeemers) = TxBakery::extract_witnesses(&tx.tx_info)?;
        let plutus_scripts =
            TxBakery::witness_scripts(&tx.tx_info, &tx.validators, &tx.minting_policies)?;

        let aux_data: Result<Option<csl::metadata::AuxiliaryData>> = tx
            .metadata
            .clone()
            .map(|metadata| {
                let mut aux_data = csl::metadata::AuxiliaryData::new();
                aux_data.set_metadata(&metadata.try_into()?);
                Ok(aux_data)
            })
            .transpose();
        let aux_data = aux_data?;

        let (tx_builder, ex_units) = match &tx.ex_units_map {
            Some(ex_units_map) => (self.mk_tx_builder(&tx)?, (*ex_units_map).clone()),
            None => {
                let tx_builder = self.mk_tx_builder(&tx)?;

                let ex_units = submitter
                    .evaluate_transaction(&tx_builder, &plutus_scripts, &redeemers)
                    .await?;

                (
                    self.mk_tx_builder(&tx.clone().with_ex_units(&ex_units))?,
                    ex_units,
                )
            }
        };

        let redeemers_w_ex_u = redeemers
            .iter()
            .map(|r| TxBakery::apply_ex_units(r, &ex_units))
            .collect::<Result<Vec<_>>>()?;

        let tx_body = self.balance_transaction(&tx, tx_builder, &datums, &redeemers_w_ex_u)?;

        let mut witness_set = TransactionWitnessSet::new();
        let mut script_witnesses = PlutusScripts::new();

        plutus_scripts
            .iter()
            .for_each(|script| script_witnesses.add(script));

        let mut redeemer_witnesses = Redeemers::new();

        redeemers_w_ex_u
            .iter()
            .for_each(|redeemer| redeemer_witnesses.add(redeemer));

        witness_set.set_plutus_scripts(&script_witnesses);
        witness_set.set_redeemers(&redeemer_witnesses);

        Ok(csl::Transaction::new(&tx_body, &witness_set, aux_data))
    }

    /// Convert a TransactionInfo into a valid signed Transaction
    pub async fn bake_signed_tx(
        &self,
        submitter: &impl Submitter,
        wallet: &impl Wallet,
        tx: TxWithCtx<'_>,
    ) -> Result<csl::Transaction> {
        let tx = self.bake_balanced_tx(submitter, tx).await?;
        Ok(wallet.sign_transaction(&tx))
    }

    /// Convert a TransactionInfo into a valid signed Transaction and submit it to the chain
    /// (Not waiting for confirmation)
    pub async fn bake_and_deliver(
        &self,
        submitter: &impl Submitter,
        wallet: &impl Wallet,
        tx: TxWithCtx<'_>,
    ) -> Result<TransactionHash> {
        let tx = self.bake_signed_tx(submitter, wallet, tx).await?;

        Ok(submitter.submit_transaction(&tx).await?)
    }
}
