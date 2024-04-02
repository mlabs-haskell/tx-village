use lbf_tx_bakery_tests_plutus_api::demo::plutus::{EqDatum, EqRedeemer, RefInputRedeemer};
use num_bigint::BigInt;
use plutus_ledger_api::plutus_data::IsPlutusData;
use plutus_ledger_api::v2::address::{Address, Credential};
use plutus_ledger_api::v2::crypto::LedgerBytes;
use plutus_ledger_api::v2::datum::{Datum, OutputDatum};
use plutus_ledger_api::v2::redeemer::Redeemer;
use plutus_ledger_api::v2::script::{MintingPolicyHash, ValidatorHash};
use plutus_ledger_api::v2::transaction::{
    TransactionHash, TransactionInfo, TransactionInput, TransactionOutput, TxInInfo,
};
use plutus_ledger_api::v2::value::{AssetClass, CurrencySymbol, TokenName, Value};
use std::collections::BTreeMap;
use tx_bakery::chain_query::{ChainQuery, FullTransactionOutput};
use tx_bakery::error::Result;
use tx_bakery::metadata::{Metadata, TransactionMetadata};
use tx_bakery::submitter::Submitter;
use tx_bakery::tx_info_builder::TxScaffold;
use tx_bakery::utils::script::ScriptOrRef;
use tx_bakery::wallet::Wallet;
use tx_bakery::{ChangeStrategy, CollateralStrategy, TxBakery, TxWithCtx};

mod csl;

/// Transaction that stores a EqDatum value at the Eq Validator.
mod lock_eq_datum {
    use tx_bakery::ChangeStrategy;

    use super::*;

    pub fn mk_tx_info(
        validator_addr: &Address,
        eq_datum: &EqDatum,
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
    ) -> TransactionInfo {
        let datum = Datum(eq_datum.to_plutus_data());
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        TxScaffold::new()
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            .add_output(TransactionOutput {
                address: validator_addr.clone(),
                value: Value::new(),
                datum: OutputDatum::InlineDatum(datum),
                reference_script: None,
            })
            .build()
    }

    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        eq_validator: &(ValidatorHash, ScriptOrRef),
        example_eq_datum: &EqDatum,
    ) -> Result<TransactionHash> {
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        let eq_validator_addr = Address {
            credential: Credential::Script(eq_validator.0.clone()),
            staking_credential: None,
        };

        let tx_info = mk_tx_info(&eq_validator_addr, example_eq_datum, &utxos);

        let validators = BTreeMap::new();
        let minting_policies = BTreeMap::new();

        let tx_bakery = TxBakery::init(chain_query).await?;
        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());

        let tx = TxWithCtx::new(
            &tx_info,
            &validators,
            &minting_policies,
            &CollateralStrategy::None,
            &change_strategy,
        );

        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}

/// Make a transaction that releases UTxO stored at the `EqValidator` in one of the cases below:
/// - redeemer is `IsEqual` and the supplied plutus data is equal to the one locked as datum
/// - redeemer is `IsNotEqual` and the supplied plutus data is not equal to the one locked as datum
mod claim_eq_datum {

    use super::*;

    pub fn mk_tx_info(
        own_addr: &Address,
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
        tx_input: (&TransactionInput, &FullTransactionOutput),
        eq_redeemer: &EqRedeemer,
    ) -> TransactionInfo {
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");
        let redeemer = Redeemer(eq_redeemer.to_plutus_data());

        TxScaffold::new()
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            .add_script_input(tx_input.0.clone(), tx_input.1.into(), None, redeemer)
            .add_output(TransactionOutput {
                address: own_addr.clone(),
                value: Value::new(),
                datum: OutputDatum::None,
                reference_script: None,
            })
            .build()
    }

    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        eq_validator: &(ValidatorHash, ScriptOrRef),
        eq_redeemer: &EqRedeemer,
        datum: &EqDatum,
    ) -> Result<TransactionHash> {
        let eq_validator_addr = Address {
            credential: Credential::Script(eq_validator.0.clone()),
            staking_credential: None,
        };

        let eq_validator_utxos = chain_query.query_utxos_by_addr(&eq_validator_addr).await?;
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        let tx_in = eq_validator_utxos
            .iter()
            .find(|(_, tx_out)| {
                if let OutputDatum::InlineDatum(Datum(inline_datum)) = &tx_out.datum {
                    EqDatum::from_plutus_data(&inline_datum).unwrap() == *datum
                } else {
                    false
                }
            })
            .expect("Utxo with inline datum not found");

        let tx_info = mk_tx_info(&wallet.get_change_addr(), &utxos, tx_in, &eq_redeemer);

        let validators = BTreeMap::from([eq_validator.clone()]);
        let minting_policies = BTreeMap::new();

        let collateral_utxo = utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let collateral = CollateralStrategy::Explicit(TxInInfo {
            reference: collateral_utxo.0.clone(),
            output: collateral_utxo.1.into(),
        });

        let tx_bakery = TxBakery::init(chain_query).await?;

        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());
        let tx = TxWithCtx::new(
            &tx_info,
            &validators,
            &minting_policies,
            &collateral,
            &change_strategy,
        );

        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}

/// Mint some tokens using a "secret" as redeemer
mod mint_with_secret {
    use super::*;

    /// Mint some tokens
    pub fn mk_tx_info(
        change_addr: &Address,
        cur_sym: &CurrencySymbol,
        token_name: &TokenName,
        secret: u32,
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
    ) -> TransactionInfo {
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let mint_asset = AssetClass {
            currency_symbol: cur_sym.clone(),
            token_name: token_name.clone(),
        };

        TxScaffold::new()
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            .add_mint(
                mint_asset,
                1,
                Redeemer(BigInt::from(secret).to_plutus_data()),
            )
            .add_output(TransactionOutput {
                address: change_addr.clone(),
                value: Value::new().insert_token(cur_sym, token_name, &BigInt::from(1)),
                datum: OutputDatum::None,
                reference_script: None,
            })
            .build()
    }
    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        minting_policy: &(MintingPolicyHash, ScriptOrRef),
    ) -> Result<TransactionHash> {
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        let cur_sym = CurrencySymbol::NativeToken(minting_policy.0.clone());
        let token_name = TokenName(LedgerBytes(vec![1, 2]));

        let tx_info = mk_tx_info(
            &wallet.get_change_addr(),
            &cur_sym,
            &token_name,
            1234,
            &utxos,
        );

        let validators = BTreeMap::new();
        let minting_policies = BTreeMap::from([minting_policy.clone()]);

        let collateral_utxo = utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let collateral = CollateralStrategy::Explicit(TxInInfo {
            reference: collateral_utxo.0.clone(),
            output: collateral_utxo.1.into(),
        });

        let tx_bakery = TxBakery::init(chain_query).await?;

        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());
        let tx = TxWithCtx::new(
            &tx_info,
            &validators,
            &minting_policies,
            &collateral,
            &change_strategy,
        );

        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}

/// Burn some tokens using a "secret" as redeemer
mod burn_with_secret {
    use super::*;

    pub fn mk_tx_info(
        cur_sym: &CurrencySymbol,
        token_name: &TokenName,
        tx_input: (&TransactionInput, &FullTransactionOutput),
        secret: u32,
    ) -> TransactionInfo {
        let mint_asset = AssetClass {
            currency_symbol: cur_sym.clone(),
            token_name: token_name.clone(),
        };

        TxScaffold::new()
            .add_pub_key_input(tx_input.0.clone(), tx_input.1.into())
            .add_mint(
                mint_asset,
                -1,
                Redeemer(BigInt::from(secret).to_plutus_data()),
            )
            .build()
    }

    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        minting_policy: &(MintingPolicyHash, ScriptOrRef),
    ) -> Result<TransactionHash> {
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        let cur_sym = CurrencySymbol::NativeToken(minting_policy.0.clone());
        let token_name = TokenName(LedgerBytes(vec![1, 2]));

        let tx_in = utxos
            .iter()
            .find(|(_, tx_out)| {
                tx_out.value.get_token_amount(&cur_sym, &token_name) >= BigInt::from(1)
            })
            .expect("Utxo with minted token not found");

        let tx_info = mk_tx_info(&cur_sym, &token_name, tx_in, 1234);

        let validators = BTreeMap::new();
        let minting_policies = BTreeMap::from([minting_policy.clone()]);

        let collateral_utxo = utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let collateral = CollateralStrategy::Explicit(TxInInfo {
            reference: collateral_utxo.0.clone(),
            output: collateral_utxo.1.into(),
        });

        let tx_bakery = TxBakery::init(chain_query).await?;

        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());
        let tx = TxWithCtx::new(
            &tx_info,
            &validators,
            &minting_policies,
            &collateral,
            &change_strategy,
        );

        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}

/// Mint some tokens validated by a reference input
mod mint_with_ref_input {
    use super::*;

    pub fn mk_tx_info(
        change_addr: &Address,
        cur_sym: &CurrencySymbol,
        token_name: &TokenName,
        ref_tx_in: (&TransactionInput, &FullTransactionOutput),
        eq_datum: &EqDatum,
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
    ) -> TransactionInfo {
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let mint_asset = AssetClass {
            currency_symbol: cur_sym.clone(),
            token_name: token_name.clone(),
        };

        let redeemer = RefInputRedeemer {
            eq_datum: eq_datum.clone(),
            ref_input: ref_tx_in.0.clone(),
        };

        TxScaffold::new()
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            .add_reference_input(ref_tx_in.0.clone(), ref_tx_in.1.into())
            .add_mint(mint_asset, 1, Redeemer(redeemer.to_plutus_data()))
            .add_output(TransactionOutput {
                address: change_addr.clone(),
                value: Value::new().insert_token(cur_sym, token_name, &BigInt::from(1)),
                datum: OutputDatum::None,
                reference_script: None,
            })
            .build()
    }

    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        datum: &EqDatum,
        minting_policy: &(MintingPolicyHash, ScriptOrRef),
        eq_validator: &(ValidatorHash, ScriptOrRef),
    ) -> Result<TransactionHash> {
        let eq_validator_addr = Address {
            credential: Credential::Script(eq_validator.0.clone()),
            staking_credential: None,
        };

        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;
        let eq_validator_utxos = chain_query.query_utxos_by_addr(&eq_validator_addr).await?;

        let cur_sym = CurrencySymbol::NativeToken(minting_policy.0.clone());
        let token_name = TokenName(LedgerBytes(vec![1, 2]));

        let ref_tx_in = eq_validator_utxos
            .iter()
            .find(|(_, tx_out)| {
                if let OutputDatum::InlineDatum(Datum(inline_datum)) = &tx_out.datum {
                    EqDatum::from_plutus_data(&inline_datum).unwrap() == *datum
                } else {
                    false
                }
            })
            .expect("Utxo with inline datum not found");

        let tx_info = mk_tx_info(
            &wallet.get_change_addr(),
            &cur_sym,
            &token_name,
            ref_tx_in,
            datum,
            &utxos,
        );

        let validators = BTreeMap::new();
        let minting_policies = BTreeMap::from([minting_policy.clone()]);

        let collateral_utxo = utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let collateral = CollateralStrategy::Explicit(TxInInfo {
            reference: collateral_utxo.0.clone(),
            output: collateral_utxo.1.into(),
        });

        let tx_bakery = TxBakery::init(chain_query).await?;

        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());
        let tx = TxWithCtx::new(
            &tx_info,
            &validators,
            &minting_policies,
            &collateral,
            &change_strategy,
        );

        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}

/// Identical to a `mint_with_secret`, but adding a 0 Ada mint
/// This is considered valid in Plutus, but the transaction bakery must filter it out
mod zero_ada_mint {
    use super::*;

    /// Mint some tokens
    pub fn mk_tx_info(
        change_addr: &Address,
        cur_sym: &CurrencySymbol,
        token_name: &TokenName,
        secret: u32,
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
    ) -> TransactionInfo {
        let mut tx_info =
            mint_with_secret::mk_tx_info(change_addr, cur_sym, token_name, secret, own_utxos);

        tx_info.mint.0.insert(
            CurrencySymbol::Ada,
            BTreeMap::from([(TokenName::ada(), BigInt::from(0))]),
        );

        tx_info
    }
    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        minting_policy: &(MintingPolicyHash, ScriptOrRef),
    ) -> Result<TransactionHash> {
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        let cur_sym = CurrencySymbol::NativeToken(minting_policy.0.clone());
        let token_name = TokenName(LedgerBytes(vec![1, 2]));

        let tx_info = mk_tx_info(
            &wallet.get_change_addr(),
            &cur_sym,
            &token_name,
            1234,
            &utxos,
        );

        let validators = BTreeMap::new();
        let minting_policies = BTreeMap::from([minting_policy.clone()]);

        let collateral_utxo = utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let collateral = CollateralStrategy::Explicit(TxInInfo {
            reference: collateral_utxo.0.clone(),
            output: collateral_utxo.1.into(),
        });

        let tx_bakery = TxBakery::init(chain_query).await?;

        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());
        let tx = TxWithCtx::new(
            &tx_info,
            &validators,
            &minting_policies,
            &collateral,
            &change_strategy,
        );

        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}

/// Store mint script at change address
mod store_ref_script {
    use super::*;

    /// Mint some tokens
    pub fn mk_tx_info(
        change_addr: &Address,
        minting_policy_hash: &MintingPolicyHash,
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
    ) -> TransactionInfo {
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let MintingPolicyHash(script_hash) = minting_policy_hash;

        TxScaffold::new()
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            .add_output(TransactionOutput {
                address: change_addr.clone(),
                value: Value::new(),
                datum: OutputDatum::None,
                reference_script: Some(script_hash.clone()),
            })
            .build()
    }
    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        minting_policy: &(MintingPolicyHash, ScriptOrRef),
    ) -> Result<TransactionHash> {
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        let tx_info = mk_tx_info(&wallet.get_change_addr(), &minting_policy.0, &utxos);

        let validators = BTreeMap::new();
        let minting_policies = BTreeMap::from([minting_policy.clone()]);

        let collateral = CollateralStrategy::None;

        let tx_bakery = TxBakery::init(chain_query).await?;

        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());

        let tx = TxWithCtx::new(
            &tx_info,
            &validators,
            &minting_policies,
            &collateral,
            &change_strategy,
        );

        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}

/// Use mint script as reference script
mod use_ref_script {
    use super::*;

    /// Mint some tokens
    pub fn mk_tx_info(
        change_addr: &Address,
        cur_sym: &CurrencySymbol,
        token_name: &TokenName,
        secret: u32,
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
    ) -> TransactionInfo {
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let mint_asset = AssetClass {
            currency_symbol: cur_sym.clone(),
            token_name: token_name.clone(),
        };

        TxScaffold::new()
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            .add_mint(
                mint_asset,
                1,
                Redeemer(BigInt::from(secret).to_plutus_data()),
            )
            .add_output(TransactionOutput {
                address: change_addr.clone(),
                value: Value::new().insert_token(cur_sym, token_name, &BigInt::from(1)),
                datum: OutputDatum::None,
                reference_script: None,
            })
            .build()
    }
    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        minting_policy_hash: &MintingPolicyHash,
    ) -> Result<TransactionHash> {
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        let ref_script = utxos
            .iter()
            .find_map(|(tx_in, tx_out)| match tx_out.reference_script {
                Some(ref script) => Some(
                    tx_bakery::utils::script::ScriptOrRef::from_script(script.clone())
                        .unwrap()
                        .into_ref_script(tx_in.clone()),
                ),
                _ => None,
            })
            .expect("Couldn't find UTxO with reference script");

        let cur_sym = CurrencySymbol::NativeToken(minting_policy_hash.clone());
        let token_name = TokenName(LedgerBytes(vec![1, 2]));

        let tx_info = mk_tx_info(
            &wallet.get_change_addr(),
            &cur_sym,
            &token_name,
            1234,
            &utxos,
        );

        let validators = BTreeMap::new();
        let minting_policies = BTreeMap::from([(minting_policy_hash.clone(), ref_script)]);

        let collateral_utxo = utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let collateral = CollateralStrategy::Explicit(TxInInfo {
            reference: collateral_utxo.0.clone(),
            output: collateral_utxo.1.into(),
        });

        let tx_bakery = TxBakery::init(chain_query).await?;

        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());

        let tx = TxWithCtx::new(
            &tx_info,
            &validators,
            &minting_policies,
            &collateral,
            &change_strategy,
        );

        let tx = tx_bakery.bake_signed_tx(submitter, wallet, tx).await?;
        let witness_scripts = tx.witness_set().plutus_scripts().unwrap();
        assert_eq!(witness_scripts.len(), 0);

        Ok(submitter.submit_transaction(&tx).await?)
    }
}

/// Transaction with metadata
mod with_metadata {
    use super::*;

    pub fn mk_tx_info(
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
    ) -> TransactionInfo {
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        TxScaffold::new()
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            .build()
    }

    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
    ) -> Result<TransactionHash> {
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        let tx_info = mk_tx_info(&utxos);

        let validators = BTreeMap::new();
        let minting_policies = BTreeMap::new();

        let tx_bakery = TxBakery::init(chain_query).await?;

        let test_metadata =
            TransactionMetadata::from([(0, Metadata::Text("Hello World!".to_string()))]);

        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());

        let tx = TxWithCtx::new(
            &tx_info,
            &validators,
            &minting_policies,
            &CollateralStrategy::None,
            &change_strategy,
        )
        .with_metadata(&test_metadata);

        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}

/// Transaction using LastOutput change strategy
mod with_last_output_change {
    use super::*;

    pub fn mk_tx_info(
        own_addr: Address,
        own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
        eq_datum: &EqDatum,
    ) -> TransactionInfo {
        let datum = Datum(eq_datum.to_plutus_data());
        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        TxScaffold::new()
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            .add_output(TransactionOutput {
                address: own_addr,
                value: Value::new(),
                datum: OutputDatum::InlineDatum(datum),
                reference_script: None,
            })
            .build()
    }

    pub async fn build_and_submit(
        wallet: &impl Wallet,
        chain_query: &impl ChainQuery,
        submitter: &impl Submitter,
        example_eq_datum: &EqDatum,
    ) -> Result<TransactionHash> {
        let utxos = chain_query
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        let tx_info = mk_tx_info(wallet.get_change_addr(), &utxos, example_eq_datum);

        let validators = BTreeMap::new();
        let minting_policies = BTreeMap::new();

        let tx_bakery = TxBakery::init(chain_query).await?;

        let change_strategy = ChangeStrategy::LastOutput;

        let tx = TxWithCtx::new(
            &tx_info,
            &validators,
            &minting_policies,
            &CollateralStrategy::None,
            &change_strategy,
        );

        tx_bakery.bake_and_deliver(submitter, wallet, tx).await
    }
}

#[cfg(test)]
mod tests {
    use super::{
        burn_with_secret, claim_eq_datum, lock_eq_datum, mint_with_ref_input, mint_with_secret,
        store_ref_script, use_ref_script, with_last_output_change, with_metadata, zero_ada_mint,
    };
    use chrono::Local;
    use lbf_tx_bakery_tests_config_api::demo::config::Config;
    use lbf_tx_bakery_tests_plutus_api::demo::plutus::{EqDatum, EqRedeemer, Product, Record, Sum};
    use lbr_prelude::json::Json;
    use plutus_ledger_api::v2::address::{Address, Credential};
    use plutus_ledger_api::v2::crypto::LedgerBytes;
    use plutus_ledger_api::v2::datum::OutputDatum;
    use plutus_ledger_api::v2::value::{AssetClass, CurrencySymbol, TokenName};
    use serial_test::serial;
    use std::fs;
    use tx_bakery::chain_query::ChainQuery;
    use tx_bakery::error::Result;
    use tx_bakery::submitter::Submitter;
    use tx_bakery::utils::ogmios::{Ogmios, OgmiosConfigBuilder};
    use tx_bakery::utils::plutip::{Plutip, PlutipConfigBuilder};
    use tx_bakery::utils::script::ScriptOrRef;
    use tx_bakery::wallet::Wallet;
    use tx_bakery::TxBakery;

    #[tokio::test]
    #[serial]
    async fn init_tx_bakery() -> Result<()> {
        let (_plutip, ogmios) = setup_plutip_test().await;
        TxBakery::init(&ogmios).await?;
        Ok(())
    }

    #[tokio::test]
    #[serial]
    async fn time_test() -> Result<()> {
        let (_plutip, ogmios) = setup_plutip_test().await;
        let posix_time_now = Local::now().into();

        let system_start = ogmios.query_system_start().await?;
        let era_summaries = ogmios.query_era_summaries().await?;
        let slot =
            tx_bakery::time::posix_time_into_slot(&era_summaries, &system_start, posix_time_now)?;

        TxBakery::init(&ogmios).await?;

        let tip = ogmios.query_tip().await?;
        let diff = slot.abs_diff(tip.slot());
        assert!(diff < 10);
        Ok(())
    }

    #[tokio::test]
    #[serial]
    async fn test_is_eq_validator() -> Result<()> {
        let config = read_config("data/tx-bakery-test-scripts-config.json");
        let eq_validator = ScriptOrRef::from_bytes(
            config.eq_validator.0,
            tx_bakery::utils::script::PlutusVersion::V2,
        )
        .unwrap()
        .as_validator();
        let (example_eq_datum_a, _) = setup_test_data();

        let (plutip, ogmios) = setup_plutip_test().await;
        let wallet = plutip.get_own_wallet().await.unwrap();

        let tx_hash_lock_a = lock_eq_datum::build_and_submit(
            &wallet,
            &ogmios,
            &ogmios,
            &eq_validator,
            &example_eq_datum_a,
        )
        .await?;

        ogmios.await_tx_confirm(&tx_hash_lock_a).await?;

        let tx_hash_claim_a = claim_eq_datum::build_and_submit(
            &wallet,
            &ogmios,
            &ogmios,
            &eq_validator,
            &EqRedeemer::IsEqual(example_eq_datum_a.clone()),
            &example_eq_datum_a,
        )
        .await?;

        ogmios.await_tx_confirm(&tx_hash_claim_a).await?;
        Ok(())
    }

    #[tokio::test]
    #[serial]
    async fn test_mint() -> Result<()> {
        let config = read_config("data/tx-bakery-test-scripts-config.json");
        let minting_policy = ScriptOrRef::from_bytes(
            config.minting_policy.0,
            tx_bakery::utils::script::PlutusVersion::V2,
        )
        .unwrap()
        .as_minting_policy();

        let (plutip, ogmios) = setup_plutip_test().await;
        let wallet = plutip.get_own_wallet().await.unwrap();

        let tx_hash_mint =
            mint_with_secret::build_and_submit(&wallet, &ogmios, &ogmios, &minting_policy).await?;

        ogmios.await_tx_confirm(&tx_hash_mint).await?;

        let tx_hash_burn =
            burn_with_secret::build_and_submit(&wallet, &ogmios, &ogmios, &minting_policy).await?;

        ogmios.await_tx_confirm(&tx_hash_burn).await?;

        Ok(())
    }

    #[tokio::test]
    #[serial]
    async fn test_ref_input() -> Result<()> {
        let config = read_config("data/tx-bakery-test-scripts-config.json");
        let eq_validator = ScriptOrRef::from_bytes(
            config.eq_validator.0,
            tx_bakery::utils::script::PlutusVersion::V2,
        )
        .unwrap()
        .as_validator();
        let ref_input_minting_policy = ScriptOrRef::from_bytes(
            config.ref_input_minting_policy.0,
            tx_bakery::utils::script::PlutusVersion::V2,
        )
        .unwrap()
        .as_minting_policy();
        let (example_eq_datum_a, _) = setup_test_data();

        let (plutip, ogmios) = setup_plutip_test().await;
        let wallet = plutip.get_own_wallet().await.unwrap();

        let tx_hash_lock_a = lock_eq_datum::build_and_submit(
            &wallet,
            &ogmios,
            &ogmios,
            &eq_validator,
            &example_eq_datum_a,
        )
        .await?;

        ogmios.await_tx_confirm(&tx_hash_lock_a).await?;

        let tx_hash_mint = mint_with_ref_input::build_and_submit(
            &wallet,
            &ogmios,
            &ogmios,
            &example_eq_datum_a,
            &ref_input_minting_policy,
            &eq_validator,
        )
        .await?;

        ogmios.await_tx_confirm(&tx_hash_mint).await?;
        Ok(())
    }

    #[tokio::test]
    #[serial]
    async fn test_zero_ada_mint() -> Result<()> {
        let config = read_config("data/tx-bakery-test-scripts-config.json");
        let minting_policy = ScriptOrRef::from_bytes(
            config.minting_policy.0,
            tx_bakery::utils::script::PlutusVersion::V2,
        )
        .unwrap()
        .as_minting_policy();

        let (plutip, ogmios) = setup_plutip_test().await;
        let wallet = plutip.get_own_wallet().await.unwrap();

        let tx_hash_mint =
            zero_ada_mint::build_and_submit(&wallet, &ogmios, &ogmios, &minting_policy).await?;

        ogmios.await_tx_confirm(&tx_hash_mint).await?;
        Ok(())
    }

    #[tokio::test]
    #[serial]
    async fn test_ref_script() -> Result<()> {
        let config = read_config("data/tx-bakery-test-scripts-config.json");
        let minting_policy = ScriptOrRef::from_bytes(
            config.minting_policy.0,
            tx_bakery::utils::script::PlutusVersion::V2,
        )
        .unwrap()
        .as_minting_policy();

        let (plutip, ogmios) = setup_plutip_test().await;
        let wallet = plutip.get_own_wallet().await.unwrap();

        let tx_hash_store_script =
            store_ref_script::build_and_submit(&wallet, &ogmios, &ogmios, &minting_policy).await?;

        ogmios.await_tx_confirm(&tx_hash_store_script).await?;

        let tx_hash_burn =
            use_ref_script::build_and_submit(&wallet, &ogmios, &ogmios, &minting_policy.0).await?;

        ogmios.await_tx_confirm(&tx_hash_burn).await?;

        Ok(())
    }

    #[tokio::test]
    #[serial]
    async fn test_with_metadata() -> Result<()> {
        let (plutip, ogmios) = setup_plutip_test().await;
        let wallet = plutip.get_own_wallet().await.unwrap();

        let tx_hash_lock_a = with_metadata::build_and_submit(&wallet, &ogmios, &ogmios).await?;

        ogmios.await_tx_confirm(&tx_hash_lock_a).await?;

        Ok(())
    }

    #[tokio::test]
    #[serial]
    async fn test_with_last_output_change() -> Result<()> {
        let (example_eq_datum_a, _) = setup_test_data();

        let (plutip, ogmios) = setup_plutip_test().await;
        let wallet = plutip.get_own_wallet().await.unwrap();

        let tx_hash = with_last_output_change::build_and_submit(
            &wallet,
            &ogmios,
            &ogmios,
            &example_eq_datum_a,
        )
        .await?;

        ogmios.await_tx_confirm(&tx_hash).await?;

        let utxos = ogmios
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await?;

        assert_eq!(utxos.len(), 1);

        let change_has_datum = match utxos.values().nth(0).unwrap().datum {
            OutputDatum::None => false,
            _ => true,
        };

        assert!(change_has_datum);

        Ok(())
    }

    async fn setup_plutip_test() -> (Plutip, Ogmios) {
        let verbose = false;
        let plutip_config = PlutipConfigBuilder::default()
            .verbose(verbose)
            .build()
            .unwrap();
        let plutip = Plutip::start(&plutip_config).await.unwrap();

        let ogmios_config = OgmiosConfigBuilder::default()
            .node_socket(plutip.get_node_socket())
            .node_config(plutip.get_node_config_path().await)
            .network(plutip.get_network())
            .verbose(verbose)
            .build()
            .unwrap();
        let ogmios = Ogmios::start(&ogmios_config).await.unwrap();

        (plutip, ogmios)
    }

    fn setup_test_data() -> (EqDatum, EqDatum) {
        let config = read_config("data/tx-bakery-test-scripts-config.json");
        let plutarch_script = ScriptOrRef::from_bytes(
            config.eq_validator.0,
            tx_bakery::utils::script::PlutusVersion::V2,
        )
        .unwrap()
        .as_validator();

        let example_token_name = TokenName(LedgerBytes(b"example token name".to_vec()));
        let example_currency_symbol = CurrencySymbol::Ada;

        let example_asset_class = AssetClass {
            currency_symbol: example_currency_symbol,
            token_name: example_token_name,
        };
        let example_plutus_bytes = LedgerBytes(b"example bytes".to_vec());

        let example_address = Address {
            credential: Credential::Script(plutarch_script.0),
            staking_credential: None,
        };

        let example_eq_datum_a = EqDatum {
            rec: Record {
                bar: example_address.clone(),
                baz: example_plutus_bytes.clone(),
                foo: example_asset_class.clone(),
            },
            sum: Sum::Baz(example_plutus_bytes.clone()),
            prod: Product(
                example_asset_class.clone(),
                example_address.clone(),
                example_plutus_bytes.clone(),
            ),
        };

        let example_eq_datum_b = EqDatum {
            rec: Record {
                bar: example_address.clone(),
                baz: example_plutus_bytes.clone(),
                foo: example_asset_class.clone(),
            },
            sum: Sum::Foo(example_asset_class.clone()),
            prod: Product(example_asset_class, example_address, example_plutus_bytes),
        };
        (example_eq_datum_a, example_eq_datum_b)
    }

    fn read_config(path: &str) -> Config {
        let conf_str = fs::read_to_string(path).expect(&format!(
            "Couldn't read plutarch config JSON file at {}.",
            path
        ));

        Json::from_json_string(&conf_str)
            .expect(&format!("Couldn't deserialize JSON data of file {}", path))
    }
}
