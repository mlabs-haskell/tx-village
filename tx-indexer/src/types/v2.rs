use num_bigint::BigInt;
use pallas_primitives::{alonzo, babbage};
use pallas_traverse::ComputeHash;

use plutus_ledger_api::{
    csl::{
        csl_to_pla::{FromCSL, ToPLA},
        lib::{self as csl},
        pla_to_csl::TryFromPLA,
    },
    plutus_data::PlutusData,
    v2::{
        self,
        address::{Credential, StakingCredential},
        assoc_map::AssocMap,
        crypto::{Ed25519PubKeyHash, PaymentPubKeyHash},
        datum::{Datum, DatumHash, OutputDatum},
        interval::Interval,
        redeemer::Redeemer,
        transaction::{DCert, POSIXTimeRange, ScriptPurpose, TransactionHash, TransactionInput},
        value::Value,
    },
};
use serde::{Deserialize, Serialize};

pub use crate::types::v1::{BlockParseError, MultiEraAddress};
use crate::{
    from_pallas::{FromPallas, FromPallasError},
    types::v1::parse_and_match_script_purposes,
};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Transaction {
    pub inputs: Vec<TransactionInput>,
    pub reference_inputs: Vec<TransactionInput>,
    pub outputs: Vec<TransactionOutput>,
    pub fee: Value,
    pub mint: Value,
    pub d_cert: Vec<DCert>,
    pub wdrl: AssocMap<StakingCredential, BigInt>,
    pub valid_range: POSIXTimeRange,
    pub signatories: Vec<PaymentPubKeyHash>,
    pub redeemers: AssocMap<ScriptPurpose, Redeemer>,
    pub datums: AssocMap<DatumHash, Datum>,
    pub id: TransactionHash,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TransactionOutput {
    pub address: MultiEraAddress,
    pub value: Value,
    pub datum: OutputDatum,
    pub reference_script: Option<Script>,
}

impl TryFrom<TransactionOutput> for v2::transaction::TransactionOutput {
    type Error = BlockParseError;

    fn try_from(value: TransactionOutput) -> Result<Self, Self::Error> {
        Ok(v2::transaction::TransactionOutput {
            address: value.address.try_into()?,
            value: value.value,
            datum: value.datum,
            reference_script: value
                .reference_script
                .as_ref()
                .map(|script| script.hash().to_pla()),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Script {
    NativeScript(csl::NativeScript),
    PlutusScript(csl::PlutusScript),
}

impl Script {
    pub fn hash(&self) -> csl::ScriptHash {
        match self {
            Script::NativeScript(script) => script.hash(),
            Script::PlutusScript(script) => script.hash(),
        }
    }
}

impl FromPallas<(babbage::TransactionBody, babbage::WitnessSet)> for Transaction {
    fn from_pallas(
        (tx, witness_set): (babbage::TransactionBody, babbage::WitnessSet),
    ) -> Result<Self, FromPallasError> {
        let tx_id = tx.compute_hash();

        let inputs = tx
            .inputs
            .into_iter()
            .map(TransactionInput::from_pallas)
            .collect::<Result<Vec<_>, _>>()?;

        let reference_inputs = tx
            .reference_inputs
            .into_iter()
            .flat_map(|ref_inputs| ref_inputs.into_iter().map(TransactionInput::from_pallas))
            .collect::<Result<Vec<_>, _>>()?;

        let d_cert = tx
            .certificates
            .unwrap_or(Vec::new())
            .into_iter()
            .map(DCert::from_pallas)
            .collect::<Result<Vec<_>, _>>()?;

        let withdrawals = tx
            .withdrawals
            .into_iter()
            .flat_map(|withdrawals| {
                withdrawals.to_vec().into_iter().map(|(cred, amount)| {
                    Ok::<_, FromPallasError>((
                        StakingCredential::Hash(Credential::from_pallas(cred)?),
                        BigInt::from(amount),
                    ))
                })
            })
            .collect::<Result<Vec<_>, _>>()?
            .into();

        let redeemers = AssocMap::from(
            witness_set
                .redeemer
                .unwrap_or(Vec::new())
                .into_iter()
                .map(|redeemer| {
                    parse_and_match_script_purposes(
                        redeemer,
                        &inputs,
                        &tx.mint,
                        &d_cert,
                        &withdrawals,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?,
        );

        Ok(Self {
            inputs,
            reference_inputs,
            outputs: tx
                .outputs
                .into_iter()
                .map(TransactionOutput::from_pallas)
                .collect::<Result<Vec<_>, _>>()?,
            fee: Value::ada_value(&BigInt::from(tx.fee)),
            mint: tx
                .mint
                .map(Value::from_pallas)
                .unwrap_or(Ok(Value::new()))?,
            d_cert,
            wdrl: withdrawals,
            valid_range: Interval::Always.into(), // TODO
            signatories: witness_set
                .vkeywitness
                .into_iter()
                .flat_map(|vkey_wits| {
                    vkey_wits.into_iter().map(|vkey| {
                        Ok::<_, FromPallasError>(PaymentPubKeyHash(Ed25519PubKeyHash::from_pallas(
                            vkey,
                        )?))
                    })
                })
                .collect::<Result<_, _>>()?,
            redeemers,
            datums: AssocMap::from(
                witness_set
                    .plutus_data
                    .into_iter()
                    .flat_map(|datums| {
                        datums.into_iter().map(|datum| {
                            let data = PlutusData::from_pallas(&datum)?;
                            let datum_hash = DatumHash::from_csl(&csl::hash_plutus_data(
                                &csl::PlutusData::try_from_pla(&data)?,
                            ));
                            Ok::<_, FromPallasError>((datum_hash, Datum(data)))
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            id: TransactionHash::from_pallas(tx_id)?,
        })
    }
}

impl FromPallas<babbage::TransactionOutput> for TransactionOutput {
    fn from_pallas(value: babbage::TransactionOutput) -> Result<Self, FromPallasError> {
        match value {
            babbage::PseudoTransactionOutput::PostAlonzo(output) => Ok(TransactionOutput {
                address: MultiEraAddress::from_pallas(output.address)?,
                value: Value::from_pallas(output.value)?,
                datum: OutputDatum::from_pallas(output.datum_option)?,
                reference_script: output
                    .script_ref
                    .map(|x| Script::from_pallas(x.unwrap()))
                    .transpose()?,
            }),
            babbage::PseudoTransactionOutput::Legacy(output) => {
                let out = crate::types::v1::TransactionOutput::from_pallas(output)?;
                Ok(TransactionOutput {
                    address: out.address,
                    value: out.value,
                    datum: match out.datum_hash {
                        None => OutputDatum::None,
                        Some(dh) => OutputDatum::DatumHash(dh),
                    },
                    reference_script: None,
                })
            }
        }
    }
}

impl FromPallas<Option<babbage::PseudoDatumOption<babbage::PlutusData>>> for OutputDatum {
    fn from_pallas(
        value: Option<babbage::PseudoDatumOption<babbage::PlutusData>>,
    ) -> Result<Self, FromPallasError> {
        Ok(match value {
            None => OutputDatum::None,
            Some(datum_option) => match datum_option {
                babbage::PseudoDatumOption::Hash(hash) => {
                    OutputDatum::DatumHash(DatumHash::from_pallas(hash)?)
                }
                babbage::PseudoDatumOption::Data(data) => {
                    OutputDatum::InlineDatum(Datum(PlutusData::from_pallas(&data.unwrap())?))
                }
            },
        })
    }
}

impl FromPallas<babbage::PseudoScript<babbage::NativeScript>> for Script {
    fn from_pallas(
        value: babbage::PseudoScript<babbage::NativeScript>,
    ) -> Result<Self, FromPallasError> {
        Ok(match value {
            babbage::PseudoScript::NativeScript(native_script) => {
                Script::NativeScript(csl::NativeScript::from_pallas(native_script)?)
            }
            babbage::PseudoScript::PlutusV1Script(plutus_script) => {
                Script::PlutusScript(csl::PlutusScript::new(plutus_script.0.to_vec()))
            }
            babbage::PseudoScript::PlutusV2Script(plutus_script) => {
                Script::PlutusScript(csl::PlutusScript::new_v2(plutus_script.0.to_vec()))
            }
        })
    }
}

impl FromPallas<alonzo::NativeScript> for csl::NativeScript {
    fn from_pallas(value: alonzo::NativeScript) -> Result<Self, FromPallasError> {
        Ok(match value {
            alonzo::NativeScript::ScriptPubkey(hash) => csl::NativeScript::new_script_pubkey(
                &csl::ScriptPubkey::new(&csl::Ed25519KeyHash::from_bytes(hash.to_vec())?),
            ),
            alonzo::NativeScript::ScriptAll(native_scripts) => csl::NativeScript::new_script_all(
                &csl::ScriptAll::new(&csl::NativeScripts::from_pallas(native_scripts)?),
            ),
            alonzo::NativeScript::ScriptAny(native_scripts) => csl::NativeScript::new_script_any(
                &csl::ScriptAny::new(&csl::NativeScripts::from_pallas(native_scripts)?),
            ),
            alonzo::NativeScript::ScriptNOfK(n, native_scripts) => {
                csl::NativeScript::new_script_n_of_k(&csl::ScriptNOfK::new(
                    n,
                    &csl::NativeScripts::from_pallas(native_scripts)?,
                ))
            }
            alonzo::NativeScript::InvalidBefore(slot) => csl::NativeScript::new_timelock_start(
                &csl::TimelockStart::new_timelockstart(&csl::BigNum::from(slot)),
            ),
            alonzo::NativeScript::InvalidHereafter(slot) => csl::NativeScript::new_timelock_expiry(
                &csl::TimelockExpiry::new_timelockexpiry(&csl::BigNum::from(slot)),
            ),
        })
    }
}

impl FromPallas<Vec<alonzo::NativeScript>> for csl::NativeScripts {
    fn from_pallas(value: Vec<alonzo::NativeScript>) -> Result<Self, FromPallasError> {
        let mut scripts = csl::NativeScripts::new();
        for script in value {
            let script = csl::NativeScript::from_pallas(script)?;
            scripts.add(&script);
        }
        Ok(scripts)
    }
}
