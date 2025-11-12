use num_bigint::BigInt;
use pallas_primitives::{self as primitives, babbage, conway};
use pallas_traverse::ComputeHash;

use plutus_ledger_api::{
    csl::{
        csl_to_pla::FromCSL,
        lib::{self as csl},
        pla_to_csl::TryFromPLA,
    },
    plutus_data::PlutusData,
    v3::{
        address::{Credential, StakingCredential},
        assoc_map::AssocMap,
        crypto::{LedgerBytes, PaymentPubKeyHash},
        datum::{Datum, DatumHash, OutputDatum},
        interval::Interval,
        redeemer::Redeemer,
        script::{MintingPolicyHash, ScriptHash},
        transaction::{DCert, POSIXTimeRange, ScriptPurpose, TransactionHash, TransactionInput},
        value::{CurrencySymbol, TokenName, Value},
    },
};
use serde::{Deserialize, Serialize};

use crate::from_pallas::{FromPallas, FromPallasError, IntoPallas};
pub use crate::types::{
    v1::MultiEraAddress,
    v2::{Script, TransactionOutput},
};

// TODO: governance
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

impl FromPallas<(conway::TransactionBody, conway::WitnessSet)> for Transaction {
    fn from_pallas(
        (tx, witness_set): (conway::TransactionBody, conway::WitnessSet),
    ) -> Result<Self, FromPallasError> {
        let tx_id = tx.compute_hash();

        let inputs = tx
            .inputs
            .to_vec()
            .into_iter()
            .map(TransactionInput::from_pallas)
            .collect::<Result<Vec<_>, _>>()?;

        let reference_inputs = tx
            .reference_inputs
            .into_iter()
            .flat_map(|ref_inputs| {
                ref_inputs
                    .to_vec()
                    .into_iter()
                    .map(TransactionInput::from_pallas)
            })
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
            // TODO: all the fields below
            d_cert: Vec::new(),
            wdrl: withdrawals,
            valid_range: Interval::Always.into(),
            signatories: Vec::new(),
            redeemers: AssocMap::new(),
            datums: AssocMap::from(
                witness_set
                    .plutus_data
                    .into_iter()
                    .flat_map(|datums| {
                        datums.to_vec().into_iter().map(|datum| {
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

impl FromPallas<conway::TransactionInput> for TransactionInput {
    fn from_pallas(value: conway::TransactionInput) -> Result<Self, FromPallasError> {
        Ok(TransactionInput {
            transaction_id: TransactionHash(LedgerBytes(value.transaction_id.to_vec())),
            index: BigInt::from(value.index),
        })
    }
}

impl FromPallas<conway::Hash<32>> for TransactionHash {
    fn from_pallas(value: conway::Hash<32>) -> Result<Self, FromPallasError> {
        Ok(TransactionHash(LedgerBytes(value.to_vec())))
    }
}

impl FromPallas<conway::TransactionOutput> for TransactionOutput {
    fn from_pallas(value: conway::TransactionOutput) -> Result<Self, FromPallasError> {
        match value {
            conway::PseudoTransactionOutput::PostAlonzo(output) => Ok(TransactionOutput {
                address: MultiEraAddress::from_pallas(output.address)?,
                value: Value::from_pallas(output.value)?,
                datum: OutputDatum::from_pallas(output.datum_option)?,
                reference_script: output
                    .script_ref
                    .map(|x| Script::from_pallas(x.unwrap()))
                    .transpose()?,
            }),
            conway::PseudoTransactionOutput::Legacy(output) => {
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

impl<Amount>
    FromPallas<
        primitives::NonEmptyKeyValuePairs<
            primitives::Hash<28>,
            primitives::NonEmptyKeyValuePairs<conway::Bytes, Amount>,
        >,
    > for Value
where
    Amount: IntoPallas<BigInt> + Clone,
{
    fn from_pallas(
        value: conway::NonEmptyKeyValuePairs<
            conway::Hash<28>,
            conway::NonEmptyKeyValuePairs<conway::Bytes, Amount>,
        >,
    ) -> Result<Self, FromPallasError> {
        let mut val = Value::new();

        for (currency_symbol, inner_asset) in value.to_vec() {
            for (token_name, amount) in inner_asset.to_vec() {
                let currency_symbol = CurrencySymbol::from_pallas(currency_symbol)?;
                let token_name = TokenName::from_pallas(token_name)?;
                let amount = amount.into_pallas()?;

                val.insert_token_mut(currency_symbol, token_name, amount)
            }
        }

        Ok(val)
    }
}

impl FromPallas<conway::Value> for Value {
    fn from_pallas(value: conway::Value) -> Result<Self, FromPallasError> {
        match value {
            conway::Value::Coin(coin) => Ok(Value::ada_value(&BigInt::from(coin))),
            conway::Value::Multiasset(coin, multiasset) => {
                let mut val = Value::from_pallas(multiasset)?;
                val.insert_ada_mut(BigInt::from(coin));

                Ok(val)
            }
        }
    }
}

impl FromPallas<conway::Hash<32>> for CurrencySymbol {
    fn from_pallas(value: conway::Hash<32>) -> Result<Self, FromPallasError> {
        let vec = value.to_vec();
        if vec.is_empty() {
            Ok(CurrencySymbol::Ada)
        } else {
            Ok(CurrencySymbol::NativeToken(MintingPolicyHash(ScriptHash(
                LedgerBytes(vec),
            ))))
        }
    }
}

impl FromPallas<primitives::PositiveCoin> for BigInt {
    fn from_pallas(value: primitives::PositiveCoin) -> Result<Self, FromPallasError> {
        Ok(BigInt::from(<u64>::from(value)))
    }
}

impl FromPallas<primitives::NonZeroInt> for BigInt {
    fn from_pallas(value: primitives::NonZeroInt) -> Result<Self, FromPallasError> {
        Ok(BigInt::from(<i64>::from(value)))
    }
}

impl FromPallas<conway::PseudoScript<babbage::NativeScript>> for Script {
    fn from_pallas(
        value: conway::PseudoScript<babbage::NativeScript>,
    ) -> Result<Self, FromPallasError> {
        Ok(match value {
            conway::PseudoScript::NativeScript(native_script) => {
                Script::NativeScript(csl::NativeScript::from_pallas(native_script)?)
            }
            conway::PseudoScript::PlutusV1Script(plutus_script) => {
                Script::PlutusScript(csl::PlutusScript::new(plutus_script.0.to_vec()))
            }
            conway::PseudoScript::PlutusV2Script(plutus_script) => {
                Script::PlutusScript(csl::PlutusScript::new_v2(plutus_script.0.to_vec()))
            }
            conway::PseudoScript::PlutusV3Script(plutus_script) => {
                Script::PlutusScript(csl::PlutusScript::new_v2(plutus_script.0.to_vec()))
            }
        })
    }
}
