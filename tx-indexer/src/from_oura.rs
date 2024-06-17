use ::oura::model::{MintRecord, OutputAssetRecord};
use cardano_serialization_lib as csl;
use data_encoding::HEXLOWER;
use num_bigint::BigInt;
use num_traits::FromPrimitive;
use plutus_ledger_api::v2::{
    address::Address,
    crypto::LedgerBytes,
    datum::{Datum, DatumHash},
    script::{MintingPolicyHash, ScriptHash},
    transaction::TransactionHash,
    value::{CurrencySymbol, TokenName, Value},
};
use std::fmt::Debug;
use tx_bakery::utils::csl_to_pla::TryToPLA;

#[derive(thiserror::Error, Debug)]
pub enum OuraParseError {
    #[error("Unable to parse bigint from u64: {0}")]
    BigIntFromU64(u64),

    #[error("Unable to parse bigint from i64: {0}")]
    BigIntFromI64(i64),

    #[error("Unable to parse hash from string: {0}")]
    HashFromString(String),

    #[error("Unable to parse Address from bech32 string: {0}")]
    AddressFromString(String),

    #[error("Unable to parse Datum from JSON: {0}")]
    DataFromJSON(serde_json::Value),

    #[error("Unable to convert current time: {0}")]
    TimeConversionError(tx_bakery::error::Error),
}

/// Convert an Oura transaction record type to its plutus-ledger-api counterpart
pub trait FromOura<T> {
    fn from_oura(value: T) -> Result<Self, OuraParseError>
    where
        Self: Sized;
}

impl FromOura<u64> for BigInt {
    fn from_oura(value: u64) -> Result<Self, OuraParseError> {
        BigInt::from_u64(value).ok_or(OuraParseError::BigIntFromU64(value))
    }
}

impl FromOura<i64> for BigInt {
    fn from_oura(value: i64) -> Result<Self, OuraParseError> {
        BigInt::from_i64(value).ok_or(OuraParseError::BigIntFromI64(value))
    }
}

impl FromOura<String> for LedgerBytes {
    fn from_oura(value: String) -> Result<Self, OuraParseError> {
        Ok(LedgerBytes(
            HEXLOWER
                .decode(&value.clone().into_bytes()[..])
                .map_err(|_| OuraParseError::HashFromString(value))?,
        ))
    }
}

impl FromOura<String> for TransactionHash {
    fn from_oura(value: String) -> Result<Self, OuraParseError> {
        Ok(TransactionHash(LedgerBytes::from_oura(value)?))
    }
}

impl FromOura<String> for DatumHash {
    fn from_oura(value: String) -> Result<Self, OuraParseError> {
        Ok(DatumHash(LedgerBytes::from_oura(value)?))
    }
}

impl FromOura<String> for CurrencySymbol {
    fn from_oura(value: String) -> Result<Self, OuraParseError> {
        Ok(if value.is_empty() {
            CurrencySymbol::Ada
        } else {
            CurrencySymbol::NativeToken(MintingPolicyHash(ScriptHash(LedgerBytes::from_oura(
                value,
            )?)))
        })
    }
}

impl FromOura<String> for TokenName {
    fn from_oura(value: String) -> Result<Self, OuraParseError> {
        Ok(if value.is_empty() {
            TokenName::ada()
        } else {
            TokenName(LedgerBytes::from_oura(value)?)
        })
    }
}

impl FromOura<serde_json::Value> for Datum {
    fn from_oura(value: serde_json::Value) -> Result<Self, OuraParseError> {
        csl::plutus::encode_json_value_to_plutus_datum(
            value.clone(),
            csl::plutus::PlutusDatumSchema::DetailedSchema,
        )
        .ok()
        .and_then(|y: csl::plutus::PlutusData| y.try_to_pla().ok())
        .map(Datum)
        .ok_or(OuraParseError::DataFromJSON(value))
    }
}

impl FromOura<String> for Address {
    fn from_oura(value: String) -> Result<Self, OuraParseError> {
        csl::address::Address::from_bech32(&value)
            .ok()
            .and_then(|x| x.try_to_pla().ok())
            .ok_or(OuraParseError::AddressFromString(value))
    }
}

impl FromOura<Vec<OutputAssetRecord>> for Value {
    fn from_oura(value: Vec<OutputAssetRecord>) -> Result<Self, OuraParseError> {
        value.iter().try_fold(Value::new(), |acc, x| {
            let amt = BigInt::from_oura(x.amount)?;
            Ok(acc.insert_token(
                &CurrencySymbol::from_oura(x.policy.clone())?,
                &TokenName::from_oura(x.asset.clone())?,
                &amt,
            ))
        })
    }
}

impl FromOura<Vec<MintRecord>> for Value {
    fn from_oura(value: Vec<MintRecord>) -> Result<Self, OuraParseError> {
        value.iter().try_fold(Value::new(), |acc, x| {
            let amt = BigInt::from_oura(x.quantity)?;
            Ok(acc.insert_token(
                &CurrencySymbol::from_oura(x.policy.clone())?,
                &TokenName::from_oura(x.asset.clone())?,
                &amt,
            ))
        })
    }
}
