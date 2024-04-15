use super::union_b_tree_maps_with;
use cardano_serialization_lib as csl;
use num_bigint::{BigInt, ParseBigIntError};
use plutus_ledger_api::{
    plutus_data::PlutusData,
    v2::{
        address::{
            Address, CertificateIndex, ChainPointer, Credential, Slot, StakingCredential,
            TransactionIndex,
        },
        crypto::{Ed25519PubKeyHash, LedgerBytes},
        datum::{Datum, DatumHash, OutputDatum},
        script::{MintingPolicyHash, ScriptHash, ValidatorHash},
        transaction::{TransactionHash, TransactionInput, TransactionOutput},
        value::{CurrencySymbol, TokenName, Value},
    },
};
use std::{collections::BTreeMap, ops::Neg, str::FromStr};

#[derive(Debug, Clone, thiserror::Error)]
pub enum TryFromCSLError {
    #[error("Unable to parse BigInt: {0}")]
    InvalidBigInt(ParseBigIntError),
    #[error("Unable to represent CSL value in PLA: {0}")]
    ImpossibleConversion(String),
}

/// Convert a cardano-serialization-lib type to its plutus-ledger-api counterpart
pub trait FromCSL<T> {
    fn from_csl(value: &T) -> Self
    where
        Self: Sized;
}

pub trait ToPLA<T> {
    fn to_pla(&self) -> T;
}

impl<T, U> ToPLA<U> for T
where
    U: FromCSL<T>,
{
    fn to_pla(&self) -> U {
        FromCSL::from_csl(self)
    }
}

/// Convert a cardano-serialization-lib type to its plutus-ledger-api counterpart
pub trait TryFromCSL<T> {
    fn try_from_csl(value: &T) -> Result<Self, TryFromCSLError>
    where
        Self: Sized;
}

pub trait TryToPLA<T> {
    fn try_to_pla(&self) -> Result<T, TryFromCSLError>;
}

impl<T, U> TryToPLA<U> for T
where
    U: TryFromCSL<T>,
{
    fn try_to_pla(&self) -> Result<U, TryFromCSLError> {
        TryFromCSL::try_from_csl(self)
    }
}

impl FromCSL<csl::AssetName> for TokenName {
    fn from_csl(value: &csl::AssetName) -> Self {
        TokenName(LedgerBytes(value.name()))
    }
}

impl FromCSL<csl::PolicyID> for MintingPolicyHash {
    fn from_csl(value: &csl::PolicyID) -> Self {
        MintingPolicyHash(ScriptHash(LedgerBytes(value.to_bytes())))
    }
}

impl FromCSL<csl::utils::Value> for Value {
    fn from_csl(value: &csl::utils::Value) -> Self {
        let lovelaces = BigInt::from_csl(&value.coin());
        let mut pla_value = Value::ada_value(&lovelaces);
        if let Some(multi_asset) = value.multiasset() {
            pla_value = &pla_value + &Value::from_csl(&multi_asset)
        }
        pla_value
    }
}

impl FromCSL<csl::utils::BigNum> for BigInt {
    fn from_csl(value: &csl::utils::BigNum) -> Self {
        let x: u64 = From::from(*value);
        BigInt::from(x)
    }
}

impl FromCSL<csl::Assets> for BTreeMap<TokenName, BigInt> {
    fn from_csl(value: &csl::Assets) -> Self {
        let keys = value.keys();
        (0..keys.len()).fold(BTreeMap::new(), |mut acc, idx| {
            let asset_name = keys.get(idx);
            if let Some(quantity) = value.get(&asset_name) {
                acc.insert(
                    TokenName::from_csl(&asset_name),
                    BigInt::from_csl(&quantity),
                );
            }
            acc
        })
    }
}

impl FromCSL<csl::MultiAsset> for Value {
    fn from_csl(value: &csl::MultiAsset) -> Self {
        let keys = value.keys();
        Value((0..keys.len()).fold(BTreeMap::new(), |mut acc, idx| {
            let script_hash = keys.get(idx);
            if let Some(assets) = value.get(&script_hash) {
                let assets = BTreeMap::from_csl(&assets);
                acc.insert(
                    CurrencySymbol::NativeToken(MintingPolicyHash::from_csl(&script_hash)),
                    assets,
                );
            }
            acc
        }))
    }
}

impl FromCSL<u32> for BigInt {
    fn from_csl(value: &u32) -> Self {
        BigInt::from(*value)
    }
}

impl TryFromCSL<csl::utils::BigInt> for BigInt {
    fn try_from_csl(value: &csl::utils::BigInt) -> Result<Self, TryFromCSLError> {
        BigInt::from_str(&value.to_str()).map_err(TryFromCSLError::InvalidBigInt)
    }
}

impl FromCSL<csl::utils::Int> for BigInt {
    fn from_csl(value: &csl::utils::Int) -> Self {
        if value.is_positive() {
            BigInt::from_csl(&value.as_positive().unwrap())
        } else {
            BigInt::from_csl(&value.as_negative().unwrap()).neg()
        }
    }
}

impl FromCSL<csl::crypto::TransactionHash> for TransactionHash {
    fn from_csl(value: &csl::crypto::TransactionHash) -> Self {
        TransactionHash(LedgerBytes(value.to_bytes()))
    }
}

impl FromCSL<csl::TransactionInput> for TransactionInput {
    fn from_csl(value: &csl::TransactionInput) -> Self {
        TransactionInput {
            transaction_id: TransactionHash::from_csl(&value.transaction_id()),
            index: BigInt::from_csl(&value.index()),
        }
    }
}

impl FromCSL<csl::TransactionInputs> for Vec<TransactionInput> {
    fn from_csl(value: &csl::TransactionInputs) -> Self {
        (0..value.len())
            .map(|idx| TransactionInput::from_csl(&value.get(idx)))
            .collect()
    }
}

impl FromCSL<csl::crypto::Ed25519KeyHash> for Ed25519PubKeyHash {
    fn from_csl(value: &csl::crypto::Ed25519KeyHash) -> Self {
        Ed25519PubKeyHash(LedgerBytes(value.to_bytes()))
    }
}

impl FromCSL<csl::crypto::ScriptHash> for ScriptHash {
    fn from_csl(value: &csl::crypto::ScriptHash) -> Self {
        ScriptHash(LedgerBytes(value.to_bytes()))
    }
}

impl FromCSL<csl::crypto::ScriptHash> for ValidatorHash {
    fn from_csl(value: &csl::crypto::ScriptHash) -> Self {
        ValidatorHash(ScriptHash::from_csl(value))
    }
}

impl FromCSL<csl::address::StakeCredential> for StakingCredential {
    fn from_csl(value: &csl::address::StakeCredential) -> Self {
        StakingCredential::Hash(Credential::from_csl(value))
    }
}

impl FromCSL<csl::address::StakeCredential> for Credential {
    fn from_csl(value: &csl::address::StakeCredential) -> Self {
        match value.kind() {
            csl::address::StakeCredKind::Key => {
                Credential::PubKey(Ed25519PubKeyHash::from_csl(&value.to_keyhash().unwrap()))
            }
            csl::address::StakeCredKind::Script => {
                Credential::Script(ValidatorHash::from_csl(&value.to_scripthash().unwrap()))
            }
        }
    }
}

impl FromCSL<csl::utils::BigNum> for Slot {
    fn from_csl(value: &csl::utils::BigNum) -> Self {
        Slot(BigInt::from_csl(value))
    }
}

impl FromCSL<csl::utils::BigNum> for TransactionIndex {
    fn from_csl(value: &csl::utils::BigNum) -> Self {
        TransactionIndex(BigInt::from_csl(value))
    }
}

impl FromCSL<csl::utils::BigNum> for CertificateIndex {
    fn from_csl(value: &csl::utils::BigNum) -> Self {
        CertificateIndex(BigInt::from_csl(value))
    }
}

impl FromCSL<csl::address::Pointer> for ChainPointer {
    fn from_csl(value: &csl::address::Pointer) -> Self {
        ChainPointer {
            slot_number: Slot::from_csl(&value.slot_bignum()),
            transaction_index: TransactionIndex::from_csl(&value.tx_index_bignum()),
            certificate_index: CertificateIndex::from_csl(&value.cert_index_bignum()),
        }
    }
}

impl FromCSL<csl::address::Pointer> for StakingCredential {
    fn from_csl(value: &csl::address::Pointer) -> Self {
        StakingCredential::Pointer(ChainPointer::from_csl(value))
    }
}
impl TryFromCSL<csl::address::Address> for Address {
    fn try_from_csl(value: &csl::address::Address) -> Result<Self, TryFromCSLError> {
        if let Some(addr) = csl::address::BaseAddress::from_address(value) {
            Ok(Address {
                credential: Credential::from_csl(&addr.payment_cred()),
                staking_credential: Some(StakingCredential::from_csl(&addr.stake_cred())),
            })
        } else if let Some(addr) = csl::address::PointerAddress::from_address(value) {
            Ok(Address {
                credential: Credential::from_csl(&addr.payment_cred()),
                staking_credential: Some(StakingCredential::from_csl(&addr.stake_pointer())),
            })
        } else if let Some(addr) = csl::address::EnterpriseAddress::from_address(value) {
            Ok(Address {
                credential: Credential::from_csl(&addr.payment_cred()),
                staking_credential: None,
            })
        } else {
            Err(TryFromCSLError::ImpossibleConversion(format!(
                "Unable to represent address {:?}",
                value
            )))
        }
    }
}

impl TryFromCSL<csl::plutus::PlutusData> for PlutusData {
    fn try_from_csl(value: &csl::plutus::PlutusData) -> Result<Self, TryFromCSLError> {
        Ok(match value.kind() {
            csl::plutus::PlutusDataKind::ConstrPlutusData => {
                let constr_data = value.as_constr_plutus_data().unwrap();
                let tag = BigInt::from_csl(&constr_data.alternative());
                let args = constr_data.data().try_to_pla()?;
                PlutusData::Constr(tag, args)
            }
            csl::plutus::PlutusDataKind::Map => {
                PlutusData::Map(value.as_map().unwrap().try_to_pla()?)
            }
            csl::plutus::PlutusDataKind::List => {
                PlutusData::List(value.as_list().unwrap().try_to_pla()?)
            }
            csl::plutus::PlutusDataKind::Integer => {
                PlutusData::Integer(value.as_integer().unwrap().try_to_pla()?)
            }
            csl::plutus::PlutusDataKind::Bytes => PlutusData::Bytes(value.as_bytes().unwrap()),
        })
    }
}

impl TryFromCSL<csl::plutus::PlutusList> for Vec<PlutusData> {
    fn try_from_csl(value: &csl::plutus::PlutusList) -> Result<Self, TryFromCSLError> {
        (0..value.len())
            .map(|idx| value.get(idx).try_to_pla())
            .collect()
    }
}

impl TryFromCSL<csl::plutus::PlutusMap> for Vec<(PlutusData, PlutusData)> {
    fn try_from_csl(c_map: &csl::plutus::PlutusMap) -> Result<Self, TryFromCSLError> {
        let keys = c_map.keys();
        (0..keys.len())
            .map(|idx| {
                let key = keys.get(idx);
                let value = c_map.get(&key).unwrap();
                Ok((key.try_to_pla()?, value.try_to_pla()?))
            })
            .collect()
    }
}

impl FromCSL<csl::crypto::DataHash> for DatumHash {
    fn from_csl(value: &csl::crypto::DataHash) -> Self {
        DatumHash(LedgerBytes(value.to_bytes()))
    }
}

impl TryFromCSL<csl::OutputDatum> for OutputDatum {
    fn try_from_csl(value: &csl::OutputDatum) -> Result<Self, TryFromCSLError> {
        Ok(if let Some(d) = value.data() {
            OutputDatum::InlineDatum(Datum(d.try_to_pla()?))
        } else if let Some(h) = value.data_hash() {
            OutputDatum::DatumHash(DatumHash::from_csl(&h))
        } else {
            OutputDatum::None
        })
    }
}

impl TryFromCSL<csl::TransactionOutput> for TransactionOutput {
    fn try_from_csl(value: &csl::TransactionOutput) -> Result<Self, TryFromCSLError> {
        Ok(TransactionOutput {
            address: value.address().try_to_pla()?,
            datum: if value.has_data_hash() {
                OutputDatum::DatumHash(DatumHash::from_csl(&value.data_hash().unwrap()))
            } else if value.has_plutus_data() {
                OutputDatum::InlineDatum(Datum(value.plutus_data().unwrap().try_to_pla()?))
            } else {
                OutputDatum::None
            },
            reference_script: if value.has_script_ref() {
                let script_ref = value.script_ref().unwrap();
                let script_hash = if script_ref.is_native_script() {
                    script_ref.native_script().unwrap().hash()
                } else {
                    script_ref.plutus_script().unwrap().hash()
                };
                Some(ScriptHash::from_csl(&script_hash))
            } else {
                None
            },
            value: Value::from_csl(&value.amount()),
        })
    }
}

impl TryFromCSL<csl::TransactionOutputs> for Vec<TransactionOutput> {
    fn try_from_csl(value: &csl::TransactionOutputs) -> Result<Self, TryFromCSLError> {
        (0..value.len())
            .map(|idx| TransactionOutput::try_from_csl(&value.get(idx)))
            .collect()
    }
}

impl FromCSL<csl::MintAssets> for BTreeMap<TokenName, BigInt> {
    fn from_csl(m_ass: &csl::MintAssets) -> Self {
        let keys = m_ass.keys();
        (0..keys.len())
            .map(|idx| {
                let key = keys.get(idx);
                let value = m_ass.get(&key).unwrap();
                (TokenName::from_csl(&key), BigInt::from_csl(&value))
            })
            .collect()
    }
}

impl FromCSL<csl::MintsAssets> for BTreeMap<TokenName, BigInt> {
    fn from_csl(value: &csl::MintsAssets) -> Self {
        let mut m_ass_vec = Vec::new();

        // This is so stupid. `MintsAssets` doesn't have a `len` method for some reason.
        for idx in 0.. {
            if let Some(m_ass) = value.get(idx) {
                m_ass_vec.push(m_ass);
            } else {
                break;
            }
        }

        m_ass_vec.into_iter().fold(BTreeMap::new(), |acc, m| {
            let ass = BTreeMap::from_csl(&m);
            union_b_tree_maps_with(|l, r| l + r, [&acc, &ass])
        })
    }
}

impl FromCSL<csl::Mint> for Value {
    fn from_csl(mint: &csl::Mint) -> Self {
        let keys = mint.keys();
        Value(
            (0..keys.len())
                .map(|idx| {
                    let sh = keys.get(idx);
                    let ass = mint.get_all(&sh).unwrap_or(csl::MintsAssets::new());
                    (
                        CurrencySymbol::NativeToken(MintingPolicyHash::from_csl(&sh)),
                        BTreeMap::from_csl(&ass),
                    )
                })
                .collect::<BTreeMap<CurrencySymbol, BTreeMap<TokenName, BigInt>>>(),
        )
    }
}

impl FromCSL<csl::RequiredSigners> for Vec<Ed25519PubKeyHash> {
    fn from_csl(value: &csl::RequiredSigners) -> Self {
        (0..value.len())
            .map(|idx| Ed25519PubKeyHash::from_csl(&value.get(idx)))
            .collect()
    }
}

impl FromCSL<csl::NativeScripts> for Vec<csl::NativeScript> {
    fn from_csl(value: &csl::NativeScripts) -> Self {
        (0..value.len()).map(|idx| value.get(idx)).collect()
    }
}

impl FromCSL<csl::plutus::PlutusScripts> for Vec<csl::plutus::PlutusScript> {
    fn from_csl(value: &csl::plutus::PlutusScripts) -> Self {
        (0..value.len()).map(|idx| value.get(idx)).collect()
    }
}
