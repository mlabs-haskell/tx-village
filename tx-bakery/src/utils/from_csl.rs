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
pub enum FromCSLError {
    #[error("Unable to parse BigInt: {0}")]
    InvalidBigInt(ParseBigIntError),
    #[error("Unable to represent CSL value in PLA: {0}")]
    ImpossibleConversion(String),
}

pub trait FromCSL<T> {
    fn from_csl(value: &T) -> Result<Self, FromCSLError>
    where
        Self: Sized;
}

impl FromCSL<csl::AssetName> for TokenName {
    fn from_csl(value: &csl::AssetName) -> Result<Self, FromCSLError> {
        Ok(TokenName(LedgerBytes(value.name())))
    }
}

impl FromCSL<csl::PolicyID> for MintingPolicyHash {
    fn from_csl(value: &csl::PolicyID) -> Result<Self, FromCSLError> {
        Ok(MintingPolicyHash(ScriptHash(LedgerBytes(value.to_bytes()))))
    }
}

impl FromCSL<csl::utils::Value> for Value {
    fn from_csl(value: &csl::utils::Value) -> Result<Self, FromCSLError> {
        let lovelaces = BigInt::from_csl(&value.coin())?;
        let mut pla_value = Value::ada_value(&lovelaces);
        if let Some(multi_asset) = value.multiasset() {
            pla_value = &pla_value + &Value::from_csl(&multi_asset)?
        }
        Ok(pla_value)
    }
}

impl FromCSL<csl::utils::BigNum> for BigInt {
    fn from_csl(value: &csl::utils::BigNum) -> Result<Self, FromCSLError> {
        let x: u64 = From::from(*value);
        Ok(BigInt::from(x))
    }
}

impl FromCSL<csl::Assets> for BTreeMap<TokenName, BigInt> {
    fn from_csl(value: &csl::Assets) -> Result<Self, FromCSLError> {
        let keys = value.keys();
        (0..keys.len()).try_fold(BTreeMap::new(), |mut acc, idx| {
            let asset_name = keys.get(idx);
            if let Some(quantity) = value.get(&asset_name) {
                acc.insert(
                    TokenName::from_csl(&asset_name)?,
                    BigInt::from_csl(&quantity)?,
                );
            }
            Ok(acc)
        })
    }
}

impl FromCSL<csl::MultiAsset> for Value {
    fn from_csl(value: &csl::MultiAsset) -> Result<Self, FromCSLError> {
        let keys = value.keys();
        Ok(Value((0..keys.len()).try_fold(
            BTreeMap::new(),
            |mut acc, idx| {
                let script_hash = keys.get(idx);
                if let Some(assets) = value.get(&script_hash) {
                    let assets = BTreeMap::from_csl(&assets)?;
                    acc.insert(
                        CurrencySymbol::NativeToken(MintingPolicyHash::from_csl(&script_hash)?),
                        assets,
                    );
                }
                Ok(acc)
            },
        )?))
    }
}

impl FromCSL<u32> for BigInt {
    fn from_csl(value: &u32) -> Result<Self, FromCSLError> {
        Ok(BigInt::from(*value))
    }
}

impl FromCSL<csl::utils::BigInt> for BigInt {
    fn from_csl(value: &csl::utils::BigInt) -> Result<Self, FromCSLError> {
        BigInt::from_str(&value.to_str()).map_err(FromCSLError::InvalidBigInt)
    }
}

impl FromCSL<csl::utils::Int> for BigInt {
    fn from_csl(value: &csl::utils::Int) -> Result<Self, FromCSLError> {
        Ok(if value.is_positive() {
            BigInt::from_csl(&value.as_positive().unwrap())?
        } else {
            BigInt::from_csl(&value.as_negative().unwrap())?.neg()
        })
    }
}

impl FromCSL<csl::crypto::TransactionHash> for TransactionHash {
    fn from_csl(value: &csl::crypto::TransactionHash) -> Result<Self, FromCSLError> {
        Ok(TransactionHash(LedgerBytes(value.to_bytes())))
    }
}

impl FromCSL<csl::TransactionInput> for TransactionInput {
    fn from_csl(value: &csl::TransactionInput) -> Result<Self, FromCSLError> {
        Ok(TransactionInput {
            transaction_id: TransactionHash::from_csl(&value.transaction_id())?,
            index: BigInt::from_csl(&value.index())?,
        })
    }
}

impl FromCSL<csl::TransactionInputs> for Vec<TransactionInput> {
    fn from_csl(value: &csl::TransactionInputs) -> Result<Self, FromCSLError> {
        (0..value.len())
            .map(|idx| TransactionInput::from_csl(&value.get(idx)))
            .collect()
    }
}

impl FromCSL<csl::crypto::Ed25519KeyHash> for Ed25519PubKeyHash {
    fn from_csl(value: &csl::crypto::Ed25519KeyHash) -> Result<Self, FromCSLError> {
        Ok(Ed25519PubKeyHash(LedgerBytes(value.to_bytes())))
    }
}

impl FromCSL<csl::crypto::ScriptHash> for ScriptHash {
    fn from_csl(value: &csl::crypto::ScriptHash) -> Result<Self, FromCSLError> {
        Ok(ScriptHash(LedgerBytes(value.to_bytes())))
    }
}

impl FromCSL<csl::crypto::ScriptHash> for ValidatorHash {
    fn from_csl(value: &csl::crypto::ScriptHash) -> Result<Self, FromCSLError> {
        Ok(ValidatorHash(ScriptHash::from_csl(value)?))
    }
}

impl FromCSL<csl::address::StakeCredential> for StakingCredential {
    fn from_csl(value: &csl::address::StakeCredential) -> Result<Self, FromCSLError> {
        Ok(StakingCredential::Hash(Credential::from_csl(value)?))
    }
}

impl FromCSL<csl::address::StakeCredential> for Credential {
    fn from_csl(value: &csl::address::StakeCredential) -> Result<Self, FromCSLError> {
        Ok(match value.kind() {
            csl::address::StakeCredKind::Key => {
                Credential::PubKey(Ed25519PubKeyHash::from_csl(&value.to_keyhash().unwrap())?)
            }
            csl::address::StakeCredKind::Script => {
                Credential::Script(ValidatorHash::from_csl(&value.to_scripthash().unwrap())?)
            }
        })
    }
}

impl FromCSL<csl::utils::BigNum> for Slot {
    fn from_csl(value: &csl::utils::BigNum) -> Result<Self, FromCSLError> {
        Ok(Slot(BigInt::from_csl(value)?))
    }
}

impl FromCSL<csl::utils::BigNum> for TransactionIndex {
    fn from_csl(value: &csl::utils::BigNum) -> Result<Self, FromCSLError> {
        Ok(TransactionIndex(BigInt::from_csl(value)?))
    }
}

impl FromCSL<csl::utils::BigNum> for CertificateIndex {
    fn from_csl(value: &csl::utils::BigNum) -> Result<Self, FromCSLError> {
        Ok(CertificateIndex(BigInt::from_csl(value)?))
    }
}

impl FromCSL<csl::address::Pointer> for ChainPointer {
    fn from_csl(value: &csl::address::Pointer) -> Result<Self, FromCSLError> {
        Ok(ChainPointer {
            slot_number: Slot::from_csl(&value.slot_bignum())?,
            transaction_index: TransactionIndex::from_csl(&value.tx_index_bignum())?,
            certificate_index: CertificateIndex::from_csl(&value.cert_index_bignum())?,
        })
    }
}

impl FromCSL<csl::address::Pointer> for StakingCredential {
    fn from_csl(value: &csl::address::Pointer) -> Result<Self, FromCSLError> {
        Ok(StakingCredential::Pointer(ChainPointer::from_csl(value)?))
    }
}
impl FromCSL<csl::address::Address> for Address {
    fn from_csl(value: &csl::address::Address) -> Result<Self, FromCSLError> {
        if let Some(addr) = csl::address::BaseAddress::from_address(value) {
            Ok(Address {
                credential: Credential::from_csl(&addr.payment_cred())?,
                staking_credential: Some(StakingCredential::from_csl(&addr.stake_cred())?),
            })
        } else if let Some(addr) = csl::address::PointerAddress::from_address(value) {
            Ok(Address {
                credential: Credential::from_csl(&addr.payment_cred())?,
                staking_credential: Some(StakingCredential::from_csl(&addr.stake_pointer())?),
            })
        } else if let Some(addr) = csl::address::EnterpriseAddress::from_address(value) {
            Ok(Address {
                credential: Credential::from_csl(&addr.payment_cred())?,
                staking_credential: None,
            })
        } else {
            Err(FromCSLError::ImpossibleConversion(format!(
                "Unable to represent address {:?}",
                value
            )))
        }
    }
}

impl FromCSL<csl::plutus::PlutusData> for PlutusData {
    fn from_csl(value: &csl::plutus::PlutusData) -> Result<Self, FromCSLError> {
        Ok(match value.kind() {
            csl::plutus::PlutusDataKind::ConstrPlutusData => {
                let constr_data = value.as_constr_plutus_data().unwrap();
                let tag = BigInt::from_csl(&constr_data.alternative())?;
                let args = Vec::from_csl(&constr_data.data())?;
                PlutusData::Constr(tag, args)
            }
            csl::plutus::PlutusDataKind::Map => {
                PlutusData::Map(Vec::from_csl(&value.as_map().unwrap())?)
            }
            csl::plutus::PlutusDataKind::List => {
                PlutusData::List(Vec::from_csl(&value.as_list().unwrap())?)
            }
            csl::plutus::PlutusDataKind::Integer => {
                PlutusData::Integer(BigInt::from_csl(&value.as_integer().unwrap())?)
            }
            csl::plutus::PlutusDataKind::Bytes => PlutusData::Bytes(value.as_bytes().unwrap()),
        })
    }
}

impl FromCSL<csl::plutus::PlutusList> for Vec<PlutusData> {
    fn from_csl(value: &csl::plutus::PlutusList) -> Result<Self, FromCSLError> {
        (0..value.len())
            .map(|idx| PlutusData::from_csl(&value.get(idx)))
            .collect()
    }
}

impl FromCSL<csl::plutus::PlutusMap> for Vec<(PlutusData, PlutusData)> {
    fn from_csl(c_map: &csl::plutus::PlutusMap) -> Result<Self, FromCSLError> {
        let keys = c_map.keys();
        (0..keys.len())
            .map(|idx| {
                let key = keys.get(idx);
                let value = c_map.get(&key).unwrap();
                Ok((FromCSL::from_csl(&key)?, FromCSL::from_csl(&value)?))
            })
            .collect()
    }
}

impl FromCSL<csl::crypto::DataHash> for DatumHash {
    fn from_csl(value: &csl::crypto::DataHash) -> Result<Self, FromCSLError> {
        Ok(DatumHash(LedgerBytes(value.to_bytes())))
    }
}

impl FromCSL<csl::OutputDatum> for OutputDatum {
    fn from_csl(value: &csl::OutputDatum) -> Result<Self, FromCSLError> {
        Ok(if let Some(d) = value.data() {
            OutputDatum::InlineDatum(Datum(PlutusData::from_csl(&d)?))
        } else if let Some(h) = value.data_hash() {
            OutputDatum::DatumHash(DatumHash::from_csl(&h)?)
        } else {
            OutputDatum::None
        })
    }
}

impl FromCSL<csl::TransactionOutput> for TransactionOutput {
    fn from_csl(value: &csl::TransactionOutput) -> Result<Self, FromCSLError> {
        Ok(TransactionOutput {
            address: FromCSL::from_csl(&value.address())?,
            datum: if value.has_data_hash() {
                OutputDatum::DatumHash(DatumHash::from_csl(&value.data_hash().unwrap())?)
            } else if value.has_plutus_data() {
                OutputDatum::InlineDatum(Datum(PlutusData::from_csl(
                    &value.plutus_data().unwrap(),
                )?))
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
                Some(ScriptHash::from_csl(&script_hash)?)
            } else {
                None
            },
            value: Value::from_csl(&value.amount())?,
        })
    }
}

impl FromCSL<csl::TransactionOutputs> for Vec<TransactionOutput> {
    fn from_csl(value: &csl::TransactionOutputs) -> Result<Self, FromCSLError> {
        (0..value.len())
            .map(|idx| TransactionOutput::from_csl(&value.get(idx)))
            .collect()
    }
}

impl FromCSL<csl::MintAssets> for BTreeMap<TokenName, BigInt> {
    fn from_csl(m_ass: &csl::MintAssets) -> Result<Self, FromCSLError> {
        let keys = m_ass.keys();
        (0..keys.len())
            .map(|idx| {
                let key = keys.get(idx);
                let value = m_ass.get(&key).unwrap();
                Ok((TokenName::from_csl(&key)?, BigInt::from_csl(&value)?))
            })
            .collect()
    }
}

impl FromCSL<csl::MintsAssets> for BTreeMap<TokenName, BigInt> {
    fn from_csl(value: &csl::MintsAssets) -> Result<Self, FromCSLError> {
        let mut m_ass_vec = Vec::new();

        // This is so stupid. `MintsAssets` doesn't have a `len` method for some reason.
        for idx in 0.. {
            if let Some(m_ass) = value.get(idx) {
                m_ass_vec.push(m_ass);
            } else {
                break;
            }
        }

        m_ass_vec.into_iter().try_fold(BTreeMap::new(), |acc, m| {
            let ass = BTreeMap::from_csl(&m)?;
            Ok(union_b_tree_maps_with(|l, r| l + r, [&acc, &ass]))
        })
    }
}

impl FromCSL<csl::Mint> for Value {
    fn from_csl(mint: &csl::Mint) -> Result<Self, FromCSLError> {
        let keys = mint.keys();
        Ok(Value(
      (0..keys.len())
        .map(|idx| {
          let sh = keys.get(idx);
          let ass = mint.get_all(&sh).unwrap_or(csl::MintsAssets::new());
          Ok((
            CurrencySymbol::NativeToken(MintingPolicyHash::from_csl(&sh)?),
            BTreeMap::from_csl(&ass)?,
          ))
        })
        .collect::<Result<BTreeMap<CurrencySymbol, BTreeMap<TokenName, BigInt>>, FromCSLError>>()?,
    ))
    }
}

impl FromCSL<csl::RequiredSigners> for Vec<Ed25519PubKeyHash> {
    fn from_csl(value: &csl::RequiredSigners) -> Result<Self, FromCSLError> {
        (0..value.len())
            .map(|idx| Ed25519PubKeyHash::from_csl(&value.get(idx)))
            .collect()
    }
}

impl FromCSL<csl::NativeScripts> for Vec<csl::NativeScript> {
    fn from_csl(value: &csl::NativeScripts) -> Result<Self, FromCSLError> {
        Ok((0..value.len()).map(|idx| value.get(idx)).collect())
    }
}

impl FromCSL<csl::plutus::PlutusScripts> for Vec<csl::plutus::PlutusScript> {
    fn from_csl(value: &csl::plutus::PlutusScripts) -> Result<Self, FromCSLError> {
        Ok((0..value.len()).map(|idx| value.get(idx)).collect())
    }
}
