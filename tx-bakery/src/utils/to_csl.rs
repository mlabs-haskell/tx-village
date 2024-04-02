use cardano_serialization_lib as csl;
use num_bigint::{BigInt, TryFromBigIntError};
use plutus_ledger_api::{
    plutus_data::PlutusData,
    v2::{
        address::{
            Address, CertificateIndex, ChainPointer, Credential, Slot, StakingCredential,
            TransactionIndex,
        },
        assoc_map::AssocMap,
        crypto::Ed25519PubKeyHash,
        datum::{Datum, DatumHash},
        script::{MintingPolicyHash, ScriptHash},
        transaction::{POSIXTimeRange, TransactionHash, TransactionInput},
        value::{CurrencySymbol, TokenName, Value},
    },
};
use std::collections::BTreeMap;

#[derive(Debug, thiserror::Error)]
pub enum ToCSLError {
    #[error("{0}")]
    CSLDeserializeError(csl::error::DeserializeError),
    #[error("{0}")]
    CSLJsError(csl::error::JsError),
    #[error("Unable to cast BigInt {0} into type {1}: value is out of bound")]
    BigIntOutOfRange(BigInt, String),
    #[error("Unable to represent PLA value in CSL: ${0}")]
    ImpossibleConversion(String),
    #[error("Invalid valid transaction time range: ${0:?}")]
    InvalidTimeRange(POSIXTimeRange),
}

/// Convert a PLA type to CSL - core version.
pub trait ToCSL<T> {
    type ExtraInfo;
    fn to_csl_with(&self, extra_info: Self::ExtraInfo) -> Result<T, ToCSLError>;
}

/// Blanket implementation for ToCSL without extra info.
/// DO NOT IMPLEMENT THIS DIRECTLY. Implement `ToCSL` instead.
pub trait ToCSLWithDef<T>: ToCSL<T> {
    fn to_csl(&self) -> Result<T, ToCSLError>;
}

impl<T, U: ToCSL<T, ExtraInfo = ()>> ToCSLWithDef<T> for U {
    fn to_csl(&self) -> Result<T, ToCSLError> {
        self.to_csl_with(Default::default())
    }
}

impl ToCSL<csl::utils::BigNum> for BigInt {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::utils::BigNum, ToCSLError> {
        // BigNum(s) are u64 under the hood.
        let x: u64 = self
            .to_owned()
            .try_into()
            .map_err(|err: TryFromBigIntError<BigInt>| {
                ToCSLError::BigIntOutOfRange(err.into_original(), "u64".into())
            })?;

        Ok(csl::utils::BigNum::from(x))
    }
}

impl ToCSL<csl::utils::BigInt> for BigInt {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::utils::BigInt, ToCSLError> {
        Ok(self.to_owned().into())
    }
}

impl ToCSL<csl::utils::Int> for BigInt {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::utils::Int, ToCSLError> {
        Ok(csl::utils::Int::new(&self.to_csl()?))
    }
}

impl ToCSL<csl::crypto::Ed25519KeyHash> for Ed25519PubKeyHash {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::crypto::Ed25519KeyHash, ToCSLError> {
        csl::crypto::Ed25519KeyHash::from_bytes(self.0 .0.to_owned())
            .map_err(ToCSLError::CSLDeserializeError)
    }
}

impl ToCSL<csl::crypto::ScriptHash> for ScriptHash {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::crypto::ScriptHash, ToCSLError> {
        csl::crypto::ScriptHash::from_bytes(self.0 .0.to_owned())
            .map_err(ToCSLError::CSLDeserializeError)
    }
}

impl ToCSL<csl::crypto::TransactionHash> for TransactionHash {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::crypto::TransactionHash, ToCSLError> {
        csl::crypto::TransactionHash::from_bytes(self.0 .0.to_owned())
            .map_err(ToCSLError::CSLDeserializeError)
    }
}

impl ToCSL<u32 /* TransactionIndex */> for BigInt {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<u32, ToCSLError> {
        self.to_owned()
            .try_into()
            .map_err(|err: TryFromBigIntError<BigInt>| {
                ToCSLError::BigIntOutOfRange(err.into_original(), "u32".into())
            })
    }
}

impl ToCSL<csl::TransactionInput> for TransactionInput {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::TransactionInput, ToCSLError> {
        Ok(csl::TransactionInput::new(
            &self.transaction_id.to_csl()?,
            self.index.to_csl()?,
        ))
    }
}

impl ToCSL<csl::TransactionInputs> for Vec<TransactionInput> {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::TransactionInputs, ToCSLError> {
        self.iter()
            .try_fold(csl::TransactionInputs::new(), |mut acc, input| {
                acc.add(&input.to_csl()?);
                Ok(acc)
            })
    }
}

impl ToCSL<csl::PolicyID> for MintingPolicyHash {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::PolicyID, ToCSLError> {
        self.0.to_csl()
    }
}

impl ToCSL<csl::AssetName> for TokenName {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::AssetName, ToCSLError> {
        csl::AssetName::new(self.0 .0.to_owned()).map_err(ToCSLError::CSLJsError)
    }
}

impl ToCSL<csl::Assets> for BTreeMap<TokenName, BigInt> {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::Assets, ToCSLError> {
        self.iter().try_fold(csl::Assets::new(), |mut acc, (k, v)| {
            acc.insert(&k.to_csl()?, &v.to_csl()?);
            Ok(acc)
        })
    }
}

impl ToCSL<csl::MintAssets> for BTreeMap<TokenName, BigInt> {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::MintAssets, ToCSLError> {
        self.iter()
            .try_fold(csl::MintAssets::new(), |mut acc, (k, v)| {
                acc.insert(&k.to_csl()?, v.to_csl()?);
                Ok(acc)
            })
    }
}

impl ToCSL<csl::utils::Value> for Value {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::utils::Value, ToCSLError> {
        let coin: csl::utils::Coin = self
            .0
            .get(&CurrencySymbol::Ada)
            .and_then(|m| m.get(&TokenName::ada()))
            .map_or(Ok(csl::utils::BigNum::zero()), ToCSLWithDef::to_csl)?;

        let m_ass = self
            .0
            .iter()
            .filter_map(|(cs, tn_map)| match &cs {
                CurrencySymbol::Ada => None,
                CurrencySymbol::NativeToken(h) => Some((h, tn_map)),
            })
            .try_fold(csl::MultiAsset::new(), |mut acc, (cs, ass)| {
                acc.insert(&cs.to_csl()?, &ass.to_csl()?);
                Ok(acc)
            })?;

        let mut v = csl::utils::Value::new(&coin);

        v.set_multiasset(&m_ass);

        Ok(v)
    }
}

impl ToCSL<csl::plutus::PlutusData> for PlutusData {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::plutus::PlutusData, ToCSLError> {
        match &self {
            PlutusData::Constr(tag, args) => Ok(csl::plutus::PlutusData::new_constr_plutus_data(
                &csl::plutus::ConstrPlutusData::new(&tag.to_csl()?, &args.to_csl()?),
            )),
            PlutusData::Map(l) => Ok(csl::plutus::PlutusData::new_map(&l.to_csl()?)),
            PlutusData::List(l) => Ok(csl::plutus::PlutusData::new_list(&l.to_csl()?)),
            PlutusData::Integer(i) => Ok(csl::plutus::PlutusData::new_integer(&i.to_csl()?)),
            PlutusData::Bytes(b) => Ok(csl::plutus::PlutusData::new_bytes(b.to_owned())),
        }
    }
}

impl ToCSL<csl::plutus::PlutusList> for Vec<PlutusData> {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::plutus::PlutusList, ToCSLError> {
        self.iter()
            // traverse
            .map(|x| x.to_csl())
            .collect::<Result<Vec<csl::plutus::PlutusData>, ToCSLError>>()
            .map(|x| x.into())
    }
}

impl ToCSL<csl::plutus::PlutusMap> for Vec<(PlutusData, PlutusData)> {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::plutus::PlutusMap, ToCSLError> {
        self.iter()
            .try_fold(csl::plutus::PlutusMap::new(), |mut acc, (k, v)| {
                acc.insert(&k.to_csl()?, &v.to_csl()?);
                Ok(acc)
            })
    }
}

impl ToCSL<csl::crypto::DataHash> for DatumHash {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::crypto::DataHash, ToCSLError> {
        csl::crypto::DataHash::from_bytes(self.0 .0.to_owned())
            .map_err(ToCSLError::CSLDeserializeError)
    }
}

impl ToCSL<csl::plutus::PlutusData> for Datum {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::plutus::PlutusData, ToCSLError> {
        self.0.to_csl()
    }
}

impl ToCSL<csl::address::Pointer> for ChainPointer {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::address::Pointer, ToCSLError> {
        Ok(csl::address::Pointer::new_pointer(
            &self.slot_number.to_csl()?,
            &self.transaction_index.to_csl()?,
            &self.certificate_index.to_csl()?,
        ))
    }
}

impl ToCSL<csl::utils::BigNum> for Slot {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::utils::BigNum, ToCSLError> {
        self.0.to_csl()
    }
}

impl ToCSL<csl::utils::BigNum> for TransactionIndex {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::utils::BigNum, ToCSLError> {
        self.0.to_csl()
    }
}

impl ToCSL<csl::utils::BigNum> for CertificateIndex {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::utils::BigNum, ToCSLError> {
        self.0.to_csl()
    }
}

impl ToCSL<csl::address::StakeCredential> for Credential {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::address::StakeCredential, ToCSLError> {
        match &self {
            Credential::PubKey(pkh) => {
                Ok(csl::address::StakeCredential::from_keyhash(&pkh.to_csl()?))
            }
            Credential::Script(sh) => Ok(csl::address::StakeCredential::from_scripthash(
                &sh.0.to_csl()?,
            )),
        }
    }
}

impl ToCSL<csl::address::StakeCredential> for StakingCredential {
    type ExtraInfo = ();
    fn to_csl_with(&self, _: ()) -> Result<csl::address::StakeCredential, ToCSLError> {
        match self {
            StakingCredential::Hash(c) => c.to_csl(),
            StakingCredential::Pointer(_) => Err(ToCSLError::ImpossibleConversion(
                "cannot represent chain pointer".into(),
            )),
        }
    }
}

impl ToCSL<csl::address::Address> for Address {
    type ExtraInfo = u8;
    fn to_csl_with(&self, network_tag: u8) -> Result<csl::address::Address, ToCSLError> {
        let payment = self.credential.to_csl()?;

        Ok(match &self.staking_credential {
            None => csl::address::EnterpriseAddress::new(network_tag, &payment).to_address(),
            Some(sc) => match sc {
                StakingCredential::Hash(c) => {
                    csl::address::BaseAddress::new(network_tag, &payment, &c.to_csl()?).to_address()
                }
                StakingCredential::Pointer(ptr) => {
                    csl::address::PointerAddress::new(network_tag, &payment, &ptr.to_csl()?)
                        .to_address()
                }
            },
        })
    }
}

impl ToCSL<csl::address::RewardAddress> for StakingCredential {
    type ExtraInfo = u8;
    fn to_csl_with(&self, network_tag: u8) -> Result<csl::address::RewardAddress, ToCSLError> {
        Ok(csl::address::RewardAddress::new(
            network_tag,
            &self.to_csl()?,
        ))
    }
}

impl ToCSL<csl::Withdrawals> for AssocMap<StakingCredential, BigInt> {
    type ExtraInfo = u8;
    fn to_csl_with(&self, network_tag: u8) -> Result<csl::Withdrawals, ToCSLError> {
        self.0
            .iter()
            .try_fold(csl::Withdrawals::new(), |mut acc, (s, q)| {
                acc.insert(&s.to_csl_with(network_tag)?, &q.to_csl()?);
                Ok(acc)
            })
    }
}
