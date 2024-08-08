//! Conversion from plutus-ledger-api to cardano-serialization-lib

use cardano_serialization_lib as csl;
use num_bigint::{BigInt, TryFromBigIntError};
use num_traits::sign::Signed;
use plutus_ledger_api::{
    plutus_data::PlutusData,
    v2::{
        address::{
            Address, CertificateIndex, ChainPointer, Credential, Slot, StakingCredential,
            TransactionIndex,
        },
        assoc_map::AssocMap,
        crypto::Ed25519PubKeyHash,
        datum::{Datum, DatumHash, OutputDatum},
        redeemer::Redeemer,
        script::{MintingPolicyHash, ScriptHash},
        transaction::{POSIXTimeRange, TransactionHash, TransactionInput, TransactionOutput},
        value::{CurrencySymbol, TokenName, Value},
    },
};
use std::collections::BTreeMap;

#[derive(Debug, thiserror::Error)]
pub enum TryFromPLAError {
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

    #[error("Script is missing from context: {0:?}")]
    MissingScript(ScriptHash),
}

/// Convert a plutus-ledger-api type to its cardano-serialization-lib counterpart
/// `try_to_csl_with` accepts extra data where the PLA data itself is not enough
pub trait TryFromPLA<T> {
    type ExtraInfo<'a>;

    fn try_from_pla_with<'a>(
        val: &T,
        extra_info: Self::ExtraInfo<'a>,
    ) -> Result<Self, TryFromPLAError>
    where
        Self: Sized;
}

/// Convert a plutus-ledger-api type to its cardano-serialization-lib counterpart
/// for cases where the csl data can be fully constructed from the pla data
///
/// DO NOT IMPLEMENT THIS DIRECTLY. Implement `TryFromPLA` instead.
pub trait TryFromPLAWithDef<T> {
    fn try_from_pla(val: &T) -> Result<Self, TryFromPLAError>
    where
        Self: Sized;
}

impl<'a, T, U> TryFromPLAWithDef<T> for U
where
    U: TryFromPLA<T, ExtraInfo<'a> = ()>,
{
    fn try_from_pla(val: &T) -> Result<Self, TryFromPLAError> {
        TryFromPLA::try_from_pla_with(val, Default::default())
    }
}

/// Convert a plutus-ledger-api type to its cardano-serialization-lib counterpart
/// `try_to_csl_with` accepts extra data where the PLA data itself is not enough
///
/// DO NOT IMPLEMENT THIS DIRECTLY. Implement `TryFromPLA` instead.
pub trait TryToCSL<T> {
    type ExtraInfo<'a>;

    fn try_to_csl_with<'a>(&self, extra_info: Self::ExtraInfo<'a>) -> Result<T, TryFromPLAError>;
}

impl<T, U> TryToCSL<U> for T
where
    U: TryFromPLA<T>,
{
    type ExtraInfo<'a> = U::ExtraInfo<'a>;

    fn try_to_csl_with<'a>(&self, extra_info: Self::ExtraInfo<'a>) -> Result<U, TryFromPLAError> {
        TryFromPLA::try_from_pla_with(self, extra_info)
    }
}

/// Convert a plutus-ledger-api type to its cardano-serialization-lib counterpart
/// for cases where the csl data can be fully constructed from the pla data
///
/// DO NOT IMPLEMENT THIS DIRECTLY. Implement `TryFromPLA` instead.
pub trait TryToCSLWithDef<T>: TryToCSL<T> {
    fn try_to_csl(&self) -> Result<T, TryFromPLAError>;
}

impl<T, U> TryToCSLWithDef<U> for T
where
    U: TryFromPLAWithDef<T> + TryFromPLA<T>,
{
    fn try_to_csl(&self) -> Result<U, TryFromPLAError> {
        TryFromPLAWithDef::try_from_pla(self)
    }
}

impl TryFromPLA<u64> for csl::utils::BigNum {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &u64, _: ()) -> Result<Self, TryFromPLAError> {
        // BigNum(s) are u64 under the hood.

        Ok(csl::utils::BigNum::from(*val))
    }
}

impl TryFromPLA<BigInt> for csl::utils::BigNum {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &BigInt, _: ()) -> Result<Self, TryFromPLAError> {
        // BigNum(s) are u64 under the hood.
        let x: u64 = val
            .to_owned()
            .try_into()
            .map_err(|err: TryFromBigIntError<BigInt>| {
                TryFromPLAError::BigIntOutOfRange(err.into_original(), "u64".into())
            })?;

        x.try_to_csl()
    }
}

impl TryFromPLA<BigInt> for csl::utils::BigInt {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &BigInt, _: ()) -> Result<Self, TryFromPLAError> {
        Ok(val.to_owned().into())
    }
}

impl TryFromPLA<BigInt> for csl::utils::Int {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &BigInt, _: ()) -> Result<Self, TryFromPLAError> {
        if val.is_negative() {
            Ok(csl::utils::Int::new_negative(&(val.abs()).try_to_csl()?))
        } else {
            Ok(csl::utils::Int::new(&val.try_to_csl()?))
        }
    }
}

impl TryFromPLA<i64> for csl::utils::Int {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &i64, _: ()) -> Result<Self, TryFromPLAError> {
        if val.is_negative() {
            Ok(csl::utils::Int::new_negative(&csl::utils::to_bignum(
                val.abs() as u64,
            )))
        } else {
            Ok(csl::utils::Int::new(&csl::utils::to_bignum(*val as u64)))
        }
    }
}

impl TryFromPLA<Ed25519PubKeyHash> for csl::crypto::Ed25519KeyHash {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &Ed25519PubKeyHash, _: ()) -> Result<Self, TryFromPLAError> {
        csl::crypto::Ed25519KeyHash::from_bytes(val.0 .0.to_owned())
            .map_err(TryFromPLAError::CSLDeserializeError)
    }
}

impl TryFromPLA<ScriptHash> for csl::crypto::ScriptHash {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &ScriptHash, _: ()) -> Result<Self, TryFromPLAError> {
        csl::crypto::ScriptHash::from_bytes(val.0 .0.to_owned())
            .map_err(TryFromPLAError::CSLDeserializeError)
    }
}

impl TryFromPLA<TransactionHash> for csl::crypto::TransactionHash {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &TransactionHash, _: ()) -> Result<Self, TryFromPLAError> {
        csl::crypto::TransactionHash::from_bytes(val.0 .0.to_owned())
            .map_err(TryFromPLAError::CSLDeserializeError)
    }
}

impl TryFromPLA<BigInt> for u32 /* TransactionIndex */ {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &BigInt, _: ()) -> Result<Self, TryFromPLAError> {
        val.to_owned()
            .try_into()
            .map_err(|err: TryFromBigIntError<BigInt>| {
                TryFromPLAError::BigIntOutOfRange(err.into_original(), "u32".into())
            })
    }
}

impl TryFromPLA<TransactionInput> for csl::TransactionInput {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &TransactionInput, _: ()) -> Result<Self, TryFromPLAError> {
        Ok(csl::TransactionInput::new(
            &val.transaction_id.try_to_csl()?,
            val.index.try_to_csl()?,
        ))
    }
}

impl TryFromPLA<Vec<TransactionInput>> for csl::TransactionInputs {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &Vec<TransactionInput>, _: ()) -> Result<Self, TryFromPLAError> {
        val.iter()
            .try_fold(csl::TransactionInputs::new(), |mut acc, input| {
                acc.add(&input.try_to_csl()?);
                Ok(acc)
            })
    }
}

pub struct TransactionOutputExtraInfo<'a> {
    pub scripts: &'a BTreeMap<ScriptHash, crate::utils::script::ScriptOrRef>,
    pub network_id: u8,
    pub data_cost: &'a csl::DataCost,
}

impl TryFromPLA<TransactionOutput> for csl::TransactionOutput {
    type ExtraInfo<'a> = TransactionOutputExtraInfo<'a>;

    fn try_from_pla_with(
        val: &TransactionOutput,
        extra_info: TransactionOutputExtraInfo,
    ) -> Result<Self, TryFromPLAError> {
        let mut output_builder = csl::output_builder::TransactionOutputBuilder::new()
            .with_address(&val.address.try_to_csl_with(extra_info.network_id)?);

        output_builder = match &val.datum {
            OutputDatum::None => output_builder,
            OutputDatum::InlineDatum(Datum(d)) => output_builder.with_plutus_data(&d.try_to_csl()?),
            OutputDatum::DatumHash(dh) => output_builder.with_data_hash(&dh.try_to_csl()?),
        };

        let script_ref = val
            .reference_script
            .clone()
            .map(|script_hash| -> Result<_, TryFromPLAError> {
                let script_or_ref = extra_info
                    .scripts
                    .get(&script_hash)
                    .ok_or(TryFromPLAError::MissingScript(script_hash))?;
                Ok(match script_or_ref {
                    crate::utils::script::ScriptOrRef::RefScript(_, script) => {
                        csl::ScriptRef::new_plutus_script(&script)
                    }
                    crate::utils::script::ScriptOrRef::PlutusScript(script) => {
                        csl::ScriptRef::new_plutus_script(&script)
                    }
                })
            })
            .transpose()?;

        if let Some(script_ref) = &script_ref {
            output_builder = output_builder.with_script_ref(&script_ref);
        };

        let value_without_min_utxo = val.value.try_to_csl()?;

        let mut calc = csl::utils::MinOutputAdaCalculator::new_empty(extra_info.data_cost)
            .map_err(TryFromPLAError::CSLJsError)?;
        calc.set_amount(&value_without_min_utxo);
        match &val.datum {
            OutputDatum::None => {}
            OutputDatum::InlineDatum(Datum(d)) => {
                calc.set_plutus_data(&d.try_to_csl()?);
            }
            OutputDatum::DatumHash(dh) => {
                calc.set_data_hash(&dh.try_to_csl()?);
            }
        };
        if let Some(script_ref) = script_ref {
            calc.set_script_ref(&script_ref);
        }

        let required_coin = calc.calculate_ada().map_err(TryFromPLAError::CSLJsError)?;
        let coin = std::cmp::max(value_without_min_utxo.coin(), required_coin);

        let value = match value_without_min_utxo.multiasset() {
            Some(multiasset) => csl::utils::Value::new_with_assets(&coin, &multiasset),
            None => csl::utils::Value::new(&coin),
        };

        output_builder
            .next()
            .map_err(TryFromPLAError::CSLJsError)?
            .with_value(&value)
            .build()
            .map_err(TryFromPLAError::CSLJsError)
    }
}

impl TryFromPLA<MintingPolicyHash> for csl::PolicyID {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &MintingPolicyHash, _: ()) -> Result<Self, TryFromPLAError> {
        val.0.try_to_csl()
    }
}

impl TryFromPLA<TokenName> for csl::AssetName {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &TokenName, _: ()) -> Result<Self, TryFromPLAError> {
        csl::AssetName::new(val.0 .0.to_owned()).map_err(TryFromPLAError::CSLJsError)
    }
}

impl TryFromPLA<BTreeMap<TokenName, BigInt>> for csl::Assets {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(
        val: &BTreeMap<TokenName, BigInt>,
        _: (),
    ) -> Result<Self, TryFromPLAError> {
        val.iter().try_fold(csl::Assets::new(), |mut acc, (k, v)| {
            acc.insert(&k.try_to_csl()?, &v.try_to_csl()?);
            Ok(acc)
        })
    }
}

impl TryFromPLA<BTreeMap<TokenName, BigInt>> for csl::MintAssets {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(
        val: &BTreeMap<TokenName, BigInt>,
        _: (),
    ) -> Result<Self, TryFromPLAError> {
        val.iter()
            .try_fold(csl::MintAssets::new(), |mut acc, (k, v)| {
                acc.insert(&k.try_to_csl()?, v.try_to_csl()?);
                Ok(acc)
            })
    }
}

impl TryFromPLA<Value> for csl::utils::Value {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &Value, _: ()) -> Result<Self, TryFromPLAError> {
        let coin: csl::utils::Coin = val
            .0
            .get(&CurrencySymbol::Ada)
            .and_then(|m| m.get(&TokenName::ada()))
            .map_or(Ok(csl::utils::BigNum::zero()), TryToCSLWithDef::try_to_csl)?;

        let m_ass = val
            .0
            .iter()
            .filter_map(|(cs, tn_map)| match &cs {
                CurrencySymbol::Ada => None,
                CurrencySymbol::NativeToken(h) => Some((h, tn_map)),
            })
            .try_fold(csl::MultiAsset::new(), |mut acc, (cs, ass)| {
                acc.insert(&cs.try_to_csl()?, &ass.try_to_csl()?);
                Ok(acc)
            })?;

        let mut v = csl::utils::Value::new(&coin);

        v.set_multiasset(&m_ass);

        Ok(v)
    }
}

impl TryFromPLA<PlutusData> for csl::plutus::PlutusData {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &PlutusData, _: ()) -> Result<Self, TryFromPLAError> {
        match val {
            PlutusData::Constr(tag, args) => Ok(csl::plutus::PlutusData::new_constr_plutus_data(
                &csl::plutus::ConstrPlutusData::new(&tag.try_to_csl()?, &args.try_to_csl()?),
            )),
            PlutusData::Map(l) => Ok(csl::plutus::PlutusData::new_map(&l.try_to_csl()?)),
            PlutusData::List(l) => Ok(csl::plutus::PlutusData::new_list(&l.try_to_csl()?)),
            PlutusData::Integer(i) => Ok(csl::plutus::PlutusData::new_integer(&i.try_to_csl()?)),
            PlutusData::Bytes(b) => Ok(csl::plutus::PlutusData::new_bytes(b.to_owned())),
        }
    }
}

impl TryFromPLA<Vec<PlutusData>> for csl::plutus::PlutusList {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &Vec<PlutusData>, _: ()) -> Result<Self, TryFromPLAError> {
        val.iter()
            // traverse
            .map(|x| x.try_to_csl())
            .collect::<Result<Vec<csl::plutus::PlutusData>, TryFromPLAError>>()
            .map(|x| x.into())
    }
}

impl TryFromPLA<Vec<(PlutusData, PlutusData)>> for csl::plutus::PlutusMap {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(
        val: &Vec<(PlutusData, PlutusData)>,
        _: (),
    ) -> Result<Self, TryFromPLAError> {
        val.iter()
            .try_fold(csl::plutus::PlutusMap::new(), |mut acc, (k, v)| {
                acc.insert(&k.try_to_csl()?, &v.try_to_csl()?);
                Ok(acc)
            })
    }
}

impl TryFromPLA<DatumHash> for csl::crypto::DataHash {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &DatumHash, _: ()) -> Result<Self, TryFromPLAError> {
        csl::crypto::DataHash::from_bytes(val.0 .0.to_owned())
            .map_err(TryFromPLAError::CSLDeserializeError)
    }
}

impl TryFromPLA<Datum> for csl::plutus::PlutusData {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &Datum, _: ()) -> Result<Self, TryFromPLAError> {
        val.0.try_to_csl()
    }
}

impl TryFromPLA<ChainPointer> for csl::address::Pointer {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &ChainPointer, _: ()) -> Result<Self, TryFromPLAError> {
        Ok(csl::address::Pointer::new_pointer(
            &val.slot_number.try_to_csl()?,
            &val.transaction_index.try_to_csl()?,
            &val.certificate_index.try_to_csl()?,
        ))
    }
}

impl TryFromPLA<Slot> for csl::utils::BigNum {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &Slot, _: ()) -> Result<Self, TryFromPLAError> {
        val.0.try_to_csl()
    }
}

impl TryFromPLA<TransactionIndex> for csl::utils::BigNum {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &TransactionIndex, _: ()) -> Result<Self, TryFromPLAError> {
        val.0.try_to_csl()
    }
}

impl TryFromPLA<CertificateIndex> for csl::utils::BigNum {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &CertificateIndex, _: ()) -> Result<Self, TryFromPLAError> {
        val.0.try_to_csl()
    }
}

impl TryFromPLA<Credential> for csl::address::StakeCredential {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &Credential, _: ()) -> Result<Self, TryFromPLAError> {
        match val {
            Credential::PubKey(pkh) => Ok(csl::address::StakeCredential::from_keyhash(
                &pkh.try_to_csl()?,
            )),
            Credential::Script(sh) => Ok(csl::address::StakeCredential::from_scripthash(
                &sh.0.try_to_csl()?,
            )),
        }
    }
}

impl TryFromPLA<StakingCredential> for csl::address::StakeCredential {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(val: &StakingCredential, _: ()) -> Result<Self, TryFromPLAError> {
        match val {
            StakingCredential::Hash(c) => c.try_to_csl(),
            StakingCredential::Pointer(_) => Err(TryFromPLAError::ImpossibleConversion(
                "cannot represent chain pointer".into(),
            )),
        }
    }
}

impl TryFromPLA<Address> for csl::address::Address {
    type ExtraInfo<'a> = u8;

    fn try_from_pla_with(val: &Address, network_tag: u8) -> Result<Self, TryFromPLAError> {
        let payment = val.credential.try_to_csl()?;

        Ok(match val.staking_credential {
            None => csl::address::EnterpriseAddress::new(network_tag, &payment).to_address(),
            Some(ref sc) => match sc {
                StakingCredential::Hash(c) => {
                    csl::address::BaseAddress::new(network_tag, &payment, &c.try_to_csl()?)
                        .to_address()
                }
                StakingCredential::Pointer(ptr) => {
                    csl::address::PointerAddress::new(network_tag, &payment, &ptr.try_to_csl()?)
                        .to_address()
                }
            },
        })
    }
}

impl TryFromPLA<StakingCredential> for csl::address::RewardAddress {
    type ExtraInfo<'a> = u8;

    fn try_from_pla_with(
        val: &StakingCredential,
        network_tag: u8,
    ) -> Result<Self, TryFromPLAError> {
        Ok(csl::address::RewardAddress::new(
            network_tag,
            &val.try_to_csl()?,
        ))
    }
}

impl TryFromPLA<AssocMap<StakingCredential, BigInt>> for csl::Withdrawals {
    type ExtraInfo<'a> = u8;

    fn try_from_pla_with(
        val: &AssocMap<StakingCredential, BigInt>,
        network_tag: u8,
    ) -> Result<Self, TryFromPLAError> {
        val.0
            .iter()
            .try_fold(csl::Withdrawals::new(), |mut acc, (s, q)| {
                acc.insert(&s.try_to_csl_with(network_tag)?, &q.try_to_csl()?);
                Ok(acc)
            })
    }
}

impl TryFromPLA<Redeemer> for csl::plutus::Redeemer {
    type ExtraInfo<'a> = (&'a csl::plutus::RedeemerTag, u64);

    fn try_from_pla_with<'a>(
        pla_redeemer: &Redeemer,
        (red_tag, red_idx): Self::ExtraInfo<'a>,
    ) -> Result<csl::plutus::Redeemer, TryFromPLAError> {
        let Redeemer(plutus_data) = pla_redeemer;
        Ok(csl::plutus::Redeemer::new(
            &red_tag,
            &red_idx.try_to_csl()?,
            &plutus_data.try_to_csl()?,
            &csl::plutus::ExUnits::new(&csl::utils::to_bignum(0), &csl::utils::to_bignum(0)),
        ))
    }
}

impl TryFromPLA<OutputDatum> for Option<csl::OutputDatum> {
    type ExtraInfo<'a> = ();

    fn try_from_pla_with(
        pla_output_datum: &OutputDatum,
        _: (),
    ) -> Result<Option<csl::OutputDatum>, TryFromPLAError> {
        Ok(match pla_output_datum {
            OutputDatum::None => None,
            OutputDatum::InlineDatum(Datum(d)) => {
                Some(csl::OutputDatum::new_data(&d.try_to_csl()?))
            }
            OutputDatum::DatumHash(dh) => Some(csl::OutputDatum::new_data_hash(&dh.try_to_csl()?)),
        })
    }
}
