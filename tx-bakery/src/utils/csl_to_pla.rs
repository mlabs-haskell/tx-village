//! Conversion from cardano-serialization-lib to plutus-ledger-api

use crate::{chain_query::EraSummary, time::slot_into_posix_time};

use super::union_b_tree_maps_with;
use cardano_serialization_lib as csl;
use chrono::{DateTime, Utc};
use num_bigint::{BigInt, ParseBigIntError};
use plutus_ledger_api::{
    plutus_data::PlutusData,
    v2::{
        address::{
            Address, CertificateIndex, ChainPointer, Credential, Slot, StakingCredential,
            TransactionIndex,
        },
        assoc_map::AssocMap,
        crypto::{Ed25519PubKeyHash, LedgerBytes, PaymentPubKeyHash},
        datum::{Datum, DatumHash, OutputDatum},
        interval::{Extended, LowerBound, UpperBound},
        redeemer::Redeemer,
        script::{MintingPolicyHash, ScriptHash, ValidatorHash},
        transaction::{
            DCert, POSIXTimeRange, ScriptPurpose, TransactionHash, TransactionInfo,
            TransactionInput, TransactionOutput, TxInInfo,
        },
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

    #[error("Unable to find utxo with reference: {0:?}")]
    UtxoNotFound(TransactionInput),

    #[error("Unable to convert slot to POSIX ttime: {0}")]
    InvalidSlot(String),

    #[error("This data cannot be converted into Plutus ledger API type: {0}")]
    Unsupported(String),
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

pub trait TryFromCSLWith<T> {
    type ExtraInfo<'a>;

    fn try_from_csl_with(
        value: &T,
        extra_info: Self::ExtraInfo<'_>,
    ) -> Result<Self, TryFromCSLError>
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

pub trait TryToPLAWith<T> {
    type ExtraInfo<'a>;

    fn try_to_pla_with(&self, extra_info: Self::ExtraInfo<'_>) -> Result<T, TryFromCSLError>;
}

impl<T, U> TryToPLAWith<U> for T
where
    U: TryFromCSLWith<T>,
{
    type ExtraInfo<'a> = U::ExtraInfo<'a>;

    fn try_to_pla_with(&self, extra_info: Self::ExtraInfo<'_>) -> Result<U, TryFromCSLError> {
        U::try_from_csl_with(self, extra_info)
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

impl FromCSL<csl::Value> for Value {
    fn from_csl(value: &csl::Value) -> Self {
        let lovelaces = BigInt::from_csl(&value.coin());
        let mut pla_value = Value::ada_value(&lovelaces);
        if let Some(multi_asset) = value.multiasset() {
            pla_value = &pla_value + &Value::from_csl(&multi_asset)
        }
        pla_value
    }
}

impl FromCSL<csl::BigNum> for BigInt {
    fn from_csl(value: &csl::BigNum) -> Self {
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

impl TryFromCSL<csl::BigInt> for BigInt {
    fn try_from_csl(value: &csl::BigInt) -> Result<Self, TryFromCSLError> {
        BigInt::from_str(&value.to_str()).map_err(TryFromCSLError::InvalidBigInt)
    }
}

impl FromCSL<csl::Int> for BigInt {
    fn from_csl(value: &csl::Int) -> Self {
        if value.is_positive() {
            BigInt::from_csl(&value.as_positive().unwrap())
        } else {
            BigInt::from_csl(&value.as_negative().unwrap()).neg()
        }
    }
}

impl FromCSL<csl::TransactionHash> for TransactionHash {
    fn from_csl(value: &csl::TransactionHash) -> Self {
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

impl FromCSL<csl::Ed25519KeyHash> for Ed25519PubKeyHash {
    fn from_csl(value: &csl::Ed25519KeyHash) -> Self {
        Ed25519PubKeyHash(LedgerBytes(value.to_bytes()))
    }
}

impl FromCSL<csl::ScriptHash> for ScriptHash {
    fn from_csl(value: &csl::ScriptHash) -> Self {
        ScriptHash(LedgerBytes(value.to_bytes()))
    }
}

impl FromCSL<csl::ScriptHash> for ValidatorHash {
    fn from_csl(value: &csl::ScriptHash) -> Self {
        ValidatorHash(ScriptHash::from_csl(value))
    }
}

impl FromCSL<csl::Credential> for StakingCredential {
    fn from_csl(value: &csl::Credential) -> Self {
        StakingCredential::Hash(Credential::from_csl(value))
    }
}

impl FromCSL<csl::Credential> for Credential {
    fn from_csl(value: &csl::Credential) -> Self {
        match value.kind() {
            csl::CredKind::Key => {
                Credential::PubKey(Ed25519PubKeyHash::from_csl(&value.to_keyhash().unwrap()))
            }
            csl::CredKind::Script => {
                Credential::Script(ValidatorHash::from_csl(&value.to_scripthash().unwrap()))
            }
        }
    }
}

impl FromCSL<csl::BigNum> for Slot {
    fn from_csl(value: &csl::BigNum) -> Self {
        Slot(BigInt::from_csl(value))
    }
}

impl FromCSL<csl::BigNum> for TransactionIndex {
    fn from_csl(value: &csl::BigNum) -> Self {
        TransactionIndex(BigInt::from_csl(value))
    }
}

impl FromCSL<csl::BigNum> for CertificateIndex {
    fn from_csl(value: &csl::BigNum) -> Self {
        CertificateIndex(BigInt::from_csl(value))
    }
}

impl FromCSL<csl::Pointer> for ChainPointer {
    fn from_csl(value: &csl::Pointer) -> Self {
        ChainPointer {
            slot_number: Slot::from_csl(&value.slot_bignum()),
            transaction_index: TransactionIndex::from_csl(&value.tx_index_bignum()),
            certificate_index: CertificateIndex::from_csl(&value.cert_index_bignum()),
        }
    }
}

impl FromCSL<csl::Pointer> for StakingCredential {
    fn from_csl(value: &csl::Pointer) -> Self {
        StakingCredential::Pointer(ChainPointer::from_csl(value))
    }
}
impl TryFromCSL<csl::Address> for Address {
    fn try_from_csl(value: &csl::Address) -> Result<Self, TryFromCSLError> {
        if let Some(addr) = csl::BaseAddress::from_address(value) {
            Ok(Address {
                credential: Credential::from_csl(&addr.payment_cred()),
                staking_credential: Some(StakingCredential::from_csl(&addr.stake_cred())),
            })
        } else if let Some(addr) = csl::PointerAddress::from_address(value) {
            Ok(Address {
                credential: Credential::from_csl(&addr.payment_cred()),
                staking_credential: Some(StakingCredential::from_csl(&addr.stake_pointer())),
            })
        } else if let Some(addr) = csl::EnterpriseAddress::from_address(value) {
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

impl TryFromCSL<csl::PlutusData> for PlutusData {
    fn try_from_csl(value: &csl::PlutusData) -> Result<Self, TryFromCSLError> {
        Ok(match value.kind() {
            csl::PlutusDataKind::ConstrPlutusData => {
                let constr_data = value.as_constr_plutus_data().unwrap();
                let tag = BigInt::from_csl(&constr_data.alternative());
                let args = constr_data.data().try_to_pla()?;
                PlutusData::Constr(tag, args)
            }
            csl::PlutusDataKind::Map => PlutusData::Map(value.as_map().unwrap().try_to_pla()?),
            csl::PlutusDataKind::List => PlutusData::List(value.as_list().unwrap().try_to_pla()?),
            csl::PlutusDataKind::Integer => {
                PlutusData::Integer(value.as_integer().unwrap().try_to_pla()?)
            }
            csl::PlutusDataKind::Bytes => PlutusData::Bytes(value.as_bytes().unwrap()),
        })
    }
}

impl TryFromCSL<csl::PlutusList> for Vec<PlutusData> {
    fn try_from_csl(value: &csl::PlutusList) -> Result<Self, TryFromCSLError> {
        (0..value.len())
            .map(|idx| value.get(idx).try_to_pla())
            .collect()
    }
}

impl TryFromCSL<csl::PlutusMap> for Vec<(PlutusData, PlutusData)> {
    fn try_from_csl(c_map: &csl::PlutusMap) -> Result<Self, TryFromCSLError> {
        let keys = c_map.keys();
        (0..keys.len()).try_fold(Vec::new(), |mut vector, idx| {
            let key = keys.get(idx);
            let values = c_map.get(&key).unwrap();

            for value_idx in 0..values.len() {
                vector.push((
                    key.clone().try_to_pla()?,
                    values.get(value_idx).unwrap().try_to_pla()?,
                ))
            }

            Ok(vector)
        })
    }
}

impl FromCSL<csl::DataHash> for DatumHash {
    fn from_csl(value: &csl::DataHash) -> Self {
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
                    let ass = mint.get(&sh).unwrap_or(csl::MintsAssets::new());
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

impl FromCSL<csl::PlutusScripts> for Vec<csl::PlutusScript> {
    fn from_csl(value: &csl::PlutusScripts) -> Self {
        (0..value.len()).map(|idx| value.get(idx)).collect()
    }
}

impl FromCSL<csl::RewardAddress> for StakingCredential {
    fn from_csl(value: &csl::RewardAddress) -> Self {
        value.payment_cred().to_pla()
    }
}

impl FromCSL<csl::Withdrawals> for AssocMap<StakingCredential, BigInt> {
    fn from_csl(wdrls: &csl::Withdrawals) -> Self {
        let keys = wdrls.keys();

        (0..keys.len())
            .map(|i| {
                let key = keys.get(i);
                let value = wdrls.get(&key).unwrap();
                let key: StakingCredential = key.to_pla();
                let value: BigInt = value.to_pla();
                (key, value)
            })
            .collect::<Vec<_>>()
            .into()
    }
}

// TODO(szg251): change to FromCSL after Conway support is complete
impl TryFromCSL<csl::Certificate> for DCert {
    fn try_from_csl(value: &csl::Certificate) -> Result<Self, TryFromCSLError> {
        match value.kind() {
            csl::CertificateKind::StakeRegistration => {
                let stk_reg = value.as_stake_registration().unwrap();
                Ok(Self::DelegRegKey(stk_reg.stake_credential().to_pla()))
            }
            csl::CertificateKind::StakeDeregistration => {
                let stk_dereg = value.as_stake_deregistration().unwrap();
                Ok(Self::DelegDeRegKey(stk_dereg.stake_credential().to_pla()))
            }
            csl::CertificateKind::StakeDelegation => {
                let stk_deleg = value.as_stake_delegation().unwrap();
                Ok(Self::DelegDelegate(
                    stk_deleg.stake_credential().to_pla(),
                    PaymentPubKeyHash(stk_deleg.pool_keyhash().to_pla()),
                ))
            }
            csl::CertificateKind::PoolRegistration => {
                let pool_reg = value.as_pool_registration().unwrap();
                let params = pool_reg.pool_params();
                Ok(Self::PoolRegister(
                    PaymentPubKeyHash(params.operator().to_pla()),
                    PaymentPubKeyHash(params.vrf_keyhash().to_pla()),
                ))
            }
            csl::CertificateKind::PoolRetirement => {
                let pool_ret = value.as_pool_retirement().unwrap();
                Ok(Self::PoolRetire(
                    PaymentPubKeyHash(pool_ret.pool_keyhash().to_pla()),
                    pool_ret.epoch().into(),
                ))
            }
            csl::CertificateKind::GenesisKeyDelegation => Ok(Self::Genesis),
            csl::CertificateKind::MoveInstantaneousRewardsCert => Ok(Self::Mir),
            csl::CertificateKind::CommitteeHotAuth => Err(TryFromCSLError::Unsupported(
                "Conway certificate kind".to_string(),
            )),
            csl::CertificateKind::CommitteeColdResign => Err(TryFromCSLError::Unsupported(
                "Conway certificate kind".to_string(),
            )),
            csl::CertificateKind::DRepDeregistration => Err(TryFromCSLError::Unsupported(
                "Conway certificate kind".to_string(),
            )),
            csl::CertificateKind::DRepRegistration => Err(TryFromCSLError::Unsupported(
                "Conway certificate kind".to_string(),
            )),
            csl::CertificateKind::DRepUpdate => Err(TryFromCSLError::Unsupported(
                "Conway certificate kind".to_string(),
            )),
            csl::CertificateKind::StakeAndVoteDelegation => Err(TryFromCSLError::Unsupported(
                "Conway certificate kind".to_string(),
            )),
            csl::CertificateKind::StakeRegistrationAndDelegation => Err(
                TryFromCSLError::Unsupported("Conway certificate kind".to_string()),
            ),
            csl::CertificateKind::StakeVoteRegistrationAndDelegation => Err(
                TryFromCSLError::Unsupported("Conway certificate kind".to_string()),
            ),
            csl::CertificateKind::VoteDelegation => Err(TryFromCSLError::Unsupported(
                "Conway certificate kind".to_string(),
            )),
            csl::CertificateKind::VoteRegistrationAndDelegation => Err(
                TryFromCSLError::Unsupported("Conway certificate kind".to_string()),
            ),
        }
    }
}

// TODO(szg251): change to FromCSL after Conway support is complete
impl TryFromCSL<csl::Certificates> for Vec<DCert> {
    fn try_from_csl(value: &csl::Certificates) -> Result<Self, TryFromCSLError> {
        (0..value.len())
            .map(|idx| value.get(idx).try_to_pla())
            .collect()
    }
}

impl FromCSL<csl::VRFKeyHash> for Ed25519PubKeyHash {
    fn from_csl(value: &csl::VRFKeyHash) -> Self {
        Self(LedgerBytes(value.to_bytes()))
    }
}

impl TryFromCSL<csl::PlutusList> for AssocMap<DatumHash, Datum> {
    fn try_from_csl(value: &csl::PlutusList) -> Result<Self, TryFromCSLError> {
        (0..value.len())
            .map(|idx| {
                let datum = value.get(idx);
                let hash = csl::hash_plutus_data(&datum);
                (hash, datum)
            })
            .map(|(hash, datum)| -> Result<_, TryFromCSLError> {
                let hash: DatumHash = hash.to_pla();
                let datum: PlutusData = datum.try_to_pla()?;
                let datum = Datum(datum);
                Ok((hash, datum))
            })
            .collect::<Result<Vec<_>, _>>()
            .map(AssocMap::from)
    }
}

impl
    TryFromCSL<(
        &csl::TransactionInputs,
        &csl::Mint,
        &csl::Withdrawals,
        &csl::Certificates,
        &csl::Redeemers,
    )> for AssocMap<ScriptPurpose, Redeemer>
{
    fn try_from_csl(
        (inputs, mints, wdrls, certs, redeemers): &(
            &csl::TransactionInputs,
            &csl::Mint,
            &csl::Withdrawals,
            &csl::Certificates,
            &csl::Redeemers,
        ),
    ) -> Result<Self, TryFromCSLError> {
        let mint_hashes = mints.keys();
        let wdrl_addresses = wdrls.keys();

        (0..redeemers.len())
            .map(|idx| {
                let redeemer = redeemers.get(idx);

                let index: u64 = redeemer.index().into();
                let index = index as usize;

                let purpose = match redeemer.tag().kind() {
                    csl::RedeemerTagKind::Spend => {
                        let input = inputs.get(index);
                        Ok(ScriptPurpose::Spending(input.to_pla()))
                    }
                    csl::RedeemerTagKind::Mint => {
                        let hash = mint_hashes.get(idx);
                        Ok(ScriptPurpose::Minting(CurrencySymbol::NativeToken(
                            hash.to_pla(),
                        )))
                    }
                    csl::RedeemerTagKind::Cert => {
                        let cert = certs.get(idx);
                        Ok(ScriptPurpose::Certifying(cert.try_to_pla()?))
                    }
                    csl::RedeemerTagKind::Reward => {
                        let wdrl = wdrl_addresses.get(idx);
                        Ok(ScriptPurpose::Rewarding(wdrl.to_pla()))
                    }
                    csl::RedeemerTagKind::Vote => Err(TryFromCSLError::Unsupported(
                        "Conway redeemer tag kind".to_string(),
                    )),
                    csl::RedeemerTagKind::VotingProposal => Err(TryFromCSLError::Unsupported(
                        "Conway redeemer tag kind".to_string(),
                    )),
                }?;

                let redeemer = Redeemer(redeemer.data().try_to_pla()?);

                Ok((purpose, redeemer))
            })
            .collect::<Result<Vec<_>, TryFromCSLError>>()
            .map(AssocMap::from)
    }
}

impl
    TryFromCSLWith<(
        &Option<csl::BigNum>, // validity_start
        &Option<csl::BigNum>, // ttl
    )> for POSIXTimeRange
{
    type ExtraInfo<'a> = (
        &'a Vec<EraSummary>, // era_summaries
        &'a DateTime<Utc>,   // sys_start
    );

    fn try_from_csl_with(
        (start, ttl): &(
            &Option<csl::BigNum>, // validity_start
            &Option<csl::BigNum>, // ttl
        ),
        (era_summaries, sys_start): Self::ExtraInfo<'_>,
    ) -> Result<Self, TryFromCSLError> {
        let end = start
            .zip(ttl.as_ref())
            .map(|(start, ttl)| start.checked_add(ttl).unwrap());

        let slot_to_time = |s: csl::BigNum| {
            slot_into_posix_time(era_summaries, sys_start, s.into())
                .map_err(|err| TryFromCSLError::InvalidSlot(err.to_string()))
        };

        let start = start.map(slot_to_time).transpose()?;
        let end = end.map(slot_to_time).transpose()?;

        let lower_bound = LowerBound {
            closed: start.is_some(),
            bound: match start {
                Some(t) => Extended::Finite(t),
                None => Extended::NegInf,
            },
        };
        let upper_bound = UpperBound {
            bound: match end {
                Some(t) => Extended::Finite(t),
                None => Extended::PosInf,
            },
            closed: false,
        };

        Ok(Self {
            from: lower_bound,
            to: upper_bound,
        })
    }
}

impl TryFromCSLWith<csl::Transaction> for TransactionInfo {
    fn try_from_csl_with(
        tx: &csl::Transaction,
        extra_info: Self::ExtraInfo<'_>,
    ) -> Result<Self, TryFromCSLError> {
        let body = tx.body();
        let witness_set = tx.witness_set();

        let inputs = body.inputs();
        let mint = body.mint().unwrap_or(csl::Mint::new());
        let wdrls = body.withdrawals().unwrap_or(csl::Withdrawals::new());
        let certs = body.certs().unwrap_or(csl::Certificates::new());

        let redeemers = witness_set.redeemers().unwrap_or(csl::Redeemers::new());
        let datums = witness_set.plutus_data().unwrap_or(csl::PlutusList::new());

        Ok(Self {
            inputs: inputs.try_to_pla_with(extra_info.0)?,
            reference_inputs: body.inputs().try_to_pla_with(extra_info.0)?,
            outputs: body.outputs().try_to_pla()?,
            fee: Value::ada_value(&body.fee().to_pla()),
            mint: mint.to_pla(),
            d_cert: certs.try_to_pla()?,
            wdrl: wdrls.to_pla(),
            valid_range: (&body.validity_start_interval_bignum(), &body.ttl_bignum())
                .try_to_pla_with((extra_info.1, extra_info.2))?,
            signatories: {
                let vec: Vec<Ed25519PubKeyHash> = body
                    .required_signers()
                    .unwrap_or(csl::Ed25519KeyHashes::new())
                    .to_pla();

                vec.into_iter().map(PaymentPubKeyHash).collect()
            },
            redeemers: (&inputs, &mint, &wdrls, &certs, &redeemers).try_to_pla()?,
            datums: datums.try_to_pla()?,
            id: csl::hash_transaction(&body).to_pla(),
        })
    }

    type ExtraInfo<'a> = (
        &'a BTreeMap<TransactionInput, TransactionOutput>,
        &'a Vec<EraSummary>, // era_summaries
        &'a DateTime<Utc>,   // sys_start
    );
}

impl TryFromCSLWith<csl::TransactionInputs> for Vec<TxInInfo> {
    type ExtraInfo<'a> = &'a BTreeMap<TransactionInput, TransactionOutput>;

    fn try_from_csl_with(
        value: &csl::TransactionInputs,
        available_utxos: Self::ExtraInfo<'_>,
    ) -> Result<Self, TryFromCSLError> {
        let inputs: Vec<TransactionInput> = value.to_pla();

        inputs
            .into_iter()
            .map(|input| {
                available_utxos
                    .get(&input)
                    .ok_or(TryFromCSLError::UtxoNotFound(input.clone()))
                    .map(|output| TxInInfo {
                        reference: input,
                        output: output.clone(),
                    })
            })
            .collect()
    }
}
