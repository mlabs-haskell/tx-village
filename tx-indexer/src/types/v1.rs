use num_bigint::{BigInt, BigUint};
use pallas_primitives::{self as primitives, alonzo};
use pallas_traverse::ComputeHash;
use plutus_ledger_api::csl::csl_to_pla::{FromCSL, ToPLA, TryToPLA};
use plutus_ledger_api::csl::lib as csl;
use plutus_ledger_api::csl::pla_to_csl::TryFromPLA;
use plutus_ledger_api::plutus_data::PlutusData;
use plutus_ledger_api::{
    csl::csl_to_pla::TryFromCSL,
    v1::{
        self,
        address::{Credential, StakingCredential},
        assoc_map::AssocMap,
        crypto::{Ed25519PubKeyHash, LedgerBytes, PaymentPubKeyHash},
        datum::{Datum, DatumHash},
        interval::Interval,
        redeemer::Redeemer,
        script::{MintingPolicyHash, ScriptHash, ValidatorHash},
        transaction::{DCert, POSIXTimeRange, ScriptPurpose, TransactionHash, TransactionInput},
        value::{CurrencySymbol, TokenName, Value},
    },
};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tracing::error;

use crate::from_pallas::{FromPallas, FromPallasError};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Transaction {
    pub inputs: Vec<TransactionInput>,
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
    pub datum_hash: Option<DatumHash>,
}

#[derive(Debug, Error)]
pub enum BlockParseError {
    #[error("Byron addresses cannot be converted to Plutus Ledger API types.")]
    CannotConvertByronAddress,
}

impl TryFrom<TransactionOutput> for v1::transaction::TransactionOutput {
    type Error = BlockParseError;

    fn try_from(value: TransactionOutput) -> Result<Self, Self::Error> {
        Ok(v1::transaction::TransactionOutput {
            address: value.address.try_into()?,
            value: value.value,
            datum_hash: value.datum_hash,
        })
    }
}

/// Address container format compatible with Byron addresses
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MultiEraAddress {
    pub csl_addr: csl::Address,
    pub address: Option<v1::address::Address>,
}

impl MultiEraAddress {
    pub fn serialize(&self) -> String {
        if let Some(byron_addr) = csl::ByronAddress::from_address(&self.csl_addr) {
            byron_addr.to_base58()
        } else {
            self.csl_addr.to_bech32(None).unwrap()
        }
    }
}

impl TryFrom<MultiEraAddress> for v1::address::Address {
    type Error = BlockParseError;

    fn try_from(value: MultiEraAddress) -> Result<Self, Self::Error> {
        value
            .address
            .ok_or(BlockParseError::CannotConvertByronAddress)
    }
}

impl FromPallas<primitives::Bytes> for MultiEraAddress {
    fn from_pallas(value: primitives::Bytes) -> Result<Self, FromPallasError> {
        let csl_addr = csl::Address::from_bytes(value.to_vec())?;
        let address = v1::address::Address::try_from_csl(&csl_addr).ok();

        Ok(MultiEraAddress { csl_addr, address })
    }
}

impl From<csl::Address> for MultiEraAddress {
    fn from(value: csl::Address) -> Self {
        Self {
            address: value.try_to_pla().ok(),
            csl_addr: value,
        }
    }
}

impl FromPallas<(alonzo::TransactionBody, alonzo::WitnessSet)> for Transaction {
    fn from_pallas(
        (tx, witness_set): (alonzo::TransactionBody, alonzo::WitnessSet),
    ) -> Result<Self, FromPallasError> {
        let tx_id = tx.compute_hash();
        let inputs = tx
            .inputs
            .into_iter()
            .map(TransactionInput::from_pallas)
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

pub(crate) fn parse_and_match_script_purposes(
    redeemer: alonzo::Redeemer,
    inputs: &[TransactionInput],
    mint: &Option<
        alonzo::KeyValuePairs<primitives::Hash<28>, alonzo::KeyValuePairs<primitives::Bytes, i64>>,
    >,
    certificates: &[DCert],
    withdrawals: &AssocMap<StakingCredential, BigInt>,
) -> Result<(ScriptPurpose, Redeemer), FromPallasError> {
    let plutus_data = Redeemer(PlutusData::from_pallas(&redeemer.data)?);
    let idx = redeemer.index as usize;
    let script_purpose = match redeemer.tag {
        alonzo::RedeemerTag::Spend => ScriptPurpose::Spending(inputs[idx].clone()),
        alonzo::RedeemerTag::Mint => {
            ScriptPurpose::Minting(CurrencySymbol::from_pallas(mint.as_ref().unwrap()[idx].0)?)
        }
        alonzo::RedeemerTag::Cert => ScriptPurpose::Certifying(certificates[idx].clone()),
        alonzo::RedeemerTag::Reward => ScriptPurpose::Rewarding(withdrawals.0[idx].0.clone()),
    };

    Ok((script_purpose, plutus_data))
}

impl FromPallas<alonzo::TransactionInput> for TransactionInput {
    fn from_pallas(value: alonzo::TransactionInput) -> Result<Self, FromPallasError> {
        Ok(TransactionInput {
            transaction_id: TransactionHash(LedgerBytes(value.transaction_id.to_vec())),
            index: BigInt::from(value.index),
        })
    }
}

impl FromPallas<alonzo::TransactionOutput> for TransactionOutput {
    fn from_pallas(value: alonzo::TransactionOutput) -> Result<Self, FromPallasError> {
        Ok(TransactionOutput {
            address: MultiEraAddress::from_pallas(value.address)?,
            value: Value::from_pallas(value.amount)?,
            datum_hash: value.datum_hash.map(DatumHash::from_pallas).transpose()?,
        })
    }
}

impl FromPallas<alonzo::Value> for Value {
    fn from_pallas(value: alonzo::Value) -> Result<Self, FromPallasError> {
        match value {
            alonzo::Value::Coin(coin) => Ok(Value::ada_value(&BigInt::from(coin))),
            alonzo::Value::Multiasset(coin, multiasset) => {
                let mut val = Value::from_pallas(multiasset)?;
                val.insert_ada_mut(BigInt::from(coin));

                Ok(val)
            }
        }
    }
}

impl<Amount>
    FromPallas<
        primitives::KeyValuePairs<
            primitives::Hash<28>,
            primitives::KeyValuePairs<primitives::Bytes, Amount>,
        >,
    > for Value
where
    Amount: Into<BigInt> + Clone,
{
    fn from_pallas(
        value: primitives::KeyValuePairs<
            primitives::Hash<28>,
            primitives::KeyValuePairs<primitives::Bytes, Amount>,
        >,
    ) -> Result<Self, FromPallasError> {
        let mut val = Value::new();

        for (currency_symbol, inner_asset) in value.to_vec() {
            for (token_name, amount) in inner_asset.to_vec() {
                let currency_symbol = CurrencySymbol::from_pallas(currency_symbol)?;
                let token_name = TokenName::from_pallas(token_name)?;
                let amount = amount.into();

                val.insert_token_mut(currency_symbol, token_name, amount)
            }
        }

        Ok(val)
    }
}

impl FromPallas<primitives::Hash<28>> for CurrencySymbol {
    fn from_pallas(value: primitives::Hash<28>) -> Result<Self, FromPallasError> {
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

impl FromPallas<primitives::Bytes> for TokenName {
    fn from_pallas(value: primitives::Bytes) -> Result<Self, FromPallasError> {
        Ok(TokenName(LedgerBytes(value.to_vec())))
    }
}

impl FromPallas<primitives::Hash<32>> for DatumHash {
    fn from_pallas(value: alonzo::DatumHash) -> Result<Self, FromPallasError> {
        Ok(DatumHash(LedgerBytes(value.to_vec())))
    }
}

impl FromPallas<primitives::Hash<32>> for TransactionHash {
    fn from_pallas(value: primitives::Hash<32>) -> Result<Self, FromPallasError> {
        Ok(TransactionHash(LedgerBytes(value.to_vec())))
    }
}

impl FromPallas<&alonzo::PlutusData> for PlutusData {
    fn from_pallas(value: &alonzo::PlutusData) -> Result<Self, FromPallasError> {
        Ok(match value {
            alonzo::PlutusData::Array(array) => PlutusData::List(
                array
                    .iter()
                    .map(PlutusData::from_pallas)
                    .collect::<Result<_, _>>()?,
            ),
            alonzo::PlutusData::Constr(constr) => PlutusData::Constr(
                BigInt::from(constr.tag),
                constr
                    .fields
                    .iter()
                    .map(PlutusData::from_pallas)
                    .collect::<Result<_, _>>()?,
            ),
            alonzo::PlutusData::Map(key_value_pairs) => PlutusData::Map(
                key_value_pairs
                    .iter()
                    .map(|(key, val)| {
                        Ok::<(PlutusData, PlutusData), FromPallasError>((
                            PlutusData::from_pallas(key)?,
                            PlutusData::from_pallas(val)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            alonzo::PlutusData::BigInt(big_int) => PlutusData::Integer(match big_int {
                primitives::BigInt::Int(n) => BigInt::from(<i128>::from(n.0)),
                primitives::BigInt::BigUInt(uint) => BigUint::from_bytes_be(uint).into(),
                primitives::BigInt::BigNInt(nint) => BigInt::from_signed_bytes_be(nint),
            }),
            alonzo::PlutusData::BoundedBytes(bounded_bytes) => {
                PlutusData::Bytes(bounded_bytes.to_vec())
            }
        })
    }
}

impl FromPallas<alonzo::Certificate> for DCert {
    fn from_pallas(value: alonzo::Certificate) -> Result<Self, FromPallasError> {
        Ok(match value {
            alonzo::Certificate::StakeRegistration(stake_credential) => {
                DCert::DelegRegKey(StakingCredential::from_pallas(stake_credential)?)
            }
            alonzo::Certificate::StakeDeregistration(stake_credential) => {
                DCert::DelegDeRegKey(StakingCredential::from_pallas(stake_credential)?)
            }
            alonzo::Certificate::StakeDelegation(stake_credential, hash) => DCert::DelegDelegate(
                StakingCredential::from_pallas(stake_credential)?,
                PaymentPubKeyHash(Ed25519PubKeyHash(LedgerBytes(hash.to_vec()))),
            ),
            alonzo::Certificate::PoolRegistration {
                operator,
                vrf_keyhash,
                ..
            } => DCert::PoolRegister(
                PaymentPubKeyHash(Ed25519PubKeyHash(LedgerBytes(operator.to_vec()))),
                PaymentPubKeyHash(Ed25519PubKeyHash(LedgerBytes(vrf_keyhash.to_vec()))),
            ),
            alonzo::Certificate::PoolRetirement(hash, amount) => DCert::PoolRetire(
                PaymentPubKeyHash(Ed25519PubKeyHash(LedgerBytes(hash.to_vec()))),
                BigInt::from(amount),
            ),
            alonzo::Certificate::GenesisKeyDelegation(..) => DCert::Genesis,
            alonzo::Certificate::MoveInstantaneousRewardsCert(_) => DCert::Mir,
        })
    }
}

impl FromPallas<alonzo::Bytes> for Credential {
    fn from_pallas(value: alonzo::Bytes) -> Result<Self, FromPallasError> {
        // Parsing rules: https://cips.cardano.org/cip/CIP-19
        let mut bytes = value.to_vec();
        let header_byte = bytes
            .pop()
            .ok_or(FromPallasError::InvalidStakeAddressLength(hex::encode(
                &bytes,
            )))?;

        let header_type = (header_byte & 0b0001_0000) >> 4;

        match header_type {
            0 => Ok(Credential::PubKey(Ed25519PubKeyHash(LedgerBytes(bytes)))),
            1 => Ok(Credential::Script(ValidatorHash(ScriptHash(LedgerBytes(
                bytes,
            ))))),
            _ => Err(FromPallasError::InvalidStakeAddressHeaderByte(hex::encode(
                &[header_byte],
            ))),
        }
    }
}

impl FromPallas<alonzo::StakeCredential> for StakingCredential {
    fn from_pallas(value: alonzo::StakeCredential) -> Result<Self, FromPallasError> {
        Ok(match value {
            alonzo::StakeCredential::ScriptHash(hash) => StakingCredential::Hash(
                Credential::Script(ValidatorHash(ScriptHash(LedgerBytes(hash.to_vec())))),
            ),
            alonzo::StakeCredential::AddrKeyhash(hash) => StakingCredential::Hash(
                Credential::PubKey(Ed25519PubKeyHash(LedgerBytes(hash.to_vec()))),
            ),
        })
    }
}

impl FromPallas<alonzo::VKeyWitness> for Ed25519PubKeyHash {
    fn from_pallas(value: alonzo::VKeyWitness) -> Result<Self, FromPallasError> {
        Ok(csl::PublicKey::from_bytes(&value.vkey.to_vec())?
            .hash()
            .to_pla())
    }
}
