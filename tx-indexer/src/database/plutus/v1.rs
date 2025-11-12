use num_bigint::BigInt;
use pla::csl::pla_to_csl::TryToCSL;
use plutus_ledger_api as pla;
use plutus_ledger_api::csl::{csl_to_pla::TryToPLA, lib as csl};

use crate::database::plutus::db_types::*;

//////////////////////
// CurrencySymbol
//////////////////////

impl From<pla::v1::value::CurrencySymbol> for CurrencySymbol {
    fn from(item: pla::v1::value::CurrencySymbol) -> Self {
        match item {
            pla::v1::value::CurrencySymbol::Ada => CurrencySymbol(Vec::with_capacity(0)),
            pla::v1::value::CurrencySymbol::NativeToken(pla::v1::script::MintingPolicyHash(
                pla::v1::script::ScriptHash(pla::v1::crypto::LedgerBytes(bytes)),
            )) => CurrencySymbol(bytes),
        }
    }
}

impl From<CurrencySymbol> for pla::v1::value::CurrencySymbol {
    fn from(item: CurrencySymbol) -> Self {
        let CurrencySymbol(bytes) = item;
        if bytes.is_empty() {
            pla::v1::value::CurrencySymbol::Ada
        } else {
            pla::v1::value::CurrencySymbol::NativeToken(pla::v1::script::MintingPolicyHash(
                pla::v1::script::ScriptHash(pla::v1::crypto::LedgerBytes(bytes)),
            ))
        }
    }
}

//////////////////////
// TokenName
//////////////////////

impl From<pla::v1::value::TokenName> for TokenName {
    fn from(item: pla::v1::value::TokenName) -> Self {
        TokenName(item.0 .0)
    }
}

impl From<TokenName> for pla::v1::value::TokenName {
    fn from(item: TokenName) -> Self {
        pla::v1::value::TokenName(pla::v1::crypto::LedgerBytes(item.0))
    }
}

//////////////////////
// TransactionHash
//////////////////////

impl From<pla::v1::transaction::TransactionHash> for TransactionHash {
    fn from(item: pla::v1::transaction::TransactionHash) -> Self {
        TransactionHash(item.0.into())
    }
}

impl From<TransactionHash> for pla::v1::transaction::TransactionHash {
    fn from(item: TransactionHash) -> Self {
        pla::v1::transaction::TransactionHash(item.0.into())
    }
}

//////////////////////
// Ed25519PubKeyHash
//////////////////////

impl From<pla::v1::crypto::Ed25519PubKeyHash> for Ed25519PubKeyHash {
    fn from(item: pla::v1::crypto::Ed25519PubKeyHash) -> Self {
        Ed25519PubKeyHash(item.0.into())
    }
}

impl From<Ed25519PubKeyHash> for pla::v1::crypto::Ed25519PubKeyHash {
    fn from(item: Ed25519PubKeyHash) -> Self {
        pla::v1::crypto::Ed25519PubKeyHash(item.0.into())
    }
}

//////////////////////
// ScriptHash
//////////////////////

impl From<pla::v1::script::ScriptHash> for ScriptHash {
    fn from(item: pla::v1::script::ScriptHash) -> Self {
        ScriptHash(item.0.into())
    }
}

impl From<ScriptHash> for pla::v1::script::ScriptHash {
    fn from(item: ScriptHash) -> Self {
        pla::v1::script::ScriptHash(item.0.into())
    }
}

//////////////////////
// DatumHash
//////////////////////

impl From<pla::v1::datum::DatumHash> for DatumHash {
    fn from(item: pla::v1::datum::DatumHash) -> Self {
        DatumHash(item.0.into())
    }
}

impl From<DatumHash> for pla::v1::datum::DatumHash {
    fn from(item: DatumHash) -> Self {
        pla::v1::datum::DatumHash(item.0.into())
    }
}

//////////////////////
// PlutusData
//////////////////////

impl TryFrom<pla::plutus_data::PlutusData> for PlutusData {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::plutus_data::PlutusData) -> Result<Self, Self::Error> {
        Ok(PlutusData(
            csl::decode_plutus_datum_to_json_value(
                &item
                    .try_to_csl()
                    .map_err(PlutusDataEncodingError::TryFromPLAError)?,
                csl::PlutusDatumSchema::DetailedSchema,
            )
            .map_err(PlutusDataEncodingError::CSLConversionError)?,
        ))
    }
}

impl TryFrom<PlutusData> for pla::plutus_data::PlutusData {
    type Error = DBTypeConversionError;

    fn try_from(item: PlutusData) -> Result<Self, Self::Error> {
        Ok(
            csl::encode_json_value_to_plutus_datum(item.0, csl::PlutusDatumSchema::DetailedSchema)
                .map_err(PlutusDataEncodingError::CSLConversionError)?
                .try_to_pla()
                .map_err(PlutusDataEncodingError::TryFromCSLError)?,
        )
    }
}

//////////////////////
// Credential
//////////////////////

impl From<pla::v1::address::Credential> for Credential {
    fn from(item: pla::v1::address::Credential) -> Self {
        match item {
            pla::v1::address::Credential::PubKey(pkh) => Credential {
                pub_key_hash: Some(pkh.into()),
                script_hash: None,
            },
            pla::v1::address::Credential::Script(pla::v1::script::ValidatorHash(sh)) => {
                Credential {
                    pub_key_hash: None,
                    script_hash: Some(sh.into()),
                }
            }
        }
    }
}

impl TryFrom<Credential> for pla::v1::address::Credential {
    type Error = DBTypeConversionError;

    fn try_from(item: Credential) -> Result<Self, Self::Error> {
        Ok(match item {
            Credential {
                pub_key_hash: Some(pkh_db),
                script_hash: None,
            } => pla::v1::address::Credential::PubKey(pkh_db.into()),
            Credential {
                pub_key_hash: None,
                script_hash: Some(sh_db),
            } => pla::v1::address::Credential::Script(pla::v1::script::ValidatorHash(sh_db.into())),
            _ => Err(DBTypeConversionError::InvariantBroken(
                "DB Credential must have either 'pub_key_hash' or 'script_hash'".to_string(),
            ))?,
        })
    }
}

//////////////////////
// ChainPointer
//////////////////////

impl TryFrom<pla::v1::address::ChainPointer> for ChainPointer {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v1::address::ChainPointer) -> Result<Self, Self::Error> {
        Ok(ChainPointer {
            slot_num: item
                .slot_number
                .0
                .try_into()
                .map_err(DBTypeConversionError::BigIntConversion)?,
            tx_idx: item
                .transaction_index
                .0
                .try_into()
                .map_err(DBTypeConversionError::BigIntConversion)?,
            cert_idx: item
                .certificate_index
                .0
                .try_into()
                .map_err(DBTypeConversionError::BigIntConversion)?,
        })
    }
}

impl From<ChainPointer> for pla::v1::address::ChainPointer {
    fn from(item: ChainPointer) -> Self {
        pla::v1::address::ChainPointer {
            slot_number: pla::v1::address::Slot(BigInt::from(item.slot_num)),
            transaction_index: pla::v1::address::TransactionIndex(BigInt::from(item.tx_idx)),
            certificate_index: pla::v1::address::CertificateIndex(BigInt::from(item.cert_idx)),
        }
    }
}

//////////////////////
// StakingCredential
//////////////////////

impl TryFrom<pla::v1::address::StakingCredential> for StakingCredential {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v1::address::StakingCredential) -> Result<Self, Self::Error> {
        Ok(match item {
            pla::v1::address::StakingCredential::Hash(cred) => StakingCredential {
                staking_hash: Some(cred.into()),
                staking_ptr: None,
            },
            pla::v1::address::StakingCredential::Pointer(ptr) => StakingCredential {
                staking_hash: None,
                staking_ptr: Some(ptr.try_into()?),
            },
        })
    }
}

impl TryFrom<StakingCredential> for pla::v1::address::StakingCredential {
    type Error = DBTypeConversionError;

    fn try_from(item: StakingCredential) -> Result<Self, Self::Error> {
        Ok(match item {
            StakingCredential {
                staking_hash: Some(cred),
                staking_ptr: None,
            } => pla::v1::address::StakingCredential::Hash(cred.try_into()?),
            StakingCredential {
                staking_hash: None,
                staking_ptr: Some(ptr),
            } => pla::v1::address::StakingCredential::Pointer(ptr.into()),

            _ => Err(DBTypeConversionError::InvariantBroken(
                "DB StakingCredential must have either 'staking_hash' or 'staking_ptr'".to_string(),
            ))?,
        })
    }
}

//////////////////////
// Address
//////////////////////

impl TryFrom<pla::v1::address::Address> for Address {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v1::address::Address) -> Result<Self, Self::Error> {
        Ok(Address {
            credential: item.credential.into(),
            staking_credential: item
                .staking_credential
                .map(StakingCredential::try_from)
                .transpose()?,
        })
    }
}

impl TryFrom<Address> for pla::v1::address::Address {
    type Error = DBTypeConversionError;

    fn try_from(item: Address) -> Result<Self, Self::Error> {
        Ok(pla::v1::address::Address {
            credential: item.credential.try_into()?,
            staking_credential: item
                .staking_credential
                .map(pla::v1::address::StakingCredential::try_from)
                .transpose()?,
        })
    }
}

//////////////////////
// AssetQuantity
//////////////////////

impl
    TryFrom<(
        pla::v1::value::CurrencySymbol,
        pla::v1::value::TokenName,
        BigInt,
    )> for AssetQuantity
{
    type Error = DBTypeConversionError;

    fn try_from(
        item: (
            pla::v1::value::CurrencySymbol,
            pla::v1::value::TokenName,
            BigInt,
        ),
    ) -> Result<Self, Self::Error> {
        Ok(AssetQuantity {
            currency_symbol: item.0.into(),
            token_name: item.1.into(),
            amount: item
                .2
                .try_into()
                .map_err(DBTypeConversionError::BigIntConversion)?,
        })
    }
}

impl From<AssetQuantity>
    for (
        pla::v1::value::CurrencySymbol,
        pla::v1::value::TokenName,
        BigInt,
    )
{
    fn from(item: AssetQuantity) -> Self {
        (
            item.currency_symbol.into(),
            item.token_name.into(),
            item.amount.into(),
        )
    }
}

//////////////////////
// Value
//////////////////////

impl TryFrom<pla::v1::value::Value> for Value {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v1::value::Value) -> Result<Self, Self::Error> {
        let assets = item
            .0
            .iter()
            .flat_map(|(cs, assets)| {
                assets
                    .iter()
                    .map(|(tn, amount)| {
                        AssetQuantity::try_from((cs.to_owned(), tn.to_owned(), amount.to_owned()))
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Result<Vec<AssetQuantity>, DBTypeConversionError>>()?;

        Ok(Value(assets))
    }
}

impl From<Value> for pla::v1::value::Value {
    fn from(item: Value) -> Self {
        item.0.into_iter().fold(
            pla::v1::value::Value::new(),
            |value,
             AssetQuantity {
                 currency_symbol,
                 token_name,
                 amount,
             }| {
                value.insert_token(&currency_symbol.into(), &token_name.into(), &amount.into())
            },
        )
    }
}

//////////////////////
// TransactionInput
//////////////////////

impl TryFrom<pla::v1::transaction::TransactionInput> for TransactionInput {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v1::transaction::TransactionInput) -> Result<Self, Self::Error> {
        Ok(TransactionInput {
            tx_id: item.transaction_id.into(),
            tx_idx: item
                .index
                .try_into()
                .map_err(DBTypeConversionError::BigIntConversion)?,
        })
    }
}

impl From<TransactionInput> for pla::v1::transaction::TransactionInput {
    fn from(item: TransactionInput) -> Self {
        pla::v1::transaction::TransactionInput {
            transaction_id: item.tx_id.into(),
            index: item.tx_idx.into(),
        }
    }
}

impl TryFrom<Option<pla::v1::datum::DatumHash>> for OutputDatum {
    type Error = DBTypeConversionError;

    fn try_from(item: Option<pla::v1::datum::DatumHash>) -> Result<Self, Self::Error> {
        Ok(match item {
            Some(dh) => OutputDatum {
                datum_hash: Some(dh.into()),
                inline_datum: None,
            },
            None => OutputDatum {
                datum_hash: None,
                inline_datum: None,
            },
        })
    }
}

impl TryFrom<OutputDatum> for Option<pla::v1::datum::DatumHash> {
    type Error = DBTypeConversionError;

    fn try_from(item: OutputDatum) -> Result<Self, Self::Error> {
        Ok(match item {
            OutputDatum {
                datum_hash: Some(dh_db),
                ..
            } => Some(dh_db.into()),
            OutputDatum {
                inline_datum: Some(d),
                ..
            } => Err(DBTypeConversionError::InvariantBroken(format!(
                "Unexpected inline datum for v1 transaction: {d:?}"
            )))?,
            _ => None,
        })
    }
}

//////////////////////
// TransactionOutput
//////////////////////

impl TryFrom<pla::v1::transaction::TransactionOutput> for TransactionOutput {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v1::transaction::TransactionOutput) -> Result<Self, Self::Error> {
        Ok(TransactionOutput {
            address: item.address.try_into()?,
            assets: item.value.try_into()?,
            datum: item.datum_hash.try_into()?,
            reference_script: None,
        })
    }
}

impl TryFrom<TransactionOutput> for pla::v1::transaction::TransactionOutput {
    type Error = DBTypeConversionError;

    fn try_from(item: TransactionOutput) -> Result<Self, Self::Error> {
        Ok(pla::v1::transaction::TransactionOutput {
            address: item.address.try_into()?,
            value: item.assets.into(),
            datum_hash: item.datum.try_into()?,
        })
    }
}

//////////////////////
// TxInInfo
//////////////////////

impl TryFrom<pla::v1::transaction::TxInInfo> for TxInInfo {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v1::transaction::TxInInfo) -> Result<Self, Self::Error> {
        Ok(TxInInfo {
            reference: item.reference.try_into()?,
            output: item.output.try_into()?,
        })
    }
}

impl TryFrom<TxInInfo> for pla::v1::transaction::TxInInfo {
    type Error = DBTypeConversionError;

    fn try_from(item: TxInInfo) -> Result<Self, Self::Error> {
        Ok(pla::v1::transaction::TxInInfo {
            reference: item.reference.into(),
            output: item.output.try_into()?,
        })
    }
}
