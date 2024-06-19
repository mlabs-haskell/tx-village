use cardano_serialization_lib as csl;
use num_bigint::BigInt;
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
        transaction::{TransactionHash, TransactionInput, TransactionOutput, TxInInfo},
        value::{CurrencySymbol, TokenName, Value},
    },
};
use thiserror::Error;
use tx_bakery::utils::{
    csl_to_pla::{TryFromCSLError, TryToPLA},
    pla_to_csl::{TryFromPLAError, TryToCSLWithDef},
};

#[derive(Error, Debug)]
pub enum DBTypeConversionError {
    #[error("Couldn't parse DB type, because some invariants weren't valid: {0}")]
    InvariantBroken(String),

    #[error("Cannot represent BigInt as PostgreSQL BIGINT type: {0}")]
    BigIntConversion(num_bigint::TryFromBigIntError<BigInt>),

    #[error(transparent)]
    PlutusDataEncodingError(#[from] PlutusDataEncodingError),
}

//////////////////////
/// CurrencySymbol
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.CurrencySymbol")]
pub struct CurrencySymbolDB(pub Vec<u8>);

impl From<CurrencySymbol> for CurrencySymbolDB {
    fn from(item: CurrencySymbol) -> Self {
        match item {
            CurrencySymbol::Ada => CurrencySymbolDB(Vec::with_capacity(0)),
            CurrencySymbol::NativeToken(MintingPolicyHash(ScriptHash(LedgerBytes(bytes)))) => {
                CurrencySymbolDB(bytes)
            }
        }
    }
}

impl From<CurrencySymbolDB> for CurrencySymbol {
    fn from(item: CurrencySymbolDB) -> Self {
        let CurrencySymbolDB(bytes) = item;
        if bytes.is_empty() {
            CurrencySymbol::Ada
        } else {
            CurrencySymbol::NativeToken(MintingPolicyHash(ScriptHash(LedgerBytes(bytes))))
        }
    }
}

//////////////////////
/// TokenName
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.TokenName")]
pub struct TokenNameDB(pub Vec<u8>);

impl From<TokenName> for TokenNameDB {
    fn from(item: TokenName) -> Self {
        TokenNameDB(item.0 .0)
    }
}

impl From<TokenNameDB> for TokenName {
    fn from(item: TokenNameDB) -> Self {
        TokenName(LedgerBytes(item.0))
    }
}

//////////////////////
/// TxId
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.TxId")]
pub struct TransactionHashDB(pub Vec<u8>);

impl From<TransactionHash> for TransactionHashDB {
    fn from(item: TransactionHash) -> Self {
        TransactionHashDB(item.0 .0)
    }
}

impl From<TransactionHashDB> for TransactionHash {
    fn from(item: TransactionHashDB) -> Self {
        TransactionHash(LedgerBytes(item.0))
    }
}

//////////////////////
/// PubKeyHash
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.PubKeyHash")]
pub struct Ed25519PubKeyHashDB(pub Vec<u8>);

impl From<Ed25519PubKeyHash> for Ed25519PubKeyHashDB {
    fn from(item: Ed25519PubKeyHash) -> Self {
        Ed25519PubKeyHashDB(item.0 .0)
    }
}

impl From<Ed25519PubKeyHashDB> for Ed25519PubKeyHash {
    fn from(item: Ed25519PubKeyHashDB) -> Self {
        Ed25519PubKeyHash(LedgerBytes(item.0))
    }
}

//////////////////////
/// ScriptHash
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.ScriptHash")]
pub struct ScriptHashDB(pub Vec<u8>);

impl From<ScriptHash> for ScriptHashDB {
    fn from(item: ScriptHash) -> Self {
        ScriptHashDB(item.0 .0)
    }
}

impl From<ScriptHashDB> for ScriptHash {
    fn from(item: ScriptHashDB) -> Self {
        ScriptHash(LedgerBytes(item.0))
    }
}

//////////////////////
/// DatumHash
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.DatumHash")]
pub struct DatumHashDB(pub Vec<u8>);

impl From<DatumHash> for DatumHashDB {
    fn from(item: DatumHash) -> Self {
        DatumHashDB(item.0 .0)
    }
}

impl From<DatumHashDB> for DatumHash {
    fn from(item: DatumHashDB) -> Self {
        DatumHash(LedgerBytes(item.0))
    }
}

//////////////////////
/// Slot
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[sqlx(type_name = "Plutus.Slot")]
pub struct SlotDB(pub i64);

impl From<u64> for SlotDB {
    fn from(item: u64) -> Self {
        SlotDB(item as i64)
    }
}

impl From<&SlotDB> for u64 {
    fn from(item: &SlotDB) -> Self {
        item.0 as u64
    }
}

//////////////////////
/// PlutusData
//////////////////////

#[derive(Error, Debug)]
pub enum PlutusDataEncodingError {
    #[error(transparent)]
    CSLConversionError(#[from] csl::error::JsError),

    #[error(transparent)]
    TryFromPLAError(#[from] TryFromPLAError),

    #[error(transparent)]
    TryFromCSLError(#[from] TryFromCSLError),
}

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.PlutusData")]
pub struct PlutusDataDB(pub serde_json::Value);

impl TryFrom<PlutusData> for PlutusDataDB {
    type Error = DBTypeConversionError;

    fn try_from(item: PlutusData) -> Result<Self, Self::Error> {
        Ok(PlutusDataDB(
            csl::plutus::decode_plutus_datum_to_json_value(
                &item
                    .try_to_csl()
                    .map_err(PlutusDataEncodingError::TryFromPLAError)?,
                csl::plutus::PlutusDatumSchema::DetailedSchema,
            )
            .map_err(PlutusDataEncodingError::CSLConversionError)?,
        ))
    }
}

impl TryFrom<PlutusDataDB> for PlutusData {
    type Error = DBTypeConversionError;

    fn try_from(item: PlutusDataDB) -> Result<Self, Self::Error> {
        Ok(csl::plutus::encode_json_value_to_plutus_datum(
            item.0,
            csl::plutus::PlutusDatumSchema::DetailedSchema,
        )
        .map_err(PlutusDataEncodingError::CSLConversionError)?
        .try_to_pla()
        .map_err(PlutusDataEncodingError::TryFromCSLError)?)
    }
}

//////////////////////
/// Credential
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.Credential")]
pub struct CredentialDB {
    pub_key_hash: Option<Ed25519PubKeyHashDB>,
    script_hash: Option<ScriptHashDB>,
}

impl From<Credential> for CredentialDB {
    fn from(item: Credential) -> Self {
        match item {
            Credential::PubKey(pkh) => CredentialDB {
                pub_key_hash: Some(pkh.into()),
                script_hash: None,
            },
            Credential::Script(ValidatorHash(sh)) => CredentialDB {
                pub_key_hash: None,
                script_hash: Some(sh.into()),
            },
        }
    }
}

impl TryFrom<CredentialDB> for Credential {
    type Error = DBTypeConversionError;

    fn try_from(item: CredentialDB) -> Result<Self, Self::Error> {
        Ok(match item {
            CredentialDB {
                pub_key_hash: Some(pkh_db),
                ..
            } => Credential::PubKey(pkh_db.into()),
            CredentialDB {
                script_hash: Some(sh_db),
                ..
            } => Credential::Script(ValidatorHash(sh_db.into())),
            _ => Err(DBTypeConversionError::InvariantBroken(
                "DB Credential must have either 'pub_key_hash' or 'script_hash'".to_string(),
            ))?,
        })
    }
}

//////////////////////
/// StakingPtr
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.StakingPtr")]
pub struct ChainPointerDB {
    slot_num: i64,
    tx_idx: i64,
    cert_idx: i64,
}

impl TryFrom<ChainPointer> for ChainPointerDB {
    type Error = DBTypeConversionError;

    fn try_from(item: ChainPointer) -> Result<Self, Self::Error> {
        Ok(ChainPointerDB {
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

impl From<ChainPointerDB> for ChainPointer {
    fn from(item: ChainPointerDB) -> Self {
        ChainPointer {
            slot_number: Slot(BigInt::from(item.slot_num)),
            transaction_index: TransactionIndex(BigInt::from(item.tx_idx)),
            certificate_index: CertificateIndex(BigInt::from(item.cert_idx)),
        }
    }
}

//////////////////////
/// StakingCredential
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.StakingCredential")]
pub struct StakingCredentialDB {
    staking_hash: Option<CredentialDB>,
    staking_ptr: Option<ChainPointerDB>,
}

impl TryFrom<StakingCredential> for StakingCredentialDB {
    type Error = DBTypeConversionError;

    fn try_from(item: StakingCredential) -> Result<Self, Self::Error> {
        Ok(match item {
            StakingCredential::Hash(cred) => StakingCredentialDB {
                staking_hash: Some(cred.into()),
                staking_ptr: None,
            },
            StakingCredential::Pointer(ptr) => StakingCredentialDB {
                staking_hash: None,
                staking_ptr: Some(ptr.try_into()?),
            },
        })
    }
}

impl TryFrom<StakingCredentialDB> for StakingCredential {
    type Error = DBTypeConversionError;

    fn try_from(item: StakingCredentialDB) -> Result<Self, Self::Error> {
        Ok(match item {
            StakingCredentialDB {
                staking_hash: Some(cred),
                ..
            } => StakingCredential::Hash(cred.try_into()?),
            StakingCredentialDB {
                staking_ptr: Some(ptr),
                ..
            } => StakingCredential::Pointer(ptr.into()),

            _ => Err(DBTypeConversionError::InvariantBroken(
                "DB StakingCredential must have either 'staking_hash' or 'staking_ptr'".to_string(),
            ))?,
        })
    }
}

//////////////////////
/// Address
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.Address")]
pub struct AddressDB {
    credential: CredentialDB,
    staking_credential: Option<StakingCredentialDB>,
}

impl TryFrom<Address> for AddressDB {
    type Error = DBTypeConversionError;

    fn try_from(item: Address) -> Result<Self, Self::Error> {
        Ok(AddressDB {
            credential: item.credential.into(),
            staking_credential: item
                .staking_credential
                .map(StakingCredentialDB::try_from)
                .transpose()?,
        })
    }
}

impl TryFrom<AddressDB> for Address {
    type Error = DBTypeConversionError;

    fn try_from(item: AddressDB) -> Result<Self, Self::Error> {
        Ok(Address {
            credential: item.credential.try_into()?,
            staking_credential: item
                .staking_credential
                .map(StakingCredential::try_from)
                .transpose()?,
        })
    }
}

//////////////////////
/// AssetQuantity
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.AssetQuantity")]
pub struct AssetQuantityDB {
    currency_symbol: CurrencySymbolDB,
    token_name: TokenNameDB,
    amount: i64,
}

impl TryFrom<(CurrencySymbol, TokenName, BigInt)> for AssetQuantityDB {
    type Error = DBTypeConversionError;

    fn try_from(item: (CurrencySymbol, TokenName, BigInt)) -> Result<Self, Self::Error> {
        Ok(AssetQuantityDB {
            currency_symbol: item.0.into(),
            token_name: item.1.into(),
            amount: item
                .2
                .try_into()
                .map_err(DBTypeConversionError::BigIntConversion)?,
        })
    }
}

impl From<AssetQuantityDB> for (CurrencySymbol, TokenName, BigInt) {
    fn from(item: AssetQuantityDB) -> Self {
        (
            item.currency_symbol.into(),
            item.token_name.into(),
            item.amount.into(),
        )
    }
}

//////////////////////
/// Value
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.Value")]
pub struct ValueDB(pub Vec<AssetQuantityDB>);

impl TryFrom<Value> for ValueDB {
    type Error = DBTypeConversionError;

    fn try_from(item: Value) -> Result<Self, Self::Error> {
        let assets = item
            .0
            .iter()
            .flat_map(|(cs, assets)| {
                assets
                    .iter()
                    .map(|(tn, amount)| {
                        AssetQuantityDB::try_from((cs.to_owned(), tn.to_owned(), amount.to_owned()))
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Result<Vec<AssetQuantityDB>, DBTypeConversionError>>()?;

        Ok(ValueDB(assets))
    }
}

impl From<ValueDB> for Value {
    fn from(item: ValueDB) -> Self {
        item.0.into_iter().fold(
            Value::new(),
            |value,
             AssetQuantityDB {
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
/// TxOutRef
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.TxOutRef")]
pub struct TransactionInputDB {
    tx_id: TransactionHashDB,
    tx_idx: i64,
}

impl TryFrom<TransactionInput> for TransactionInputDB {
    type Error = DBTypeConversionError;

    fn try_from(item: TransactionInput) -> Result<Self, Self::Error> {
        Ok(TransactionInputDB {
            tx_id: item.transaction_id.into(),
            tx_idx: item
                .index
                .try_into()
                .map_err(DBTypeConversionError::BigIntConversion)?,
        })
    }
}

impl From<TransactionInputDB> for TransactionInput {
    fn from(item: TransactionInputDB) -> Self {
        TransactionInput {
            transaction_id: item.tx_id.into(),
            index: item.tx_idx.into(),
        }
    }
}

//////////////////////
/// OutputDatum
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.OutputDatum")]
pub struct OutputDatumDB {
    datum_hash: Option<DatumHashDB>,
    inline_datum: Option<PlutusDataDB>,
}

impl TryFrom<OutputDatum> for OutputDatumDB {
    type Error = DBTypeConversionError;

    fn try_from(item: OutputDatum) -> Result<Self, Self::Error> {
        Ok(match item {
            OutputDatum::DatumHash(dh) => OutputDatumDB {
                datum_hash: Some(dh.into()),
                inline_datum: None,
            },
            OutputDatum::InlineDatum(Datum(datum)) => OutputDatumDB {
                datum_hash: None,
                inline_datum: Some(datum.try_into()?),
            },
            OutputDatum::None => OutputDatumDB {
                datum_hash: None,
                inline_datum: None,
            },
        })
    }
}

impl TryFrom<OutputDatumDB> for OutputDatum {
    type Error = DBTypeConversionError;

    fn try_from(item: OutputDatumDB) -> Result<Self, Self::Error> {
        Ok(match item {
            OutputDatumDB {
                datum_hash: Some(dh_db),
                ..
            } => OutputDatum::DatumHash(dh_db.into()),
            OutputDatumDB {
                inline_datum: Some(datum_db),
                ..
            } => OutputDatum::InlineDatum(Datum(datum_db.try_into()?)),
            _ => OutputDatum::None,
        })
    }
}

//////////////////////
/// TxOut
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.TxOut")]
pub struct TransactionOutputDB {
    address: AddressDB,
    assets: ValueDB,
    datum: OutputDatumDB,
    reference_script: Option<ScriptHashDB>,
}

impl TryFrom<TransactionOutput> for TransactionOutputDB {
    type Error = DBTypeConversionError;

    fn try_from(item: TransactionOutput) -> Result<Self, Self::Error> {
        Ok(TransactionOutputDB {
            address: item.address.try_into()?,
            assets: item.value.try_into()?,
            datum: item.datum.try_into()?,
            reference_script: item.reference_script.map(ScriptHashDB::from),
        })
    }
}

impl TryFrom<TransactionOutputDB> for TransactionOutput {
    type Error = DBTypeConversionError;

    fn try_from(item: TransactionOutputDB) -> Result<Self, Self::Error> {
        Ok(TransactionOutput {
            address: item.address.try_into()?,
            value: item.assets.into(),
            datum: item.datum.try_into()?,
            reference_script: item.reference_script.map(ScriptHash::from),
        })
    }
}

//////////////////////
/// TxInInfo
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.TxOut")]
pub struct TxInInfoDB {
    reference: TransactionInputDB,
    output: TransactionOutputDB,
}

impl TryFrom<TxInInfo> for TxInInfoDB {
    type Error = DBTypeConversionError;

    fn try_from(item: TxInInfo) -> Result<Self, Self::Error> {
        Ok(TxInInfoDB {
            reference: item.reference.try_into()?,
            output: item.output.try_into()?,
        })
    }
}

impl TryFrom<TxInInfoDB> for TxInInfo {
    type Error = DBTypeConversionError;

    fn try_from(item: TxInInfoDB) -> Result<Self, Self::Error> {
        Ok(TxInInfo {
            reference: item.reference.into(),
            output: item.output.try_into()?,
        })
    }
}
