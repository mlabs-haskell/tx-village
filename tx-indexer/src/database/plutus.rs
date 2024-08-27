use num_bigint::BigInt;
use plutus_ledger_api as pla;
use thiserror::Error;
use tx_bakery::csl;
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
pub struct CurrencySymbol(pub Vec<u8>);

impl From<pla::v2::value::CurrencySymbol> for CurrencySymbol {
    fn from(item: pla::v2::value::CurrencySymbol) -> Self {
        match item {
            pla::v2::value::CurrencySymbol::Ada => CurrencySymbol(Vec::with_capacity(0)),
            pla::v2::value::CurrencySymbol::NativeToken(pla::v2::script::MintingPolicyHash(
                pla::v2::script::ScriptHash(pla::v2::crypto::LedgerBytes(bytes)),
            )) => CurrencySymbol(bytes),
        }
    }
}

impl From<CurrencySymbol> for pla::v2::value::CurrencySymbol {
    fn from(item: CurrencySymbol) -> Self {
        let CurrencySymbol(bytes) = item;
        if bytes.is_empty() {
            pla::v2::value::CurrencySymbol::Ada
        } else {
            pla::v2::value::CurrencySymbol::NativeToken(pla::v2::script::MintingPolicyHash(
                pla::v2::script::ScriptHash(pla::v2::crypto::LedgerBytes(bytes)),
            ))
        }
    }
}

//////////////////////
/// TokenName
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.TokenName")]
pub struct TokenName(pub Vec<u8>);

impl From<pla::v2::value::TokenName> for TokenName {
    fn from(item: pla::v2::value::TokenName) -> Self {
        TokenName(item.0 .0)
    }
}

impl From<TokenName> for pla::v2::value::TokenName {
    fn from(item: TokenName) -> Self {
        pla::v2::value::TokenName(pla::v2::crypto::LedgerBytes(item.0))
    }
}

//////////////////////
/// TransactionHash
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.TransactionHash")]
pub struct TransactionHash(pub Vec<u8>);

impl From<pla::v2::transaction::TransactionHash> for TransactionHash {
    fn from(item: pla::v2::transaction::TransactionHash) -> Self {
        TransactionHash(item.0 .0)
    }
}

impl From<TransactionHash> for pla::v2::transaction::TransactionHash {
    fn from(item: TransactionHash) -> Self {
        pla::v2::transaction::TransactionHash(pla::v2::crypto::LedgerBytes(item.0))
    }
}

//////////////////////
/// Ed25519PubKeyHash
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.Ed25519PubKeyHash")]
pub struct Ed25519PubKeyHash(pub Vec<u8>);

impl From<pla::v2::crypto::Ed25519PubKeyHash> for Ed25519PubKeyHash {
    fn from(item: pla::v2::crypto::Ed25519PubKeyHash) -> Self {
        Ed25519PubKeyHash(item.0 .0)
    }
}

impl From<Ed25519PubKeyHash> for pla::v2::crypto::Ed25519PubKeyHash {
    fn from(item: Ed25519PubKeyHash) -> Self {
        pla::v2::crypto::Ed25519PubKeyHash(pla::v2::crypto::LedgerBytes(item.0))
    }
}

//////////////////////
/// ScriptHash
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.ScriptHash")]
pub struct ScriptHash(pub Vec<u8>);

impl From<pla::v2::script::ScriptHash> for ScriptHash {
    fn from(item: pla::v2::script::ScriptHash) -> Self {
        ScriptHash(item.0 .0)
    }
}

impl From<ScriptHash> for pla::v2::script::ScriptHash {
    fn from(item: ScriptHash) -> Self {
        pla::v2::script::ScriptHash(pla::v2::crypto::LedgerBytes(item.0))
    }
}

//////////////////////
/// DatumHash
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.DatumHash")]
pub struct DatumHash(pub Vec<u8>);

impl From<pla::v2::datum::DatumHash> for DatumHash {
    fn from(item: pla::v2::datum::DatumHash) -> Self {
        DatumHash(item.0 .0)
    }
}

impl From<DatumHash> for pla::v2::datum::DatumHash {
    fn from(item: DatumHash) -> Self {
        pla::v2::datum::DatumHash(pla::v2::crypto::LedgerBytes(item.0))
    }
}

//////////////////////
/// Slot
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[sqlx(type_name = "Plutus.Slot")]
pub struct Slot(pub i64);

impl From<u64> for Slot {
    fn from(item: u64) -> Self {
        Slot(item as i64)
    }
}

impl From<&Slot> for u64 {
    fn from(item: &Slot) -> Self {
        item.0 as u64
    }
}

//////////////////////
/// PlutusData
//////////////////////

#[derive(Error, Debug)]
pub enum PlutusDataEncodingError {
    #[error(transparent)]
    CSLConversionError(#[from] csl::JsError),

    #[error(transparent)]
    TryFromPLAError(#[from] TryFromPLAError),

    #[error(transparent)]
    TryFromCSLError(#[from] TryFromCSLError),
}

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.PlutusData")]
pub struct PlutusData(pub serde_json::Value);

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
/// Credential
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.Credential")]
pub struct Credential {
    pub_key_hash: Option<Ed25519PubKeyHash>,
    script_hash: Option<ScriptHash>,
}

impl From<pla::v2::address::Credential> for Credential {
    fn from(item: pla::v2::address::Credential) -> Self {
        match item {
            pla::v2::address::Credential::PubKey(pkh) => Credential {
                pub_key_hash: Some(pkh.into()),
                script_hash: None,
            },
            pla::v2::address::Credential::Script(pla::v2::script::ValidatorHash(sh)) => {
                Credential {
                    pub_key_hash: None,
                    script_hash: Some(sh.into()),
                }
            }
        }
    }
}

impl TryFrom<Credential> for pla::v2::address::Credential {
    type Error = DBTypeConversionError;

    fn try_from(item: Credential) -> Result<Self, Self::Error> {
        Ok(match item {
            Credential {
                pub_key_hash: Some(pkh_db),
                script_hash: None,
            } => pla::v2::address::Credential::PubKey(pkh_db.into()),
            Credential {
                pub_key_hash: None,
                script_hash: Some(sh_db),
            } => pla::v2::address::Credential::Script(pla::v2::script::ValidatorHash(sh_db.into())),
            _ => Err(DBTypeConversionError::InvariantBroken(
                "DB Credential must have either 'pub_key_hash' or 'script_hash'".to_string(),
            ))?,
        })
    }
}

//////////////////////
/// ChainPointer
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.ChainPointer")]
pub struct ChainPointer {
    slot_num: i64,
    tx_idx: i64,
    cert_idx: i64,
}

impl TryFrom<pla::v2::address::ChainPointer> for ChainPointer {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v2::address::ChainPointer) -> Result<Self, Self::Error> {
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

impl From<ChainPointer> for pla::v2::address::ChainPointer {
    fn from(item: ChainPointer) -> Self {
        pla::v2::address::ChainPointer {
            slot_number: pla::v2::address::Slot(BigInt::from(item.slot_num)),
            transaction_index: pla::v2::address::TransactionIndex(BigInt::from(item.tx_idx)),
            certificate_index: pla::v2::address::CertificateIndex(BigInt::from(item.cert_idx)),
        }
    }
}

//////////////////////
/// StakingCredential
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.StakingCredential")]
pub struct StakingCredential {
    staking_hash: Option<Credential>,
    staking_ptr: Option<ChainPointer>,
}

impl TryFrom<pla::v2::address::StakingCredential> for StakingCredential {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v2::address::StakingCredential) -> Result<Self, Self::Error> {
        Ok(match item {
            pla::v2::address::StakingCredential::Hash(cred) => StakingCredential {
                staking_hash: Some(cred.into()),
                staking_ptr: None,
            },
            pla::v2::address::StakingCredential::Pointer(ptr) => StakingCredential {
                staking_hash: None,
                staking_ptr: Some(ptr.try_into()?),
            },
        })
    }
}

impl TryFrom<StakingCredential> for pla::v2::address::StakingCredential {
    type Error = DBTypeConversionError;

    fn try_from(item: StakingCredential) -> Result<Self, Self::Error> {
        Ok(match item {
            StakingCredential {
                staking_hash: Some(cred),
                staking_ptr: None,
            } => pla::v2::address::StakingCredential::Hash(cred.try_into()?),
            StakingCredential {
                staking_hash: None,
                staking_ptr: Some(ptr),
            } => pla::v2::address::StakingCredential::Pointer(ptr.into()),

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
pub struct Address {
    credential: Credential,
    staking_credential: Option<StakingCredential>,
}

impl TryFrom<pla::v2::address::Address> for Address {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v2::address::Address) -> Result<Self, Self::Error> {
        Ok(Address {
            credential: item.credential.into(),
            staking_credential: item
                .staking_credential
                .map(StakingCredential::try_from)
                .transpose()?,
        })
    }
}

impl TryFrom<Address> for pla::v2::address::Address {
    type Error = DBTypeConversionError;

    fn try_from(item: Address) -> Result<Self, Self::Error> {
        Ok(pla::v2::address::Address {
            credential: item.credential.try_into()?,
            staking_credential: item
                .staking_credential
                .map(pla::v2::address::StakingCredential::try_from)
                .transpose()?,
        })
    }
}

//////////////////////
/// AssetQuantity
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.AssetQuantity")]
pub struct AssetQuantity {
    currency_symbol: CurrencySymbol,
    token_name: TokenName,
    amount: i64,
}

impl
    TryFrom<(
        pla::v2::value::CurrencySymbol,
        pla::v2::value::TokenName,
        BigInt,
    )> for AssetQuantity
{
    type Error = DBTypeConversionError;

    fn try_from(
        item: (
            pla::v2::value::CurrencySymbol,
            pla::v2::value::TokenName,
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
        pla::v2::value::CurrencySymbol,
        pla::v2::value::TokenName,
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
/// Value
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.Value")]
pub struct Value(pub Vec<AssetQuantity>);

impl TryFrom<pla::v2::value::Value> for Value {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v2::value::Value) -> Result<Self, Self::Error> {
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

impl From<Value> for pla::v2::value::Value {
    fn from(item: Value) -> Self {
        item.0.into_iter().fold(
            pla::v2::value::Value::new(),
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
/// TransactionInput
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.TransactionInput")]
pub struct TransactionInput {
    tx_id: TransactionHash,
    tx_idx: i64,
}

impl TryFrom<pla::v2::transaction::TransactionInput> for TransactionInput {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v2::transaction::TransactionInput) -> Result<Self, Self::Error> {
        Ok(TransactionInput {
            tx_id: item.transaction_id.into(),
            tx_idx: item
                .index
                .try_into()
                .map_err(DBTypeConversionError::BigIntConversion)?,
        })
    }
}

impl From<TransactionInput> for pla::v2::transaction::TransactionInput {
    fn from(item: TransactionInput) -> Self {
        pla::v2::transaction::TransactionInput {
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
pub struct OutputDatum {
    datum_hash: Option<DatumHash>,
    inline_datum: Option<PlutusData>,
}

impl TryFrom<pla::v2::datum::OutputDatum> for OutputDatum {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v2::datum::OutputDatum) -> Result<Self, Self::Error> {
        Ok(match item {
            pla::v2::datum::OutputDatum::DatumHash(dh) => OutputDatum {
                datum_hash: Some(dh.into()),
                inline_datum: None,
            },
            pla::v2::datum::OutputDatum::InlineDatum(pla::v2::datum::Datum(datum)) => OutputDatum {
                datum_hash: None,
                inline_datum: Some(datum.try_into()?),
            },
            pla::v2::datum::OutputDatum::None => OutputDatum {
                datum_hash: None,
                inline_datum: None,
            },
        })
    }
}

impl TryFrom<OutputDatum> for pla::v2::datum::OutputDatum {
    type Error = DBTypeConversionError;

    fn try_from(item: OutputDatum) -> Result<Self, Self::Error> {
        Ok(match item {
            OutputDatum {
                datum_hash: Some(dh_db),
                ..
            } => pla::v2::datum::OutputDatum::DatumHash(dh_db.into()),
            OutputDatum {
                inline_datum: Some(datum_db),
                ..
            } => pla::v2::datum::OutputDatum::InlineDatum(pla::v2::datum::Datum(
                datum_db.try_into()?,
            )),
            _ => pla::v2::datum::OutputDatum::None,
        })
    }
}

//////////////////////
/// TransactionOutput
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.TransactionOutput")]
pub struct TransactionOutput {
    address: Address,
    assets: Value,
    datum: OutputDatum,
    reference_script: Option<ScriptHash>,
}

impl TryFrom<pla::v2::transaction::TransactionOutput> for TransactionOutput {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v2::transaction::TransactionOutput) -> Result<Self, Self::Error> {
        Ok(TransactionOutput {
            address: item.address.try_into()?,
            assets: item.value.try_into()?,
            datum: item.datum.try_into()?,
            reference_script: item.reference_script.map(ScriptHash::from),
        })
    }
}

impl TryFrom<TransactionOutput> for pla::v2::transaction::TransactionOutput {
    type Error = DBTypeConversionError;

    fn try_from(item: TransactionOutput) -> Result<Self, Self::Error> {
        Ok(pla::v2::transaction::TransactionOutput {
            address: item.address.try_into()?,
            value: item.assets.into(),
            datum: item.datum.try_into()?,
            reference_script: item.reference_script.map(pla::v2::script::ScriptHash::from),
        })
    }
}

//////////////////////
/// TxInInfo
//////////////////////

#[derive(sqlx::Type, Clone, Debug, PartialEq, Eq)]
#[sqlx(type_name = "Plutus.TxInInfo")]
pub struct TxInInfo {
    reference: TransactionInput,
    output: TransactionOutput,
}

impl TryFrom<pla::v2::transaction::TxInInfo> for TxInInfo {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v2::transaction::TxInInfo) -> Result<Self, Self::Error> {
        Ok(TxInInfo {
            reference: item.reference.try_into()?,
            output: item.output.try_into()?,
        })
    }
}

impl TryFrom<TxInInfo> for pla::v2::transaction::TxInInfo {
    type Error = DBTypeConversionError;

    fn try_from(item: TxInInfo) -> Result<Self, Self::Error> {
        Ok(pla::v2::transaction::TxInInfo {
            reference: item.reference.into(),
            output: item.output.try_into()?,
        })
    }
}
