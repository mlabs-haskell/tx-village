#[cfg(feature = "diesel")]
use diesel::sql_types::{Array, Bytea, Nullable};
use num_bigint::BigInt;
use plutus_ledger_api::csl::{
    csl_to_pla::TryFromCSLError, lib as csl, pla_to_csl::TryFromPLAError,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DBTypeConversionError {
    #[error("Couldn't parse DB type, because some invariants weren't valid: {0}")]
    InvariantBroken(String),

    #[error("Cannot represent BigInt as PostgreSQL BIGINT type: {0}")]
    BigIntConversion(num_bigint::TryFromBigIntError<BigInt>),

    #[error(transparent)]
    PlutusDataEncodingError(#[from] PlutusDataEncodingError),
}

#[cfg(feature = "diesel")]
pub mod sql_types {
    //////////////////////
    // Cardano schema
    //////////////////////

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "hash28", schema = "cardano"))]
    pub struct Hash28;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "hash32", schema = "cardano"))]
    pub struct Hash32;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "slot", schema = "cardano"))]
    pub struct Slot;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "block_hash", schema = "cardano"))]
    pub struct BlockHash;

    //////////////////////
    // Plutus schema
    //////////////////////

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "address", schema = "plutus"))]
    pub struct Address;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "asset_quantity", schema = "plutus"))]
    pub struct AssetQuantity;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "chain_pointer", schema = "plutus"))]
    pub struct ChainPointer;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "credential", schema = "plutus"))]
    pub struct Credential;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "currency_symbol", schema = "plutus"))]
    pub struct CurrencySymbol;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "datum_hash", schema = "plutus"))]
    pub struct DatumHash;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "ed25519_pub_key_hash", schema = "plutus"))]
    pub struct Ed25519PubKeyHash;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "output_datum", schema = "plutus"))]
    pub struct OutputDatum;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "plutus_data", schema = "plutus"))]
    pub struct PlutusData;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "script_hash", schema = "plutus"))]
    pub struct ScriptHash;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "staking_credential", schema = "plutus"))]
    pub struct StakingCredential;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "token_name", schema = "plutus"))]
    pub struct TokenName;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "transaction_hash", schema = "plutus"))]
    pub struct TransactionHash;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "transaction_input", schema = "plutus"))]
    pub struct TransactionInput;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "transaction_output", schema = "plutus"))]
    pub struct TransactionOutput;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "tx_in_info", schema = "plutus"))]
    pub struct TxInInfo;

    #[derive(diesel::QueryId, diesel::SqlType)]
    #[diesel(postgres_type(name = "value", schema = "plutus"))]
    pub struct Value;
}

//////////////////////
// Hash28
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::Hash28),
    diesel_derive_pg(sql_type = sql_types::Hash28)
)]
pub struct Hash28(#[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Bytea))] pub Vec<u8>);

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for Hash28 {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("cardano.hash28")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Vec<u8> as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

//////////////////////
// Hash32
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::Hash32),
    diesel_derive_pg(sql_type = sql_types::Hash32)
)]
pub struct Hash32(#[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Bytea))] pub Vec<u8>);

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for Hash32 {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("cardano.hash32")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Vec<u8> as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

//////////////////////
// Slot
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::Slot),
    diesel_derive_pg(sql_type = sql_types::Slot)
)]
pub struct Slot(
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = diesel::sql_types::BigInt))] pub i64,
);

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for Slot {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("plutus.slot")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <i64 as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

//////////////////////
// BlockHash
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::BlockHash),
    diesel_derive_pg(sql_type = sql_types::BlockHash)
)]
pub struct BlockHash(
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::Hash32))] pub Hash32,
);

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for BlockHash {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("cardano.block_hash")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Hash32 as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

//////////////////////
// CurrencySymbol
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::CurrencySymbol),
    diesel_derive_pg(sql_type = sql_types::CurrencySymbol)
)]
pub struct CurrencySymbol(
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Bytea))] pub Vec<u8>,
);

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for CurrencySymbol {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("plutus.currency_symbol")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Vec<u8> as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

//////////////////////
// TokenName
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::TokenName),
    diesel_derive_pg(sql_type = sql_types::TokenName)
)]
pub struct TokenName(
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Bytea))] pub Vec<u8>,
);

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for TokenName {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("plutus.token_name")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Vec<u8> as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

//////////////////////
// TransactionHash
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::TransactionHash),
    diesel_derive_pg(sql_type = sql_types::TransactionHash)
)]
pub struct TransactionHash(
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::Hash32))] pub Hash32,
);

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for TransactionHash {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("plutus.transaction_hash")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Hash32 as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

//////////////////////
// Ed25519PubKeyHash
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::Ed25519PubKeyHash),
    diesel_derive_pg(sql_type = sql_types::Ed25519PubKeyHash)
)]
pub struct Ed25519PubKeyHash(
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::Hash28))] pub Hash28,
);

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for Ed25519PubKeyHash {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("plutus.ed25519_pub_key_hash")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Hash28 as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

//////////////////////
// ScriptHash
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::ScriptHash),
    diesel_derive_pg(sql_type = sql_types::ScriptHash)
)]
pub struct ScriptHash(
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::Hash28))] pub Hash28,
);

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for ScriptHash {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("plutus.script_hash")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Hash28 as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

//////////////////////
// DatumHash
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::DatumHash),
    diesel_derive_pg(sql_type = sql_types::DatumHash)
    )]
pub struct DatumHash(
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::Hash32))] pub Hash32,
);

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for DatumHash {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("plutus.datum_hash")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Hash32 as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

//////////////////////
// PlutusData
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

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::PlutusData),
    diesel_derive_pg(sql_type = sql_types::PlutusData)
)]
pub struct PlutusData(
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = diesel::sql_types::Jsonb))]
    pub  serde_json::Value,
);

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for PlutusData {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("plutus.plutus_data")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty
            || <serde_json::Value as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

//////////////////////
// Credential
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "plutus.credential"))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::Credential),
    diesel_derive_pg(sql_type = sql_types::Credential)
)]
pub struct Credential {
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Nullable<sql_types::Ed25519PubKeyHash>))]
    pub pub_key_hash: Option<Ed25519PubKeyHash>,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Nullable<sql_types::ScriptHash>))]
    pub script_hash: Option<ScriptHash>,
}

//////////////////////
// ChainPointer
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "plutus.chain_pointer"))]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::ChainPointer),
    diesel_derive_pg(sql_type = sql_types::ChainPointer)
)]
pub struct ChainPointer {
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = diesel::sql_types::BigInt))]
    pub slot_num: i64,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = diesel::sql_types::BigInt))]
    pub tx_idx: i64,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = diesel::sql_types::BigInt))]
    pub cert_idx: i64,
}

//////////////////////
// StakingCredential
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(
    feature = "sqlx",
    derive(sqlx::Type),
    sqlx(type_name = "plutus.staking_credential")
)]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::StakingCredential),
    diesel_derive_pg(sql_type = sql_types::StakingCredential)
)]
pub struct StakingCredential {
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Nullable<sql_types::Credential>))]
    pub staking_hash: Option<Credential>,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Nullable<sql_types::ChainPointer>))]
    pub staking_ptr: Option<ChainPointer>,
}

//////////////////////
// Address
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(
    feature = "sqlx",
    derive(sqlx::Type),
    sqlx(type_name = "plutus.address")
)]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::Address),
    diesel_derive_pg(sql_type = sql_types::Address)
)]
pub struct Address {
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::Credential))]
    pub credential: Credential,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Nullable<sql_types::StakingCredential>))]
    pub staking_credential: Option<StakingCredential>,
}

//////////////////////
// AssetQuantity
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(
    feature = "sqlx",
    derive(sqlx::Type),
    sqlx(type_name = "plutus.asset_quantity")
)]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::AssetQuantity),
    diesel_derive_pg(sql_type = sql_types::AssetQuantity)
)]
pub struct AssetQuantity {
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::CurrencySymbol))]
    pub currency_symbol: CurrencySymbol,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::TokenName))]
    pub token_name: TokenName,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = diesel::sql_types::BigInt))]
    pub amount: i64,
}

//////////////////////
// Value
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(
    feature = "sqlx",
    derive(sqlx::Type),
    sqlx(type_name = "plutus.asset_quantity[]")
)]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::Value),
    diesel_derive_pg(sql_type = sql_types::Value)
)]
pub struct Value(
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Array<sql_types::AssetQuantity>))]
    pub Vec<AssetQuantity>,
);

//////////////////////
// TransactionInput
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(
    feature = "sqlx",
    derive(sqlx::Type),
    sqlx(type_name = "plutus.transaction_input")
)]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::TransactionInput),
    diesel_derive_pg(sql_type = sql_types::TransactionInput)
)]
pub struct TransactionInput {
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::TransactionHash))]
    pub tx_id: TransactionHash,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = diesel::sql_types::BigInt))]
    pub tx_idx: i64,
}

//////////////////////
// OutputDatum
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(
    feature = "sqlx",
    derive(sqlx::Type),
    sqlx(type_name = "plutus.output_datum")
)]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::OutputDatum),
    diesel_derive_pg(sql_type = sql_types::OutputDatum)
)]
pub struct OutputDatum {
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Nullable<sql_types::DatumHash>))]
    pub datum_hash: Option<DatumHash>,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Nullable<sql_types::PlutusData>))]
    pub inline_datum: Option<PlutusData>,
}

//////////////////////
// TransactionOutput
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(
    feature = "sqlx",
    derive(sqlx::Type),
    sqlx(type_name = "plutus.transaction_output")
)]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::TransactionOutput),
    diesel_derive_pg(sql_type = sql_types::TransactionOutput)
)]
pub struct TransactionOutput {
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::Address))]
    pub address: Address,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::Value))]
    pub assets: Value,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::OutputDatum))]
    pub datum: OutputDatum,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = Nullable<sql_types::ScriptHash>))]
    pub reference_script: Option<ScriptHash>,
}

//////////////////////
// TxInInfo
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(
    feature = "sqlx",
    derive(sqlx::Type),
    sqlx(type_name = "plutus.tx_in_info")
)]
#[cfg_attr(
    feature = "diesel",
    derive(diesel::AsExpression, diesel::FromSqlRow, diesel_derive_pg::PgCustomType),
    diesel(sql_type = sql_types::TxInInfo),
    diesel_derive_pg(sql_type = sql_types::TxInInfo)
)]
pub struct TxInInfo {
    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::TransactionInput))]
    pub reference: TransactionInput,

    #[cfg_attr(feature = "diesel", diesel_derive_pg(sql_type = sql_types::TransactionOutput))]
    pub output: TransactionOutput,
}
