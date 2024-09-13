#[cfg(feature = "diesel")]
use diesel::sql_types::{Array, Bytea, Jsonb, Nullable};
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
/// Hash28
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::Hash28))]
pub struct Hash28(pub Vec<u8>);

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "hash28"))]
pub struct PgHash28;

impl From<pla::v2::crypto::LedgerBytes> for Hash28 {
    fn from(item: pla::v2::crypto::LedgerBytes) -> Self {
        Hash28(item.0)
    }
}

impl From<Hash28> for pla::v2::crypto::LedgerBytes {
    fn from(item: Hash28) -> Self {
        pla::v2::crypto::LedgerBytes(item.0)
    }
}

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for Hash28 {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("Plutus.Hash28")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Vec<u8> as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::Hash28, diesel::pg::Pg> for Hash28 {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let inner = diesel::deserialize::FromSql::<Bytea, diesel::pg::Pg>::from_sql(bytes)?;
        Ok(Self(inner))
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::Hash28, diesel::pg::Pg> for Hash28 {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        <Vec<u8> as diesel::serialize::ToSql<Bytea, diesel::pg::Pg>>::to_sql(
            &self.0,
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// Hash32
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::Hash32))]
pub struct Hash32(pub Vec<u8>);

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "hash32"))]
pub struct PgHash32;

impl From<pla::v2::crypto::LedgerBytes> for Hash32 {
    fn from(item: pla::v2::crypto::LedgerBytes) -> Self {
        Hash32(item.0)
    }
}

impl From<Hash32> for pla::v2::crypto::LedgerBytes {
    fn from(item: Hash32) -> Self {
        pla::v2::crypto::LedgerBytes(item.0)
    }
}

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for Hash32 {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("Plutus.Hash32")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Vec<u8> as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::Hash32, diesel::pg::Pg> for Hash32 {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let inner = diesel::deserialize::FromSql::<Bytea, diesel::pg::Pg>::from_sql(bytes)?;
        Ok(Self(inner))
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::Hash32, diesel::pg::Pg> for Hash32 {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        <Vec<u8> as diesel::serialize::ToSql<Bytea, diesel::pg::Pg>>::to_sql(
            &self.0,
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// CurrencySymbol
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature  = "diesel", diesel(sql_type = tx_indexer::schema::CurrencySymbol))]
pub struct CurrencySymbol(pub Vec<u8>);

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "currencysymbol"))]
pub struct PgCurrencySymbol;

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

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for CurrencySymbol {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("Plutus.CurrencySymbol")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Vec<u8> as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::CurrencySymbol, diesel::pg::Pg>
    for CurrencySymbol
{
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let inner = diesel::deserialize::FromSql::<Bytea, diesel::pg::Pg>::from_sql(bytes)?;
        Ok(Self(inner))
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::CurrencySymbol, diesel::pg::Pg>
    for CurrencySymbol
{
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        <Vec<u8> as diesel::serialize::ToSql<Bytea, diesel::pg::Pg>>::to_sql(
            &self.0,
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// TokenName
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature  = "diesel", diesel(sql_type = tx_indexer::schema::TokenName))]
pub struct TokenName(pub Vec<u8>);

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "tokenname"))]
pub struct PgTokenName;

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

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for TokenName {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("Plutus.TokenName")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Vec<u8> as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::TokenName, diesel::pg::Pg> for TokenName {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let inner = diesel::deserialize::FromSql::<Bytea, diesel::pg::Pg>::from_sql(bytes)?;
        Ok(Self(inner))
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::TokenName, diesel::pg::Pg> for TokenName {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        <Vec<u8> as diesel::serialize::ToSql<Bytea, diesel::pg::Pg>>::to_sql(
            &self.0,
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// TransactionHash
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature  = "diesel", diesel(sql_type = tx_indexer::schema::TransactionHash))]
pub struct TransactionHash(pub Hash32);

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "transactionhash"))]
pub struct PgTransactionHash;

impl From<pla::v2::transaction::TransactionHash> for TransactionHash {
    fn from(item: pla::v2::transaction::TransactionHash) -> Self {
        TransactionHash(item.0.into())
    }
}

impl From<TransactionHash> for pla::v2::transaction::TransactionHash {
    fn from(item: TransactionHash) -> Self {
        pla::v2::transaction::TransactionHash(item.0.into())
    }
}

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for TransactionHash {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("Plutus.TransactionHash")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Hash32 as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::TransactionHash, diesel::pg::Pg>
    for TransactionHash
{
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let inner = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self(inner))
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::TransactionHash, diesel::pg::Pg>
    for TransactionHash
{
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        <Hash32 as diesel::serialize::ToSql<tx_indexer::schema::Hash32, diesel::pg::Pg>>::to_sql(
            &self.0,
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// Ed25519PubKeyHash
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature  = "diesel", diesel(sql_type = tx_indexer::schema::Ed25519PubKeyHash))]
pub struct Ed25519PubKeyHash(pub Hash28);

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "ed25519pubkeyhash"))]
pub struct PgEd25519PubKeyHash;

impl From<pla::v2::crypto::Ed25519PubKeyHash> for Ed25519PubKeyHash {
    fn from(item: pla::v2::crypto::Ed25519PubKeyHash) -> Self {
        Ed25519PubKeyHash(item.0.into())
    }
}

impl From<Ed25519PubKeyHash> for pla::v2::crypto::Ed25519PubKeyHash {
    fn from(item: Ed25519PubKeyHash) -> Self {
        pla::v2::crypto::Ed25519PubKeyHash(item.0.into())
    }
}

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for Ed25519PubKeyHash {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("Plutus.Ed25519PubKeyHash")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Hash28 as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::Ed25519PubKeyHash, diesel::pg::Pg>
    for Ed25519PubKeyHash
{
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let inner = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self(inner))
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::Ed25519PubKeyHash, diesel::pg::Pg>
    for Ed25519PubKeyHash
{
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        <Hash28 as diesel::serialize::ToSql<tx_indexer::schema::Hash28, diesel::pg::Pg>>::to_sql(
            &self.0,
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// ScriptHash
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature  = "diesel", diesel(sql_type = tx_indexer::schema::ScriptHash))]
pub struct ScriptHash(pub Hash28);

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "scripthash"))]
pub struct PgScriptHash;

impl From<pla::v2::script::ScriptHash> for ScriptHash {
    fn from(item: pla::v2::script::ScriptHash) -> Self {
        ScriptHash(item.0.into())
    }
}

impl From<ScriptHash> for pla::v2::script::ScriptHash {
    fn from(item: ScriptHash) -> Self {
        pla::v2::script::ScriptHash(item.0.into())
    }
}

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for ScriptHash {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("Plutus.ScriptHash")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Hash28 as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::ScriptHash, diesel::pg::Pg> for ScriptHash {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let inner = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self(inner))
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::ScriptHash, diesel::pg::Pg> for ScriptHash {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        <Hash28 as diesel::serialize::ToSql<tx_indexer::schema::Hash28, diesel::pg::Pg>>::to_sql(
            &self.0,
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// DatumHash
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::DatumHash))]
pub struct DatumHash(pub Hash32);

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "datumhash"))]
pub struct PgDatumHash;

impl From<pla::v2::datum::DatumHash> for DatumHash {
    fn from(item: pla::v2::datum::DatumHash) -> Self {
        DatumHash(item.0.into())
    }
}

impl From<DatumHash> for pla::v2::datum::DatumHash {
    fn from(item: DatumHash) -> Self {
        pla::v2::datum::DatumHash(item.0.into())
    }
}

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for DatumHash {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("Plutus.DatumHash")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <Hash32 as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::DatumHash, diesel::pg::Pg> for DatumHash {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let inner = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self(inner))
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::DatumHash, diesel::pg::Pg> for DatumHash {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        <Hash32 as diesel::serialize::ToSql<tx_indexer::schema::Hash32, diesel::pg::Pg>>::to_sql(
            &self.0,
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// Slot
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::Slot))]
pub struct Slot(pub i64);

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "slot"))]
pub struct PgSlot;

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

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for Slot {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("Plutus.Slot")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty || <i64 as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::Slot, diesel::pg::Pg> for Slot {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let slot = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self(slot))
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::Slot, diesel::pg::Pg> for Slot {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        <i64 as diesel::serialize::ToSql<diesel::sql_types::BigInt, diesel::pg::Pg>>::to_sql(
            &self.0,
            &mut out.reborrow(),
        )
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

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Encode, sqlx::Decode))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::PlutusData))]
pub struct PlutusData(pub serde_json::Value);

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "plutusdata"))]
pub struct PgPlutusData;

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

#[cfg(feature = "sqlx")]
impl ::sqlx::Type<::sqlx::postgres::Postgres> for PlutusData {
    fn type_info() -> ::sqlx::postgres::PgTypeInfo {
        ::sqlx::postgres::PgTypeInfo::with_name("Plutus.PlutusData")
    }
    fn compatible(ty: &sqlx::postgres::PgTypeInfo) -> ::std::primitive::bool {
        Self::type_info() == *ty
            || <serde_json::Value as ::sqlx::Type<::sqlx::Postgres>>::compatible(ty)
    }
}

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::PlutusData, diesel::pg::Pg> for PlutusData {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let inner = diesel::deserialize::FromSql::<Jsonb, diesel::pg::Pg>::from_sql(bytes)?;
        Ok(Self(inner))
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::PlutusData, diesel::pg::Pg> for PlutusData {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        <serde_json::Value as diesel::serialize::ToSql<
            diesel::pg::sql_types::Jsonb,
            diesel::pg::Pg,
        >>::to_sql(&self.0, &mut out.reborrow())
    }
}

//////////////////////
/// Credential
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "Plutus.Credential"))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::Credential))]
pub struct Credential {
    pub_key_hash: Option<Ed25519PubKeyHash>,
    script_hash: Option<ScriptHash>,
}

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "credential"))]
pub struct PgCredential;

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

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::Credential, diesel::pg::Pg> for Credential {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let (pub_key_hash, script_hash) = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self {
            pub_key_hash,
            script_hash,
        })
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::Credential, diesel::pg::Pg> for Credential {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        diesel::serialize::WriteTuple::<(
            Nullable<tx_indexer::schema::Ed25519PubKeyHash>,
            Nullable<tx_indexer::schema::ScriptHash>,
        )>::write_tuple(
            &(self.pub_key_hash.clone(), self.script_hash.clone()),
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// ChainPointer
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "Plutus.ChainPointer"))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::ChainPointer))]
pub struct ChainPointer {
    slot_num: i64,
    tx_idx: i64,
    cert_idx: i64,
}

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "chainpointer"))]
pub struct PgChainPointer;

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

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::ChainPointer, diesel::pg::Pg>
    for ChainPointer
{
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let (slot_num, tx_idx, cert_idx) = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self {
            slot_num,
            tx_idx,
            cert_idx,
        })
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::ChainPointer, diesel::pg::Pg> for ChainPointer {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        diesel::serialize::WriteTuple::<(
            diesel::sql_types::BigInt,
            diesel::sql_types::BigInt,
            diesel::sql_types::BigInt,
        )>::write_tuple(
            &(self.slot_num, self.tx_idx, self.cert_idx),
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// StakingCredential
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "Plutus.StakingCredential"))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::StakingCredential))]
pub struct StakingCredential {
    staking_hash: Option<Credential>,
    staking_ptr: Option<ChainPointer>,
}
#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "stakingcredential"))]
pub struct PgStakingCredential;

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

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::StakingCredential, diesel::pg::Pg>
    for StakingCredential
{
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let (staking_hash, staking_ptr) = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self {
            staking_hash,
            staking_ptr,
        })
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::StakingCredential, diesel::pg::Pg>
    for StakingCredential
{
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        diesel::serialize::WriteTuple::<(
            Nullable<tx_indexer::schema::Credential>,
            Nullable<tx_indexer::schema::ChainPointer>,
        )>::write_tuple(
            &(self.staking_hash.clone(), self.staking_ptr.clone()),
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// Address
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "Plutus.Address"))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = PgAddress))]
pub struct Address {
    credential: Credential,
    staking_credential: Option<StakingCredential>,
}

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "address"))]
pub struct PgAddress;

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

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::Address, diesel::pg::Pg> for Address {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let (credential, staking_credential) = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self {
            credential,
            staking_credential,
        })
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::Address, diesel::pg::Pg> for Address {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        diesel::serialize::WriteTuple::<(
            tx_indexer::schema::Credential,
            Nullable<tx_indexer::schema::StakingCredential>,
        )>::write_tuple(
            &(self.credential.clone(), self.staking_credential.clone()),
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// AssetQuantity
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "Plutus.AssetQuantity"))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::AssetQuantity))]
pub struct AssetQuantity {
    currency_symbol: CurrencySymbol,
    token_name: TokenName,
    amount: i64,
}

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "assetquantity"))]
pub struct PgAssetQuantity;

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

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::AssetQuantity, diesel::pg::Pg>
    for AssetQuantity
{
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let (currency_symbol, token_name, amount) = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self {
            currency_symbol,
            token_name,
            amount,
        })
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::AssetQuantity, diesel::pg::Pg> for AssetQuantity {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        diesel::serialize::WriteTuple::<(
            tx_indexer::schema::CurrencySymbol,
            tx_indexer::schema::TokenName,
            diesel::sql_types::BigInt,
        )>::write_tuple(
            &(
                self.currency_symbol.clone(),
                self.token_name.clone(),
                self.amount,
            ),
            &mut out.reborrow(),
        )
    }
}
//////////////////////
/// Value
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "Plutus.AssetQuantity[]"))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = PgValue))]
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

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "value"))]
pub struct PgValue;

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

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<PgValue, diesel::pg::Pg> for Value {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let inner = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self(inner))
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<PgValue, diesel::pg::Pg> for Value {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        <Vec<AssetQuantity> as diesel::serialize::ToSql<
            Array<tx_indexer::schema::AssetQuantity>,
            diesel::pg::Pg,
        >>::to_sql(&self.0, &mut out.reborrow())
    }
}

//////////////////////
/// TransactionInput
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "Plutus.TransactionInput"))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::TransactionInput))]
pub struct TransactionInput {
    tx_id: TransactionHash,
    tx_idx: i64,
}

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "transactioninput"))]
pub struct PgTransactionInput;

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

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::TransactionInput, diesel::pg::Pg>
    for TransactionInput
{
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let (tx_id, tx_idx) = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self { tx_id, tx_idx })
    }
}
#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::TransactionInput, diesel::pg::Pg>
    for TransactionInput
{
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        diesel::serialize::WriteTuple::<(
            tx_indexer::schema::TransactionHash,
            diesel::sql_types::BigInt,
        )>::write_tuple(&(self.tx_id.clone(), self.tx_idx), &mut out.reborrow())
    }
}

//////////////////////
/// OutputDatum
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "Plutus.OutputDatum"))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::OutputDatum))]
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

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::OutputDatum, diesel::pg::Pg> for OutputDatum {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let (datum_hash, inline_datum) = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self {
            datum_hash,
            inline_datum,
        })
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::OutputDatum, diesel::pg::Pg> for OutputDatum {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        diesel::serialize::WriteTuple::<(
            Nullable<tx_indexer::schema::DatumHash>,
            Nullable<tx_indexer::schema::PlutusData>,
        )>::write_tuple(
            &(self.datum_hash.clone(), self.inline_datum.clone()),
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// TransactionOutput
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "Plutus.TransactionOutput"))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::TransactionOutput))]
pub struct TransactionOutput {
    address: Address,
    assets: Value,
    datum: OutputDatum,
    reference_script: Option<ScriptHash>,
}

#[cfg(feature = "diesel")]
#[derive(diesel::SqlType, diesel::QueryId)]
#[diesel(postgres_type(schema = "plutus", name = "transactionoutput"))]
pub struct PgTransactionOutput;

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

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::TransactionOutput, diesel::pg::Pg>
    for TransactionOutput
{
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let (address, assets, datum, reference_script) =
            diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self {
            address,
            assets,
            datum,
            reference_script,
        })
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<crate::schema::sql_types::Transactionoutput, diesel::pg::Pg>
    for TransactionOutput
{
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        diesel::serialize::WriteTuple::<(
            crate::schema::sql_types::Address,
            crate::schema::sql_types::Value,
            crate::schema::sql_types::OutputDatum,
            Nullable<crateschema::ScriptHash>,
        )>::write_tuple(
            &(
                self.address.clone(),
                self.assets.clone(),
                self.datum.clone(),
                self.reference_script.clone(),
            ),
            &mut out.reborrow(),
        )
    }
}

//////////////////////
/// TxInInfo
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "sqlx", derive(sqlx::Type))]
#[cfg_attr(feature = "sqlx", sqlx(type_name = "Plutus.TxInInfo"))]
#[cfg_attr(feature = "diesel", derive(diesel::AsExpression, diesel::FromSqlRow))]
#[cfg_attr(feature = "diesel", diesel(sql_type = tx_indexer::schema::TxInInfo))]
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

#[cfg(feature = "diesel")]
impl diesel::deserialize::FromSql<tx_indexer::schema::TxInInfo, diesel::pg::Pg> for TxInInfo {
    fn from_sql(bytes: diesel::pg::PgValue) -> diesel::deserialize::Result<Self> {
        let (reference, output) = diesel::deserialize::FromSql::from_sql(bytes)?;
        Ok(Self { reference, output })
    }
}

#[cfg(feature = "diesel")]
impl diesel::serialize::ToSql<tx_indexer::schema::TxInInfo, diesel::pg::Pg> for TxInInfo {
    fn to_sql<'b>(
        &'b self,
        out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
    ) -> diesel::serialize::Result {
        diesel::serialize::WriteTuple::<(
            tx_indexer::schema::TransactionInput,
            tx_indexer::schema::TransactionOutput,
        )>::write_tuple(
            &(self.reference.clone(), self.output.clone()),
            &mut out.reborrow(),
        )
    }
}
