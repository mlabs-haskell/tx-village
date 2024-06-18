use plutus_ledger_api::v2::{crypto::LedgerBytes, transaction::TransactionHash};
use sqlx::{
    database::HasValueRef,
    encode::IsNull,
    error::BoxDynError,
    postgres::{PgArgumentBuffer, PgTypeInfo},
    prelude::Type,
    Decode, Encode, Postgres,
};

//////////////////////
/// TransactionHash
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TransactionHashDB(pub TransactionHash);

impl From<TransactionHash> for TransactionHashDB {
    fn from(item: TransactionHash) -> Self {
        TransactionHashDB(item)
    }
}

impl From<TransactionHashDB> for TransactionHash {
    fn from(item: TransactionHashDB) -> Self {
        item.0
    }
}

// Simple delegation to Vec<u8> (i.e bytea)
impl Encode<'_, Postgres> for TransactionHashDB {
    fn encode_by_ref(&self, buf: &mut PgArgumentBuffer) -> IsNull {
        <Vec<u8> as Encode<Postgres>>::encode_by_ref(&self.0 .0 .0, buf)
    }
}

// Simple delegation to Vec<u8> (i.e bytea)
impl<'r> Decode<'r, Postgres> for TransactionHashDB {
    fn decode(value: <Postgres as HasValueRef<'r>>::ValueRef) -> Result<Self, BoxDynError> {
        <Vec<u8> as Decode<Postgres>>::decode(value)
            .map(|x| TransactionHashDB(TransactionHash(LedgerBytes(x))))
    }
}

// TransactionHashDB is a custom domain type (Hash32) that acts on bytea.
impl Type<Postgres> for TransactionHashDB {
    fn type_info() -> PgTypeInfo {
        PgTypeInfo::with_name("Plutus.TxId")
    }
}

//////////////////////
/// Slot
//////////////////////

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

// Simple delegation to Vec<u8> (i.e bytea)
impl Encode<'_, Postgres> for SlotDB {
    fn encode_by_ref(&self, buf: &mut PgArgumentBuffer) -> IsNull {
        <i64 as Encode<Postgres>>::encode_by_ref(&self.0, buf)
    }
}

// Simple delegation to Vec<u8> (i.e bytea)
impl<'r> Decode<'r, Postgres> for SlotDB {
    fn decode(value: <Postgres as HasValueRef<'r>>::ValueRef) -> Result<Self, BoxDynError> {
        <i64 as Decode<Postgres>>::decode(value).map(|x| SlotDB(x))
    }
}

// SlotDB is a custom domain type (Hash32) that acts on bytea.
impl Type<Postgres> for SlotDB {
    fn type_info() -> PgTypeInfo {
        PgTypeInfo::with_name("Plutus.Slot")
    }
}
