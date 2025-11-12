use plutus_ledger_api as pla;

use crate::{database::plutus::db_types::*, types::cardano};

//////////////////////
// Hash28
//////////////////////

impl From<pla::v1::crypto::LedgerBytes> for Hash28 {
    fn from(item: pla::v1::crypto::LedgerBytes) -> Self {
        Hash28(item.0)
    }
}

impl From<Hash28> for pla::v1::crypto::LedgerBytes {
    fn from(item: Hash28) -> Self {
        pla::v1::crypto::LedgerBytes(item.0)
    }
}

//////////////////////
// Hash32
//////////////////////

impl From<pla::v1::crypto::LedgerBytes> for Hash32 {
    fn from(item: pla::v1::crypto::LedgerBytes) -> Self {
        Hash32(item.0)
    }
}

impl From<Hash32> for pla::v1::crypto::LedgerBytes {
    fn from(item: Hash32) -> Self {
        pla::v1::crypto::LedgerBytes(item.0)
    }
}

//////////////////////
// Slot
//////////////////////

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
// BlockHash
//////////////////////

impl From<cardano::BlockHash> for BlockHash {
    fn from(item: cardano::BlockHash) -> Self {
        BlockHash(Hash32(item.0))
    }
}

impl From<BlockHash> for cardano::BlockHash {
    fn from(item: BlockHash) -> Self {
        cardano::BlockHash(item.0 .0)
    }
}
