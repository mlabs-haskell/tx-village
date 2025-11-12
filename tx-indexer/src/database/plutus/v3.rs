use plutus_ledger_api as pla;

use crate::database::plutus::db_types::*;

//////////////////////
// TransactionHash
//////////////////////

impl From<pla::v3::transaction::TransactionHash> for TransactionHash {
    fn from(item: pla::v3::transaction::TransactionHash) -> Self {
        TransactionHash(item.0.into())
    }
}

impl From<TransactionHash> for pla::v3::transaction::TransactionHash {
    fn from(item: TransactionHash) -> Self {
        pla::v3::transaction::TransactionHash(item.0.into())
    }
}

//////////////////////
// TransactionInput
//////////////////////

impl TryFrom<pla::v3::transaction::TransactionInput> for TransactionInput {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v3::transaction::TransactionInput) -> Result<Self, Self::Error> {
        Ok(TransactionInput {
            tx_id: item.transaction_id.into(),
            tx_idx: item
                .index
                .try_into()
                .map_err(DBTypeConversionError::BigIntConversion)?,
        })
    }
}

impl From<TransactionInput> for pla::v3::transaction::TransactionInput {
    fn from(item: TransactionInput) -> Self {
        pla::v3::transaction::TransactionInput {
            transaction_id: item.tx_id.into(),
            index: item.tx_idx.into(),
        }
    }
}

//////////////////////
// TxInInfo
//////////////////////

impl TryFrom<pla::v3::transaction::TxInInfo> for TxInInfo {
    type Error = DBTypeConversionError;

    fn try_from(item: pla::v3::transaction::TxInInfo) -> Result<Self, Self::Error> {
        Ok(TxInInfo {
            reference: item.reference.try_into()?,
            output: item.output.try_into()?,
        })
    }
}

impl TryFrom<TxInInfo> for pla::v3::transaction::TxInInfo {
    type Error = DBTypeConversionError;

    fn try_from(item: TxInInfo) -> Result<Self, Self::Error> {
        Ok(pla::v3::transaction::TxInInfo {
            reference: item.reference.into(),
            output: item.output.try_into()?,
        })
    }
}
