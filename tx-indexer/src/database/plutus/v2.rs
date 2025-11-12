use plutus_ledger_api as pla;

use crate::database::plutus::db_types::*;

//////////////////////
// OutputDatum
//////////////////////

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
// TransactionOutput
//////////////////////

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
// TxInInfo
//////////////////////

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
