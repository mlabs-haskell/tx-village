use plutus_ledger_api::csl::pla_to_csl::TryFromPLAError;
use thiserror::Error;
use tx_indexer::{
    database::plutus::db_types::DBTypeConversionError,
    error::{ErrorPolicy, ErrorPolicyProvider},
};

#[derive(Error, Debug)]
pub enum UtxoIndexerError {
    #[error(transparent)]
    DbError(#[from] diesel::result::Error),

    #[error(transparent)]
    DBTypeConversionError(#[from] DBTypeConversionError),

    #[error("Internal error occurred: {0}")]
    Internal(anyhow::Error),

    #[error(transparent)]
    TryFromPLAError(#[from] TryFromPLAError),

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

impl ErrorPolicyProvider for UtxoIndexerError {
    fn get_error_policy(&self) -> ErrorPolicy<Self> {
        ErrorPolicy::Exit
    }
}
