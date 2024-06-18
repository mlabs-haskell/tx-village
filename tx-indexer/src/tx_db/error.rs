use thiserror::Error;
use tx_indexer::error::{ErrorPolicy, ErrorPolicyProvider};

#[derive(Error, Debug)]
pub enum TxIndexerError {
    #[error(transparent)]
    DbError(#[from] sqlx::error::Error),
}

impl ErrorPolicyProvider for TxIndexerError {
    fn get_error_policy(&self) -> ErrorPolicy<Self> {
        ErrorPolicy::Skip
    }
}
