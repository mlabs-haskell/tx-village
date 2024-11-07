use anyhow::anyhow;
use plutus_ledger_api::csl::{csl_to_pla::TryFromCSLError, pla_to_csl::TryFromPLAError};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use thiserror::Error;
use tx_bakery::{chain_query::ChainQueryError, submitter::SubmitterError};

pub type Result<T> = std::result::Result<T, OgmiosError>;

#[derive(Error, Debug)]
pub enum OgmiosError {
    #[error(transparent)]
    TryFromPLAError(#[from] TryFromPLAError),

    #[error(transparent)]
    TryFromCSLError(#[from] TryFromCSLError),

    #[error("Couldn't convert a {label} from Ogmios response: {source}")]
    ConversionError {
        label: String,
        source: anyhow::Error,
    },

    #[error(transparent)]
    RequestError(#[from] reqwest::Error),

    #[error(transparent)]
    IOError(#[from] std::io::Error),

    #[error("Failed to start Ogmios: {0}")]
    StartupError(anyhow::Error),

    #[error(transparent)]
    JSONRpcError(#[from] jsonrpsee::core::client::Error),
}

impl From<OgmiosError> for ChainQueryError {
    fn from(err: OgmiosError) -> ChainQueryError {
        ChainQueryError(anyhow!(err))
    }
}

impl From<OgmiosError> for SubmitterError {
    fn from(err: OgmiosError) -> SubmitterError {
        SubmitterError(anyhow!(err))
    }
}

#[derive(Error, Clone, Debug, Serialize, Deserialize)]
#[error("JsonRPCError {code}: {message} ({data:?})")]
pub struct JsonRPCError {
    pub code: i16,
    pub message: String,
    pub data: Option<Value>,
}
