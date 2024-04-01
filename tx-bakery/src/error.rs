use cardano_serialization_lib as csl;
use plutus_ledger_api::v2::{
    datum::DatumHash,
    script::{MintingPolicyHash, ScriptHash, ValidatorHash},
};
use thiserror::Error;

use crate::{
    chain_query::ChainQueryError, submitter::SubmitterError,
    utils::csl_adapter::CSLConversionError, wallet::WalletError,
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unable to find redeemer for minting policy (hash: {0:?})")]
    MissingMintRedeemer(MintingPolicyHash),

    #[error("Unable to find redeemer for datum (hash: {0:?})")]
    MissingDatum(DatumHash),

    #[error("Unable to find validator script (hash: {0:?})")]
    MissingValidatorScript(ValidatorHash),

    #[error("Unable to find minting policy script (hash: {0:?})")]
    MissingMintingPolicyScript(MintingPolicyHash),

    #[error("Unable to find plutus script (hash: {0:?})")]
    MissingScript(ScriptHash),

    #[error("Couldn't find suitable collateral.")]
    MissingCollateral,

    #[error("Change strategy is set to LastOutput, but it is missing from the TransactionInfo.")]
    MissingChangeOutput,

    #[error("Execution units was not calculated for {0:?}.")]
    MissingExUnits((csl::plutus::RedeemerTag, csl::utils::BigNum)),

    #[error("Protocol parameter {0} is missing")]
    MissingProtocolParameter(String),

    #[error(transparent)]
    CSLConversionError(#[from] CSLConversionError),

    #[error(transparent)]
    ConversionError(anyhow::Error),

    #[error("Unsupported {0}.")]
    Unsupported(String),

    #[error("Internal error: {0}")]
    Internal(anyhow::Error),

    #[error("Transaction building failed: {0}")]
    TransactionBuildError(anyhow::Error),

    #[error("Chain query failed: {0}")]
    ChainQueryError(#[from] ChainQueryError),

    #[error("Chain query failed: {0}")]
    WalletError(#[from] WalletError),

    #[error("Chain query failed: {0}")]
    SubmitterError(#[from] SubmitterError),

    #[error("Error occurred due to a configuration for {0}")]
    InvalidConfiguration(String),

    #[error("A POSIX time value is invalid: {0}")]
    InvalidPOSIXTime(String),
}
