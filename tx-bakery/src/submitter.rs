use cardano_serialization_lib as csl;
use plutus_ledger_api::v2::transaction::TransactionHash;
use std::collections::BTreeMap;
use std::future::Future;
use thiserror::Error;

/// Component which can submit write actions to the chain
pub trait Submitter {
    fn evaluate_transaction(
        &self,
        tx_builder: &csl::tx_builder::TransactionBuilder,
        plutus_scripts: &Vec<csl::plutus::PlutusScript>,
        redeemers: &Vec<csl::plutus::Redeemer>,
    ) -> impl Future<
        Output = Result<
            BTreeMap<(csl::plutus::RedeemerTag, csl::utils::BigNum), csl::plutus::ExUnits>,
            SubmitterError,
        >,
    >;

    /// Submit a fully build and balanced tranasaction
    fn submit_transaction(
        &self,
        tx: &csl::Transaction,
    ) -> impl Future<Output = Result<TransactionHash, SubmitterError>>;

    /// Wait for transaction confirmation on the chain
    fn await_tx_confirm(
        &self,
        tx_hash: &TransactionHash,
    ) -> impl Future<Output = Result<(), SubmitterError>>;
}

#[derive(Error, Debug)]
#[error(transparent)]
pub struct SubmitterError(pub anyhow::Error);
