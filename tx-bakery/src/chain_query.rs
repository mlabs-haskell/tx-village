use crate::utils::csl_adapter;
use crate::utils::script::Script;
use cardano_serialization_lib as csl;
use chrono::{DateTime, Duration, Utc};
use plutus_ledger_api::v2::address::Address;
use plutus_ledger_api::v2::datum::OutputDatum;
use plutus_ledger_api::v2::transaction::{TransactionInput, TransactionOutput};
use plutus_ledger_api::v2::value::Value;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::future::Future;
use thiserror::Error;

/// A chain query client responsible for all read actions from the blockchain (no write)
pub trait ChainQuery {
    /// Query the network id (not identical to network magic)
    fn get_network(&self) -> Network;

    fn query_system_start(&self) -> impl Future<Output = Result<DateTime<Utc>, ChainQueryError>>;

    fn query_era_summaries(&self)
        -> impl Future<Output = Result<Vec<EraSummary>, ChainQueryError>>;
    /// Query protocol parameters
    fn query_protocol_params(
        &self,
    ) -> impl Future<Output = Result<ProtocolParameters, ChainQueryError>>;

    fn query_tip(&self) -> impl Future<Output = Result<ChainTip, ChainQueryError>>;

    /// Query UTxOs at an address
    fn query_utxos_by_addr(
        &self,
        address: &Address,
    ) -> impl Future<Output = Result<BTreeMap<TransactionInput, FullTransactionOutput>, ChainQueryError>>;

    fn query_utxos_by_ref(
        &self,
        references: Vec<&TransactionInput>,
    ) -> impl Future<Output = Result<BTreeMap<TransactionInput, FullTransactionOutput>, ChainQueryError>>;
}

#[derive(Debug, Clone, Deserialize)]
pub enum Network {
    Testnet = 0b0000,
    Mainnet = 0b0001,
}

impl Network {
    pub fn to_network_id(&self) -> u8 {
        match self {
            Network::Testnet => 0b0000,
            Network::Mainnet => 0b0001,
        }
    }
}

#[derive(Error, Debug)]
#[error(transparent)]
pub struct ChainQueryError(pub anyhow::Error);

#[derive(Debug, Clone)]
pub struct EraSummary {
    pub start: EraTime,
    pub end: Option<EraTime>,
    pub parameters: EraParameters,
}

#[derive(Debug, Clone)]
pub struct EraTime {
    pub time: Duration,
    pub slot: u64,
    pub epoch: u64,
}

#[derive(Debug, Clone)]
pub struct EraParameters {
    pub epoch_length: u64,
    pub slot_length: f64,
    pub safe_zone: Option<u64>,
}

/// A subset of Cardano protocol parameters, only handling values that we use for transaction
/// building
#[derive(Debug, Clone)]
pub struct ProtocolParameters {
    pub min_fee_coefficient: csl::utils::Coin,
    pub min_fee_constant: csl::utils::Coin,
    pub min_utxo_deposit_coefficient: csl::utils::Coin,
    pub min_utxo_deposit_constant: csl::utils::Coin,
    // pub max_block_body_size: Bytes,
    // pub max_block_header_size: Bytes,
    pub max_transaction_size: Option<u32>,
    pub max_value_size: Option<u32>,
    // pub extra_entropy: Option<Nonce>,
    pub stake_credential_deposit: csl::utils::Coin,
    pub stake_pool_deposit: csl::utils::Coin,
    // pub stake_pool_retirement_epoch_bound: u64,
    // pub stake_pool_pledge_influence: Ratio,
    // pub min_stake_pool_cost: LovelaceOnly,
    // pub desired_number_of_stake_pools: u64,
    // pub federated_block_production_ratio: Option<Ratio>,
    // pub monetary_expansion: Ratio,
    // pub treasury_expansion: Ratio,
    // pub collateral_percentage: Option<u64>,
    // pub max_collateral_inputs: Option<u64>,
    pub plutus_cost_models: Option<csl::plutus::Costmdls>,
    pub script_execution_prices: Option<csl::plutus::ExUnitPrices>,
    // pub max_execution_units_per_transaction: Option<ExecutionUnits>,
    // pub max_execution_units_per_block: Option<ExecutionUnits>,
    // pub stake_pool_voting_thresholds: Option<StakePoolVotingThresholds>,
    // pub constitutional_committee_min_size: Option<u64>,
    // pub constitutional_committee_max_term_length: Option<u64>,
    // pub governance_action_lifetime: Option<Epoch>,
    // pub governance_action_deposit: Option<LovelaceOnly>,
    // pub delegate_representative_voting_thresholds: Option<DelegateRepresentativeVotingThresholds>,
    // pub delegate_representative_deposit: Option<LovelaceOnly>,
    // pub delegate_representative_max_idle_time: Option<Epoch>,
    // pub version: ProtocolVersion,
    //
}

#[derive(Debug, Clone)]
pub enum ChainTip {
    Origin,
    Point { slot: u64, id: String },
}

impl ChainTip {
    pub fn slot(&self) -> u64 {
        match self {
            ChainTip::Origin => 0,
            ChainTip::Point { slot, id: _ } => *slot,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FullTransactionOutput {
    pub address: Address,
    pub value: Value,
    pub datum: OutputDatum,
    pub reference_script: Option<Script>,
}

impl From<FullTransactionOutput> for TransactionOutput {
    fn from(full_tx_out: FullTransactionOutput) -> TransactionOutput {
        TransactionOutput {
            address: full_tx_out.address,
            value: full_tx_out.value,
            datum: full_tx_out.datum,
            reference_script: full_tx_out.reference_script.map(|script| match script {
                Script::PlutusScript(script, _) => csl_adapter::from_script_hash(&script.hash()),
                Script::NativeScript(script) => csl_adapter::from_script_hash(&script.hash()),
            }),
        }
    }
}

impl From<&FullTransactionOutput> for TransactionOutput {
    fn from(full_tx_out: &FullTransactionOutput) -> TransactionOutput {
        full_tx_out.clone().into()
    }
}
