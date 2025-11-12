use async_trait::async_trait;

use crate::{
    error::ErrorPolicyProvider,
    types::{
        cardano::{BlockHash, Point},
        plutus::MultiEraTransaction,
    },
};
use std::fmt::Debug;

/// Indication of when an event happened in the context of the chain.
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ChainEventTime {
    pub block_number: u64,
    pub block_hash: String,
    pub slot: u64,
}

/// Chain events that the indexer is configured to produce.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum ChainEvent {
    /// A new block was emitted
    RollForward {
        block_slot: u64,
        block_hash: BlockHash,
        tip: Point,
        transactions: Vec<MultiEraTransaction>,
    },

    /// Rollback event occurred
    Rollback {
        block_slot: u64,
        block_hash: BlockHash,
        tip: Point,
    },

    /// Regularly emitted events to verify healthiness
    Heartbeat {
        last_synced_block: Point,
        tip: Option<Point>,
        percentage: Option<f32>,
    },
}

#[async_trait]
pub trait EventHandler
where
    Self: Send + Sync + 'static,
{
    type Error: std::error::Error + Send + ErrorPolicyProvider;

    async fn handle(&self, event: &ChainEvent) -> Result<(), Self::Error>;
}
