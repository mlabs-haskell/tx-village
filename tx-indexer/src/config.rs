use tokio_util::sync::CancellationToken;

use crate::{
    handler::{chain_event::EventHandler, retry::RetryPolicy},
    types::cardano::Point,
};
use std::path::PathBuf;

pub struct TxIndexerConfig<H: EventHandler> {
    pub handler: H,
    /// Event source
    pub source: TxIndexerSource,
    /// Retry policy - how much to retry for each event callback failure
    /// This only takes effect on ErrorPolicy for a particular error is `Retry`.
    /// Once retries are exhausted, the handler will error (same treatment as ErrorPolicy::Exit)
    pub retry_policy: RetryPolicy,
    pub cancellation_token: CancellationToken,
}

pub enum TxIndexerSource {
    CardanoNode {
        node_address: PathBuf,
        /// Slot number and hash as hex string (optional).
        /// If not provided, sync will begin from the tip of the chain.
        network_magic: u64,
        since_point: Option<Point>,
        event_queue_size: usize,
    },
    FixtureFiles {
        dir_path: PathBuf,
    },
}

impl<H: EventHandler> TxIndexerConfig<H> {
    #[allow(clippy::too_many_arguments)]
    pub fn cardano_node(
        handler: H,
        node_address: PathBuf,
        network_magic: u64,
        since_point: Option<Point>,
    ) -> Self {
        Self {
            handler,
            source: TxIndexerSource::CardanoNode {
                node_address,
                network_magic,
                since_point,
                event_queue_size: 10,
            },
            retry_policy: RetryPolicy::default(),
            cancellation_token: CancellationToken::new(),
        }
    }

    pub fn source_from_fixtures(handler: H, dir_path: PathBuf) -> Self {
        Self {
            handler,
            source: TxIndexerSource::FixtureFiles { dir_path },
            retry_policy: RetryPolicy::default(),
            cancellation_token: CancellationToken::new(),
        }
    }

    pub fn with_event_queue_size(&mut self, queue_size: usize) -> &mut Self {
        match &mut self.source {
            TxIndexerSource::CardanoNode {
                ref mut event_queue_size,
                ..
            } => {
                *event_queue_size = queue_size;
                self
            }
            TxIndexerSource::FixtureFiles { .. } => self,
        }
    }

    pub fn with_retry_policy(mut self, retry_policy: RetryPolicy) -> Self {
        self.retry_policy = retry_policy;
        self
    }

    pub fn with_cancellation_token(mut self, cancellation_token: CancellationToken) -> Self {
        self.cancellation_token = cancellation_token;
        self
    }
}
