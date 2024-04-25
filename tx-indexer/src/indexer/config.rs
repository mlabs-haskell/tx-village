use std::{future::Future, sync::Arc};

use oura::model::Event;
use sqlx::{pool::PoolConnection, Postgres};

use super::{
    filter::Filter,
    retry::RetryPolicy,
    types::{AsyncFunction, AsyncResult, IsNetworkMagic, NodeAddress},
};

pub struct IndexerConfig<T: IsNetworkMagic, E> {
    pub node_address: NodeAddress,
    pub network_magic: T,
    /// Slot number and hash as hex string (optional).
    /// If not provided, sync will begin from the tip of the chain.
    pub since_slot: Option<(u64, String)>,
    /// Minimum depth a block has to be from the tip for it to be considered "confirmed"
    /// See: https://oura.txpipe.io/v1/advanced/rollback_buffer
    pub safe_block_depth: usize,
    pub event_filter: Option<Filter>,
    /// Callback function to pass events to
    pub callback_fn: Arc<AsyncFunction<(Event, PoolConnection<Postgres>), AsyncResult<E>>>,
    /// Retry policy - how much to retry for each event callback failure
    /// This only takes effect on ErrorPolicy for a particular error is `Retry`.
    /// Once retries are exhausted, the handler will error (same treatment as ErrorPolicy::Exit)
    pub retry_policy: RetryPolicy,
    /// Postgres database URL
    pub database_url: String,
}

impl<T: IsNetworkMagic, E> IndexerConfig<T, E> {
    pub fn new<R: Future<Output = Result<(), E>> + Send + Sync + 'static>(
        node_address: NodeAddress,
        network_magic: T,
        since_slot: Option<(u64, String)>,
        safe_block_depth: usize,
        event_filter: Option<Filter>,
        callback_fn: impl Fn(Event, PoolConnection<Postgres>) -> R + Send + Sync + 'static,
        retry_policy: RetryPolicy,
        database_url: String,
    ) -> Self {
        Self {
            node_address,
            network_magic,
            since_slot,
            safe_block_depth,
            event_filter,
            callback_fn: Arc::new(move |(ev, conn): (Event, PoolConnection<Postgres>)| {
                Box::pin(callback_fn(ev, conn))
            }),
            retry_policy,
            database_url,
        }
    }
}

// Encapsulating usage of deprecated stuff (impossible to construct struct without it).
// This avoids having to put "#![allow(deprecated)]" on the top of this file.
pub mod deprecation_usage {
    #![allow(deprecated)]

    use oura::mapper::Config as MapperConfig;
    use oura::sources::n2c::Config as N2CConfig;
    use oura::sources::n2n::Config as N2NConfig;
    use oura::sources::{AddressArg, IntersectArg, MagicArg, PointArg};

    pub fn n2c_config(
        addr: AddressArg,
        magic: MagicArg,
        since_slot: Option<(u64, String)>,
        safe_block_depth: usize,
    ) -> N2CConfig {
        N2CConfig {
            address: addr,
            magic: Some(magic),
            intersect: since_slot
                .map(|since_slot| IntersectArg::Point(PointArg(since_slot.0, since_slot.1))),
            mapper: MapperConfig {
                include_transaction_details: true,
                ..Default::default()
            },
            min_depth: safe_block_depth,
            retry_policy: None,
            finalize: None,
            // Deprecated fields
            since: None,
            well_known: None,
        }
    }

    pub fn n2n_config(
        addr: AddressArg,
        magic: MagicArg,
        since_slot: Option<(u64, String)>,
        safe_block_depth: usize,
    ) -> N2NConfig {
        N2NConfig {
            address: addr,
            magic: Some(magic),
            intersect: since_slot
                .map(|since_slot| IntersectArg::Point(PointArg(since_slot.0, since_slot.1))),
            mapper: MapperConfig {
                include_transaction_details: true,
                ..Default::default()
            },
            min_depth: safe_block_depth,
            retry_policy: None,
            finalize: None,
            // Deprecated fields
            since: None,
            well_known: None,
        }
    }
}

pub use self::deprecation_usage::*;
