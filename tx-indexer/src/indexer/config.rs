use super::{
    callback::Handler,
    filter::Filter,
    retry::RetryPolicy,
    types::{IsNetworkConfig, NodeAddress},
};

pub struct IndexerConfig<H: Handler, T: IsNetworkConfig> {
    pub handler: H,
    pub node_address: NodeAddress,
    pub network_config: T,
    /// Slot number and hash as hex string (optional).
    /// If not provided, sync will begin from the tip of the chain.
    pub since_slot: Option<(u64, String)>,
    /// Minimum depth a block has to be from the tip for it to be considered "confirmed"
    /// See: https://oura.txpipe.io/v1/advanced/rollback_buffer
    pub safe_block_depth: usize,
    pub event_filter: Option<Filter>,
    /// Retry policy - how much to retry for each event callback failure
    /// This only takes effect on ErrorPolicy for a particular error is `Retry`.
    /// Once retries are exhausted, the handler will error (same treatment as ErrorPolicy::Exit)
    pub retry_policy: RetryPolicy,
    /// Postgres database URL
    pub database_url: String,
}

impl<H: Handler, T: IsNetworkConfig> IndexerConfig<H, T> {
    pub fn new(
        handler: H,
        node_address: NodeAddress,
        network_config: T,
        since_slot: Option<(u64, String)>,
        safe_block_depth: usize,
        event_filter: Option<Filter>,
        retry_policy: RetryPolicy,
        database_url: String,
    ) -> Self {
        Self {
            handler,
            node_address,
            network_config,
            since_slot,
            safe_block_depth,
            event_filter,
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
