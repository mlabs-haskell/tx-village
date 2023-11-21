use oura::sources::n2c::Config as N2CConfig;
use oura::sources::n2c::Config as N2NConfig;
use oura::{
  filters::selection::Config as SelectionConfig,
  model::Event,
  sources::{AddressArg, IntersectArg, PointArg},
};

use super::{
  retry::RetryPolicy,
  types::{NetworkMagic, NodeAddress},
};

pub struct IndexerConfig<Fut> {
  pub node_address: NodeAddress,
  pub network_magic: NetworkMagic,
  /// Slot number and hash as hex string
  pub since_slot: (u64, String),
  /// Minimum depth a block has to be from the tip for it to be considered "confirmed"
  /// See: https://oura.txpipe.io/v1/advanced/rollback_buffer
  pub safe_block_depth: usize,
  pub event_filter: SelectionConfig,
  /// Callback function to pass events to
  pub callback_fn: fn(&Event) -> Fut,
  /// Retry policy - how much to retry for each event callback failure
  /// This only takes effect on ErrorPolicy for a particular error is `Retry`.
  /// Once retries are exhausted, the handler will error (same treatment as ErrorPolicy::Exit)
  pub retry_policy: RetryPolicy,
}

pub fn n2c_config(
  addr: AddressArg,
  magic: NetworkMagic,
  since_slot: (u64, String),
  safe_block_depth: usize,
) -> N2CConfig {
  N2CConfig {
    address: addr,
    magic: Some(magic.to_magic_arg()),
    intersect: Some(IntersectArg::Point(PointArg(since_slot.0, since_slot.1))),
    mapper: Default::default(),
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
  magic: NetworkMagic,
  since_slot: (u64, String),
  safe_block_depth: usize,
) -> N2NConfig {
  N2NConfig {
    address: addr,
    magic: Some(magic.to_magic_arg()),
    intersect: Some(IntersectArg::Point(PointArg(since_slot.0, since_slot.1))),
    mapper: Default::default(),
    min_depth: safe_block_depth,
    retry_policy: None,
    finalize: None,
    // Deprecated fields
    since: None,
    well_known: None,
  }
}
