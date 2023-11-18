use std::fmt::Debug;

use oura::filters::selection::Config as SelectionConfig;
use oura::model::Event;

use super::{
  error::ErrorPolicyProvider,
  retry::RetryPolicy,
  types::{NetworkMagic, NodeAddress},
};

pub struct IndexerConfig<E: Debug + ErrorPolicyProvider> {
  pub node_address: NodeAddress,
  pub network_magic: NetworkMagic,
  /// Slot number and hash as hex string
  pub since_slot: (u64, String),
  /// Minimum depth a block has to be from the tip for it to be considered "confirmed"
  /// See: https://oura.txpipe.io/v1/advanced/rollback_buffer
  pub safe_block_depth: usize,
  pub event_filter: SelectionConfig,
  /// Callback function to pass events to
  pub callback_fn: fn(&Event) -> Result<(), E>,
  /// Retry policy - how much to retry for each event callback failure
  /// This only takes effect on ErrorPolicy for a particular error is `Retry`.
  /// Once retries are exhausted, the handler will error (same treatment as ErrorPolicy::Exit)
  pub retry_policy: RetryPolicy,
}

macro_rules! source_conf {
  ( $constr:tt, $addr:expr, $magic:expr, $since_slot:expr, $safe_block_depth:expr ) => {
    $constr {
      address: $addr,
      magic: Some($magic.to_magic_arg()),
      intersect: Some(IntersectArg::Point(PointArg($since_slot.0, $since_slot.1))),
      mapper: Default::default(),
      min_depth: $safe_block_depth,
      retry_policy: None,
      finalize: None,
      // Deprecated fields
      since: None,
      well_known: None,
    }
  };
}

pub(crate) use source_conf;
