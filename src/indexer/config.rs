use std::{future::Future, pin::Pin, sync::Arc};

use oura::model::Event;

use super::{
  filter::Filter,
  retry::RetryPolicy,
  types::{NetworkMagic, NodeAddress},
};

pub struct IndexerConfig<E> {
  pub node_address: NodeAddress,
  pub network_magic: NetworkMagic,
  /// Slot number and hash as hex string (optional).
  /// If not provided, sync will begin from the tip of the chain.
  pub since_slot: Option<(u64, String)>,
  /// Minimum depth a block has to be from the tip for it to be considered "confirmed"
  /// See: https://oura.txpipe.io/v1/advanced/rollback_buffer
  pub safe_block_depth: usize,
  pub event_filter: Filter,
  /// Callback function to pass events to
  pub callback_fn:
    Arc<dyn Fn(Event) -> Pin<Box<dyn Future<Output = Result<(), E>> + Send + Sync>> + Send + Sync>,
  /// Retry policy - how much to retry for each event callback failure
  /// This only takes effect on ErrorPolicy for a particular error is `Retry`.
  /// Once retries are exhausted, the handler will error (same treatment as ErrorPolicy::Exit)
  pub retry_policy: RetryPolicy,
}

// Encapsulating usage of deprecated stuff (impossible to construct struct without it).
// This avoids having to put "#![allow(deprecated)]" on the top of this file.
pub mod deprecation_usage {
  #![allow(deprecated)]

  use oura::sources::n2c::Config as N2CConfig;
  use oura::sources::n2c::Config as N2NConfig;
  use oura::sources::{AddressArg, IntersectArg, PointArg};

  use super::super::types::NetworkMagic;

  pub fn n2c_config(
    addr: AddressArg,
    magic: NetworkMagic,
    since_slot: Option<(u64, String)>,
    safe_block_depth: usize,
  ) -> N2CConfig {
    N2CConfig {
      address: addr,
      magic: Some(magic.to_magic_arg()),
      intersect: since_slot
        .map(|since_slot| IntersectArg::Point(PointArg(since_slot.0, since_slot.1))),
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
    since_slot: Option<(u64, String)>,
    safe_block_depth: usize,
  ) -> N2NConfig {
    N2NConfig {
      address: addr,
      magic: Some(magic.to_magic_arg()),
      intersect: since_slot
        .map(|since_slot| IntersectArg::Point(PointArg(since_slot.0, since_slot.1))),
      mapper: Default::default(),
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
