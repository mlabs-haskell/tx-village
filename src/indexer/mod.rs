use std::default::Default;
use std::fmt::Debug;
use std::sync::Arc;

use anyhow::Result;
use oura::{
  model::Event,
  pipelining::{BootstrapResult, SinkProvider, StageReceiver},
  sources::{AddressArg, BearerKind, IntersectArg, MagicArg, PointArg},
  utils::{ChainWellKnownInfo, Utils, WithUtils, PREPROD_MAGIC, PREVIEW_MAGIC},
  Error,
};
use pallas::network::miniprotocols::MAINNET_MAGIC;

use oura::filters::selection::Config as SelectionConfig;
use oura::pipelining::{FilterProvider, SourceProvider};
use oura::sources::n2c::Config as N2CConfig;
use oura::sources::n2n::Config as N2NConfig;

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

pub enum NodeAddress {
  /// Path to Unix node.socket
  UnixSocket(String),
  // Hostname and port number for TCP connection to remote node
  TcpAddress(String, u64),
}

pub enum NetworkMagic {
  PREPROD,
  PREVIEW,
  MAINNET,
}

impl NetworkMagic {
  pub fn to_magic_arg(self) -> MagicArg {
    MagicArg(match self {
      NetworkMagic::PREPROD => PREPROD_MAGIC,
      NetworkMagic::PREVIEW => PREVIEW_MAGIC,
      NetworkMagic::MAINNET => MAINNET_MAGIC,
    })
  }
}

pub struct IndexerConfig<E: Debug + ErrorPolicyProvider> {
  node_address: NodeAddress,
  network_magic: NetworkMagic,
  /// Slot number and hash as hex string
  since_slot: (u64, String),
  /// Minimum depth a block has to be from the tip for it to be considered "confirmed"
  /// See: https://oura.txpipe.io/v1/advanced/rollback_buffer
  safe_block_depth: usize,
  event_filter: SelectionConfig,
  /// Callback function to pass events to
  callback_fn: fn(Event) -> Result<(), E>,
}

// This is based on: https://github.com/txpipe/oura/blob/27fb7e876471b713841d96e292ede40101b151d7/src/bin/oura/daemon.rs
pub fn run_indexer<E: Debug + ErrorPolicyProvider>(conf: IndexerConfig<E>) -> Result<(), Error> {
  let chain = match conf.network_magic {
    NetworkMagic::PREPROD => ChainWellKnownInfo::preprod(),
    NetworkMagic::PREVIEW => ChainWellKnownInfo::preview(),
    NetworkMagic::MAINNET => ChainWellKnownInfo::mainnet(),
  };
  let utils = Arc::new(Utils::new(chain));
  let (source_handle, source_rx) = match conf.node_address {
    NodeAddress::UnixSocket(path) => WithUtils::new(
      {
        source_conf!(
          N2CConfig,
          AddressArg(BearerKind::Unix, path),
          conf.network_magic,
          conf.since_slot,
          conf.safe_block_depth
        )
      },
      utils.clone(),
    )
    .bootstrap(),
    NodeAddress::TcpAddress(hostname, port) => WithUtils::new(
      {
        source_conf!(
          N2NConfig,
          AddressArg(BearerKind::Tcp, format!("{}:{}", hostname, port)),
          conf.network_magic,
          conf.since_slot,
          conf.safe_block_depth
        )
      },
      utils.clone(),
    )
    .bootstrap(),
  }?;

  let mut threads = Vec::with_capacity(10);
  threads.push(source_handle);

  let (filter_handle, filter_rx) = conf.event_filter.bootstrap(source_rx)?;
  threads.push(filter_handle);

  let sink_handle = Callback {
    f: conf.callback_fn,
    utils,
  }
  .bootstrap(filter_rx)?;
  threads.push(sink_handle);

  for handle in threads {
    handle.join().expect("error in pipeline thread");
  }

  Ok(())
}

// TODO(chase): Implement custom callback based sink
struct Callback<E: Debug + ErrorPolicyProvider> {
  f: fn(Event) -> Result<(), E>,
  utils: Arc<Utils>,
}

impl<E: Debug + ErrorPolicyProvider> SinkProvider for Callback<E> {
  fn bootstrap(&self, input: StageReceiver) -> BootstrapResult {
    todo!("Implement custom sink")
  }
}

pub enum ErrorPolicy<E> {
  Retry,
  Skip,
  Exit,
  Call(fn(E) -> ()),
}

pub trait ErrorPolicyProvider
where
  Self: Sized,
{
  fn get_error_policy(self) -> ErrorPolicy<Self>;
}
