mod callback;
pub mod config;
pub mod error;
pub mod retry;
pub mod types;

use std::default::Default;
use std::fmt::Debug;
use std::sync::Arc;

use anyhow::Result;

use oura::pipelining::{FilterProvider, SourceProvider};
use oura::sources::n2c::Config as N2CConfig;
use oura::sources::n2n::Config as N2NConfig;
use oura::{
  pipelining::SinkProvider,
  sources::{AddressArg, BearerKind, IntersectArg, PointArg},
  utils::{ChainWellKnownInfo, Utils, WithUtils},
  Error,
};

use self::{
  callback::Callback,
  config::{source_conf, IndexerConfig},
  error::ErrorPolicyProvider,
  types::{NetworkMagic, NodeAddress},
};

// This is based on: https://github.com/txpipe/oura/blob/27fb7e876471b713841d96e292ede40101b151d7/src/bin/oura/daemon.rs
pub fn run_indexer<E: Debug + ErrorPolicyProvider + 'static>(
  conf: IndexerConfig<E>,
) -> Result<(), Error> {
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
    retry_policy: conf.retry_policy,
    utils,
  }
  .bootstrap(filter_rx)?;
  threads.push(sink_handle);

  for handle in threads {
    handle.join().expect("error in pipeline thread");
  }

  Ok(())
}
