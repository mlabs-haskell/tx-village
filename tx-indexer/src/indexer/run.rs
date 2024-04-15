use std::{fmt::Debug, sync::Arc};

use anyhow::Result;
use oura::{
  pipelining::{FilterProvider, SinkProvider, SourceProvider},
  sources::{AddressArg, BearerKind},
  utils::{ChainWellKnownInfo, Utils, WithUtils},
  Error,
};
use tracing::{span, Level};

use super::{
  callback::Callback,
  config::{n2c_config, n2n_config, IndexerConfig},
  error::ErrorPolicyProvider,
  types::{NetworkMagic, NodeAddress},
};

// This is based on: https://github.com/txpipe/oura/blob/27fb7e876471b713841d96e292ede40101b151d7/src/bin/oura/daemon.rs
pub fn run_indexer<E: Debug + ErrorPolicyProvider + 'static>(
  conf: IndexerConfig<E>,
) -> Result<(), Error> {
  let span = span!(Level::INFO, "run_indexer");
  let _enter = span.enter();

  let chain = match conf.network_magic {
    NetworkMagic::PREPROD => ChainWellKnownInfo::preprod(),
    NetworkMagic::PREVIEW => ChainWellKnownInfo::preview(),
    NetworkMagic::MAINNET => ChainWellKnownInfo::mainnet(),
  };
  let utils = Arc::new(Utils::new(chain));
  let (source_handle, source_rx) = match conf.node_address {
    NodeAddress::UnixSocket(path) => {
      span!(Level::INFO, "BootstrapSourceViaSocket", socket_path = path).in_scope(|| {
        WithUtils::new(
          {
            n2c_config(
              AddressArg(BearerKind::Unix, path),
              conf.network_magic,
              conf.since_slot,
              conf.safe_block_depth,
            )
          },
          utils.clone(),
        )
        .bootstrap()
      })
    }
    NodeAddress::TcpAddress(hostname, port) => {
      span!(Level::INFO, "BootstrapSourceViaTcp", hostname, port).in_scope(|| {
        WithUtils::new(
          {
            n2n_config(
              AddressArg(BearerKind::Tcp, format!("{}:{}", hostname, port)),
              conf.network_magic,
              conf.since_slot,
              conf.safe_block_depth,
            )
          },
          utils.clone(),
        )
        .bootstrap()
      })
    }
  }?;

  // Optionally create a filter handle (if filter was provided)
  let (filter_handle, next_rx) = match conf.event_filter {
    Some(ev_f) => {
      let (filter_handle, filter_rx) = ev_f.to_selection_config().bootstrap(source_rx)?;
      (Some(filter_handle), filter_rx)
    }
    None => (None, source_rx),
  };

  let sink_handle = span!(Level::INFO, "BootstrapSink").in_scope(|| {
    Callback {
      // Storing a thread-safe shareable pointer to the async function
      f: conf.callback_fn,
      retry_policy: conf.retry_policy,
      utils,
    }
    .bootstrap(next_rx)
  })?;

  sink_handle.join().map_err(|_| "error in sink thread")?;
  filter_handle.map_or(Ok(()), |h| h.join().map_err(|_| "error in sink thread"))?;
  source_handle.join().map_err(|_| "error in source thread")?;

  Ok(())
}
