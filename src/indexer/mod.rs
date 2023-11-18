use std::default::Default;
use std::fmt::Debug;
use std::ops::Mul;
use std::sync::Arc;
use std::time::Duration;

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
  callback_fn: fn(&Event) -> Result<(), E>,
  /// Retry policy - how much to retry for each event callback failure
  /// This only takes effect on ErrorPolicy for a particular error is `Retry`.
  /// Once retries are exhausted, the handler will error (same treatment as ErrorPolicy::Exit)
  retry_policy: RetryPolicy,
}

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

// TODO(chase): Implement custom callback based sink
struct Callback<E> {
  f: fn(&Event) -> Result<(), E>,
  retry_policy: RetryPolicy,
  utils: Arc<Utils>,
}

impl<E: Debug + ErrorPolicyProvider + 'static> SinkProvider for Callback<E> {
  fn bootstrap(&self, input: StageReceiver) -> BootstrapResult {
    let callback_fn = self.f.clone();
    let retry_policy = self.retry_policy;
    let utils = self.utils.clone();
    let handle = std::thread::spawn(move || {
      handle_event(input, callback_fn, &retry_policy, utils).expect("request loop failed")
    });

    Ok(handle)
  }
}

#[derive(Debug, Copy, Clone)]
pub struct RetryPolicy {
  pub max_retries: u32,
  pub backoff_unit: Duration,
  pub backoff_factor: u32,
  pub max_backoff: Duration,
}

impl Default for RetryPolicy {
  fn default() -> Self {
    Self {
      max_retries: 20,
      backoff_unit: Duration::from_millis(5_000),
      backoff_factor: 2,
      max_backoff: Duration::from_millis(20 * 5_000),
    }
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
  fn get_error_policy(&self) -> ErrorPolicy<Self>;
}

fn compute_backoff_delay(policy: &RetryPolicy, retry: u32) -> Duration {
  let units = policy.backoff_factor.pow(retry);
  let backoff = policy.backoff_unit.mul(units);
  core::cmp::min(backoff, policy.max_backoff)
}

fn handle_event<E: Debug + ErrorPolicyProvider>(
  input: StageReceiver,
  callback_fn: impl Fn(&Event) -> Result<(), E>,
  retry_policy: &RetryPolicy,
  utils: Arc<Utils>,
) -> Result<(), E> {
  for event in input.iter() {
    let mut retry = 0;

    // The retry logic is based on: https://github.com/txpipe/oura/blob/27fb7e876471b713841d96e292ede40101b151d7/src/utils/retry.rs
    loop {
      let result = callback_fn(&event);

      match result {
        Ok(_) => {
          // notify progress to the pipeline
          utils.track_sink_progress(&event);
          break;
        }
        Err(err) => match err.get_error_policy() {
          ErrorPolicy::Exit => return Err(err),
          ErrorPolicy::Skip => {
            log::warn!("failed to send webhook request: {:?}", err);
            break;
          }
          ErrorPolicy::Call(err_f) => {
            err_f(err);
            break;
          }
          ErrorPolicy::Retry if retry < retry_policy.max_retries => {
            log::warn!("retryable operation error: {:?}", err);

            retry += 1;

            let backoff = compute_backoff_delay(&retry_policy, retry);

            log::debug!(
              "backoff for {}s until next retry #{}",
              backoff.as_secs(),
              retry
            );

            std::thread::sleep(backoff);
          }
          _ => return Err(err),
        },
      }
    }
  }

  Ok(())
}
