use super::{
    callback::{Callback, Handler},
    config::{n2c_config, n2n_config, IndexerConfig},
    retry::ProgressTracker,
    types::{Indexer, IsNetworkConfig, NodeAddress},
};
use anyhow::{anyhow, Result};
use chrono::{DateTime, Duration, Utc};
use oura::{
    pipelining::{FilterProvider, SinkProvider, SourceProvider},
    sources::{AddressArg, BearerKind},
    utils::{ChainWellKnownInfo, Utils, WithUtils},
    Error,
};
use std::sync::{atomic::AtomicUsize, Arc};
use tracing::{span, Level};
use tx_bakery::chain_query::{EraParameters, EraSummary, EraTime};

// This is based on: https://github.com/txpipe/oura/blob/27fb7e876471b713841d96e292ede40101b151d7/src/bin/oura/daemon.rs
pub async fn run_indexer<H: Handler, T: IsNetworkConfig>(
    conf: IndexerConfig<H, T>,
) -> Result<Indexer, Error> {
    let span = span!(Level::INFO, "run_indexer");
    let _enter = span.enter();

    let chain = conf.network_config.to_chain_info();

    let progress_tracker = match conf.since_slot {
        Some((since_slot, _)) => {
            let system_start = DateTime::from_timestamp(chain.byron_known_time as i64, 0).ok_or(
                anyhow!("Unable to convert shelley_known_time to to DateTime"),
            )?;

            Some(ProgressTracker {
                system_start,
                era_summaries: chain_info_to_era_summaries(&system_start, &chain)?,
                since_slot,
                sync_status: Arc::new(AtomicUsize::new(0)),
            })
        }
        None => None,
    };

    let utils = Arc::new(Utils::new(chain));

    let (source_handle, source_rx) = match conf.node_address {
        NodeAddress::UnixSocket(path) => {
            span!(Level::INFO, "BootstrapSourceViaSocket", socket_path = path).in_scope(|| {
                WithUtils::new(
                    n2c_config(
                        AddressArg(BearerKind::Unix, path),
                        conf.network_config.to_magic_arg(),
                        conf.since_slot.clone(),
                        conf.safe_block_depth,
                    ),
                    utils.clone(),
                )
                .bootstrap()
            })
        }
        NodeAddress::TcpAddress(hostname, port) => {
            span!(Level::INFO, "BootstrapSourceViaTcp", hostname, port).in_scope(|| {
                WithUtils::new(
                    n2n_config(
                        AddressArg(BearerKind::Tcp, format!("{}:{}", hostname, port)),
                        conf.network_config.to_magic_arg(),
                        conf.since_slot.clone(),
                        conf.safe_block_depth,
                    ),
                    utils.clone(),
                )
                .bootstrap()
            })
        }
    }?;

    // Optionally create a filter handle (if filter was provided)
    let (filter_handle, filter_rx) = conf
        .event_filter
        .to_selection_config()
        .bootstrap(source_rx)?;

    let sink_handle = span!(Level::INFO, "BootstrapSink").in_scope(|| {
        Callback::new(conf.handler, conf.retry_policy, utils, progress_tracker).bootstrap(filter_rx)
    })?;

    Ok(Indexer {
        source_handle,
        filter_handle,
        sink_handle,
    })
}

fn chain_info_to_era_summaries(
    system_start_time: &DateTime<Utc>,
    chain_info: &ChainWellKnownInfo,
) -> Result<Vec<EraSummary>, anyhow::Error> {
    let byron_start = EraTime {
        time: Duration::zero(),
        slot: 0,
        epoch: 0,
    };

    let shelley_start = EraTime {
        time: DateTime::from_timestamp(chain_info.shelley_known_time as i64, 0).ok_or(anyhow!(
            "Unable to convert shelley_known_time to to DateTime"
        ))? - system_start_time,
        slot: chain_info.shelley_known_slot,
        epoch: chain_info.shelley_known_slot / chain_info.byron_epoch_length as u64,
    };

    Ok(vec![
        EraSummary {
            start: byron_start,
            end: Some(shelley_start.clone()),
            parameters: EraParameters {
                epoch_length: chain_info.byron_epoch_length as u64,
                slot_length: chain_info.byron_slot_length as u64 * 1000,
                safe_zone: Some(4320),
            },
        },
        EraSummary {
            start: shelley_start,
            end: None,
            parameters: EraParameters {
                epoch_length: chain_info.shelley_epoch_length as u64,
                slot_length: chain_info.shelley_slot_length as u64 * 1000,
                safe_zone: Some(4320),
            },
        },
    ])
}
