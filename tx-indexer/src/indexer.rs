use std::thread::JoinHandle;
use std::{path::PathBuf, sync::Arc};

use anyhow::{anyhow, Result};
use oura::{
    pipelining::{FilterProvider, SinkProvider, SourceProvider},
    sources::{AddressArg, BearerKind},
    utils::{Utils, WithUtils},
    Error,
};
use tracing::{span, Level};

use crate::{
    config::{
        n2c_config, n2n_config, NetworkConfig, NodeAddress, TxIndexerConfig, TxIndexerSource,
    },
    filter::Filter,
    handler::{
        callback::{Callback, EventHandler},
        retry::RetryPolicy,
    },
    progress_tracker::ProgressTracker,
};

// Structure holding the thread handles associated to the indexer. These threads are never-ending.
pub enum TxIndexer {
    CardanoNode {
        source_handle: JoinHandle<()>,
        filter_handle: JoinHandle<()>,
        sink_handle: JoinHandle<()>,
    },

    FixtureFiles {
        handle: JoinHandle<()>,
    },
}

impl TxIndexer {
    // This is based on: https://github.com/txpipe/oura/blob/27fb7e876471b713841d96e292ede40101b151d7/src/bin/oura/daemon.rs
    pub async fn run<H: EventHandler>(
        conf: TxIndexerConfig<H>,
    ) -> Result<TxIndexer, anyhow::Error> {
        let span = span!(Level::INFO, "Run TxIndexer");
        let _enter = span.enter();

        match conf.source {
            TxIndexerSource::CardanoNode {
                node_address,
                network,
                since_slot,
                safe_block_depth,
                event_filter,
            } => source_from_cardano_node(
                conf.handler,
                node_address,
                network,
                since_slot,
                safe_block_depth,
                event_filter,
                conf.retry_policy,
            )
            .map_err(|err| anyhow!(err.to_string())),

            TxIndexerSource::FixtureFiles { dir_path } => {
                source_from_files(conf.handler, dir_path).await
            }
        }
    }

    pub fn join(self) -> Result<(), anyhow::Error> {
        match self {
            TxIndexer::CardanoNode {
                source_handle,
                filter_handle,
                sink_handle,
            } => {
                sink_handle
                    .join()
                    .map_err(|err| anyhow!("error in sink thread: {}", any_err_to_string(err)))?;
                filter_handle
                    .join()
                    .map_err(|err| anyhow!("error in filter thread: {}", any_err_to_string(err)))?;
                source_handle
                    .join()
                    .map_err(|err| anyhow!("error in source thread: {}", any_err_to_string(err)))?;
            }
            TxIndexer::FixtureFiles { handle } => handle
                .join()
                .map_err(|err| anyhow!("error in thread: {}", any_err_to_string(err)))?,
        }
        Ok(())
    }
}

fn any_err_to_string(err: Box<dyn std::any::Any>) -> String {
    if let Some(str) = err.downcast_ref::<String>() {
        String::from(str)
    } else {
        String::from("Cannot print")
    }
}

fn source_from_cardano_node(
    handler: impl EventHandler,
    node_address: NodeAddress,
    network: NetworkConfig,
    since_slot: Option<(u64, String)>,
    safe_block_depth: usize,
    event_filter: Filter,
    retry_policy: RetryPolicy,
) -> Result<TxIndexer, Error> {
    let chain = network.to_chain_info()?;

    let progress_tracker = match since_slot {
        Some((since_slot, _)) => Some(ProgressTracker::new(since_slot, &chain)?),
        None => None,
    };

    let utils = Arc::new(Utils::new(chain));

    let (source_handle, source_rx) = match node_address {
        NodeAddress::UnixSocket(path) => {
            span!(Level::INFO, "BootstrapSourceViaSocket", socket_path = path).in_scope(|| {
                WithUtils::new(
                    n2c_config(
                        AddressArg(BearerKind::Unix, path),
                        network.to_magic_arg(),
                        since_slot.clone(),
                        safe_block_depth,
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
                        network.to_magic_arg(),
                        since_slot.clone(),
                        safe_block_depth,
                    ),
                    utils.clone(),
                )
                .bootstrap()
            })
        }
    }?;

    // Optionally create a filter handle (if filter was provided)
    let (filter_handle, filter_rx) = event_filter.to_selection_config().bootstrap(source_rx)?;

    let sink_handle = span!(Level::INFO, "BootstrapSink").in_scope(|| {
        Callback::new(handler, retry_policy, utils, progress_tracker).bootstrap(filter_rx)
    })?;

    Ok(TxIndexer::CardanoNode {
        source_handle,
        filter_handle,
        sink_handle,
    })
}

async fn source_from_files(
    handler: impl EventHandler,
    dir_path: PathBuf,
) -> Result<TxIndexer, anyhow::Error> {
    use futures::stream::{StreamExt, TryStreamExt};
    use tokio::fs;
    use tokio::runtime::Runtime;
    use tokio_stream::wrappers::ReadDirStream;

    let file_stream = ReadDirStream::new(fs::read_dir(dir_path).await.map_err(|err| anyhow!(err))?);

    let handle = std::thread::spawn(|| {
        let rt = Runtime::new().unwrap();
        rt.block_on(async move {
            let handler = &handler;
            let _: Vec<()> = file_stream
                .try_filter_map(|dir_entry| async move {
                    let path = dir_entry.path();

                    if let Some(ext) = path.extension() {
                        if ext == "json" {
                            return Ok(Some(path));
                        }
                    };
                    Ok(None)
                })
                .then(|path| async move {
                    let path = path.map_err(|err| anyhow!(err))?;
                    let bytes = fs::read(path).await.map_err(|err| anyhow!(err))?;

                    let chain_event = serde_json::from_slice(&bytes).map_err(|err| anyhow!(err))?;

                    handler
                        .handle(chain_event)
                        .await
                        .map_err(|err| anyhow!(err.to_string()))
                })
                .try_collect()
                .await
                .unwrap();
        })
    });

    Ok(TxIndexer::FixtureFiles { handle })
}
