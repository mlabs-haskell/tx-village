use crate::{
    config::{n2c_config, n2n_config, NodeAddress, TxIndexerConfig},
    handler::callback::{Callback, EventHandler},
    progress_tracker::ProgressTracker,
};
use anyhow::Result;
use oura::{
    pipelining::{FilterProvider, SinkProvider, SourceProvider},
    sources::{AddressArg, BearerKind},
    utils::{Utils, WithUtils},
    Error,
};
use std::sync::Arc;
use std::thread::JoinHandle;
use tracing::{span, Level};

// Structure holding the thread handles associated to the indexer. These threads are never-ending.
pub struct TxIndexer {
    pub source_handle: JoinHandle<()>,
    pub filter_handle: JoinHandle<()>,
    pub sink_handle: JoinHandle<()>,
}

impl TxIndexer {
    // This is based on: https://github.com/txpipe/oura/blob/27fb7e876471b713841d96e292ede40101b151d7/src/bin/oura/daemon.rs
    pub async fn run<H: EventHandler>(conf: TxIndexerConfig<H>) -> Result<TxIndexer, Error> {
        let span = span!(Level::INFO, "Run TxIndexer");
        let _enter = span.enter();

        let chain = conf.network.to_chain_info()?;

        let progress_tracker = match conf.since_slot {
            Some((since_slot, _)) => Some(ProgressTracker::new(since_slot, &chain)?),
            None => None,
        };

        let utils = Arc::new(Utils::new(chain));

        let (source_handle, source_rx) = match conf.node_address {
            NodeAddress::UnixSocket(path) => {
                span!(Level::INFO, "BootstrapSourceViaSocket", socket_path = path).in_scope(|| {
                    WithUtils::new(
                        n2c_config(
                            AddressArg(BearerKind::Unix, path),
                            conf.network.to_magic_arg(),
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
                            conf.network.to_magic_arg(),
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
            Callback::new(conf.handler, conf.retry_policy, utils, progress_tracker)
                .bootstrap(filter_rx)
        })?;

        Ok(TxIndexer {
            source_handle,
            filter_handle,
            sink_handle,
        })
    }
}
