use super::{
    callback::{Callback, Handler},
    config::{n2c_config, n2n_config, IndexerConfig},
    types::{Indexer, IsNetworkConfig, NodeAddress},
};
use anyhow::Result;
use oura::{
    pipelining::{FilterProvider, SinkProvider, SourceProvider},
    sources::{AddressArg, BearerKind},
    utils::{Utils, WithUtils},
    Error,
};
use sqlx::PgPool;
use std::sync::Arc;
use tracing::{span, Level};

// This is based on: https://github.com/txpipe/oura/blob/27fb7e876471b713841d96e292ede40101b151d7/src/bin/oura/daemon.rs
pub async fn run_indexer<H: Handler, T: IsNetworkConfig>(
    conf: IndexerConfig<H, T>,
) -> Result<Indexer, Error> {
    let span = span!(Level::INFO, "run_indexer");
    let _enter = span.enter();

    let chain = conf.network_config.to_chain_info();
    let utils = Arc::new(Utils::new(chain));
    let (source_handle, source_rx) = match conf.node_address {
        NodeAddress::UnixSocket(path) => {
            span!(Level::INFO, "BootstrapSourceViaSocket", socket_path = path).in_scope(|| {
                WithUtils::new(
                    n2c_config(
                        AddressArg(BearerKind::Unix, path),
                        conf.network_config.to_magic_arg(),
                        conf.since_slot,
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
                        conf.since_slot,
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

    let pg_pool = PgPool::connect(&conf.database_url).await?;

    let sink_handle = span!(Level::INFO, "BootstrapSink").in_scope(|| {
        Callback::new(conf.handler, conf.retry_policy, utils, pg_pool).bootstrap(filter_rx)
    })?;

    Ok(Indexer {
        source_handle,
        filter_handle,
        sink_handle,
    })
}
