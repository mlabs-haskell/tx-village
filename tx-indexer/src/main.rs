use anyhow::Result;
use aux::ParseCurrencySymbol;
use clap::Parser;
use oura::model::Event;
use sqlx::PgConnection;
use std::{default::Default, fmt::Debug};
use thiserror::Error;
use tracing::Level;
use tx_indexer::indexer::{
    callback::Handler,
    config::IndexerConfig,
    error::{ErrorPolicy, ErrorPolicyProvider},
    filter::Filter,
    run_indexer,
    types::{NetworkConfig, NetworkName, NodeAddress},
};

mod aux;
mod handler;

#[derive(Debug, Parser)]
struct IndexStartArgs {
    /// Cardano node socket path
    #[arg(long)]
    socket_path: String,

    /// Network name (preprod | preview | mainnet)
    #[arg(
        short('n'),
        long,
        value_parser = clap::value_parser!(NetworkName),
        required_unless_present="network",
        conflicts_with="network_magic"
    )]
    network: Option<NetworkName>,

    /// Network magic number
    #[arg(
        short('m'),
        long("magic"),
        requires = "node_config_path",
        conflicts_with = "network_magic"
    )]
    network_magic: Option<u64>,

    /// Cardano node configuration path
    #[arg(long)]
    node_config_path: Option<String>,

    /// Sync from this slot
    #[arg(short, long, requires = "since_block_hash")]
    since_slot: Option<u64>,

    /// Sync from this block hash
    #[arg(short('a'), long, requires = "since_slot")]
    since_block_hash: Option<String>,

    /// Filter for transactions minting this currency symbol (multiple allowed)
    #[arg(short('c'), long = "curr_symbol")]
    curr_symbols: Vec<ParseCurrencySymbol>,

    /// PostgreSQL database URL
    #[arg(long)]
    database_url: String,
}

#[derive(clap::Subcommand, Debug)]
enum IndexCommand {
    /// Start the indexer
    Start(IndexStartArgs),
}

#[derive(clap::Subcommand, Debug)]
enum Command {
    /// Run the Index command
    #[command(subcommand)]
    Index(IndexCommand),
}

/// Infinity Query command line interface
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,

    #[arg(long, short)]
    debug: bool,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    // Set up tracing logger (logs to stdout).
    let collector = tracing_subscriber::fmt()
        .with_max_level(if args.debug {
            Level::DEBUG
        } else {
            Level::INFO
        })
        // build but do not install the subscriber.
        .finish();
    tracing::subscriber::set_global_default(collector)?;

    match args.command {
        Command::Index(index_cmd) => match index_cmd {
            IndexCommand::Start(IndexStartArgs {
                socket_path,
                network,
                network_magic,
                node_config_path,
                since_slot,
                since_block_hash,
                curr_symbols,
                database_url,
            }) => {
                let indexer = match (network, network_magic) {
                    (_, Some(x)) => {
                        run_indexer(IndexerConfig::new(
                            DummyHandler,
                            NodeAddress::UnixSocket(socket_path),
                            NetworkConfig {
                                magic: x,
                                node_config_path: node_config_path.unwrap(),
                            },
                            since_slot.zip(since_block_hash),
                            4,
                            if curr_symbols.is_empty() {
                                None
                            } else {
                                Some(Filter {
                                    curr_symbols: curr_symbols
                                        .into_iter()
                                        .map(|ParseCurrencySymbol(cur_sym)| cur_sym)
                                        .collect(),
                                })
                            },
                            Default::default(),
                            database_url,
                        ))
                        .await
                    }
                    (Some(x), _) => {
                        run_indexer(IndexerConfig::new(
                            DummyHandler,
                            NodeAddress::UnixSocket(socket_path),
                            x,
                            since_slot.zip(since_block_hash),
                            4,
                            if curr_symbols.is_empty() {
                                None
                            } else {
                                Some(Filter {
                                    curr_symbols: curr_symbols
                                        .into_iter()
                                        .map(|ParseCurrencySymbol(cur_sym)| cur_sym)
                                        .collect(),
                                })
                            },
                            Default::default(),
                            database_url,
                        ))
                        .await
                    }
                    _ => panic!("absurd: Clap did not parse any network magic arg"),
                }?;
                indexer
                    .sink_handle
                    .join()
                    .map_err(|_| "error in sink thread")?;
                indexer
                    .filter_handle
                    .map_or(Ok(()), |h| h.join().map_err(|_| "error in sink thread"))?;
                indexer
                    .source_handle
                    .join()
                    .map_err(|_| "error in source thread")?;

                Ok(())
            }
        },
    }
}

#[derive(Debug)]
struct Error();

impl ErrorPolicyProvider for Error {
    fn get_error_policy(&self) -> ErrorPolicy<Self> {
        ErrorPolicy::Skip
    }
}

#[derive(Error, Debug)]
pub(crate) enum DummyHandlerError {}

impl ErrorPolicyProvider for DummyHandlerError {
    fn get_error_policy(&self) -> ErrorPolicy<Self> {
        ErrorPolicy::Skip
    }
}

// TODO(chase): Enhance dummy callback
#[derive(Clone)]
struct DummyHandler;

impl Handler for DummyHandler {
    type Error = DummyHandlerError;

    async fn handle<'a>(
        &self,
        _event: Event,
        _pg_connection: &'a mut PgConnection,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}
