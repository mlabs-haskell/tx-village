use anyhow::Result;
use clap::Parser;
use sqlx::PgPool;
use std::{default::Default, fmt::Debug};
use tracing::Level;
use tx_db::handler::TxIndexerHandler;
use tx_indexer::{
    aux::ParseCurrencySymbol,
    config::{NetworkConfig, NetworkName, NodeAddress, TxIndexerConfig},
    filter::Filter,
    TxIndexer,
};

mod tx_db;

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
        conflicts_with = "network"
    )]

    /// Network identified by magic number and chain info file
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
    postgres_url: String,
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

    #[arg(long, short, global = true)]
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
                postgres_url,
            }) => {
                let network_config = network_magic
                    .map(|magic| NetworkConfig::ConfigPath {
                        magic,
                        node_config_path: node_config_path.unwrap(),
                    })
                    .or(network.map(NetworkConfig::WellKnown))
                    .unwrap();

                let pg_pool = PgPool::connect(&postgres_url).await?;

                let handler = TxIndexerHandler::new(pg_pool);

                let indexer = TxIndexer::run(TxIndexerConfig::new(
                    handler,
                    NodeAddress::UnixSocket(socket_path),
                    network_config,
                    since_slot.zip(since_block_hash),
                    4,
                    Filter {
                        curr_symbols: curr_symbols
                            .into_iter()
                            .map(|ParseCurrencySymbol(cur_sym)| cur_sym)
                            .collect(),
                    },
                    Default::default(),
                ))
                .await?;
                indexer
                    .sink_handle
                    .join()
                    .map_err(|_| "error in sink thread")?;
                indexer
                    .filter_handle
                    .join()
                    .map_err(|_| "error in sink thread")?;
                indexer
                    .source_handle
                    .join()
                    .map_err(|_| "error in source thread")?;

                Ok(())
            }
        },
    }
}
