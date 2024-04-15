mod handler;

use std::{default::Default, fmt::Debug};

use anyhow::Result;
use clap::Parser;
use oura::model::Event;
use tracing::Level;
use tx_bakery::utils::plutip::{Plutip, PlutipConfigBuilder};
use tx_indexer::indexer::{
    config::IndexerConfig,
    error::{ErrorPolicy, ErrorPolicyProvider},
    filter::Filter,
    run_indexer,
    types::{NetworkMagic, NodeAddress},
};

#[derive(Debug, Parser)]
struct IndexStartArgs {
    socket_path: String,
    #[arg(value_parser = clap::value_parser!(NetworkMagic))]
    network_magic: NetworkMagic,
    #[arg(short, long)]
    since_slot: Option<u64>,
    #[arg(short('a'), long)]
    since_slot_hash: Option<String>,
    #[arg(short('c'), long)]
    curr_symbols: Vec<String>,
}

#[derive(clap::Subcommand, Debug)]
enum IndexCommand {
    /// Start the indexer
    Start(IndexStartArgs),
}

#[derive(Debug, Parser)]
struct DummyCommand {}

#[derive(clap::Subcommand, Debug)]
enum Command {
    /// Run the Index command
    #[command(subcommand)]
    Index(IndexCommand),
    // Run indexer with Plutip local cluster
    Dummy(DummyCommand),
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
                network_magic,
                since_slot,
                since_slot_hash,
                curr_symbols,
            }) => run_indexer(IndexerConfig::new(
                NodeAddress::UnixSocket(socket_path),
                network_magic,
                since_slot.zip(since_slot_hash),
                4,
                if curr_symbols.is_empty() {
                    None
                } else {
                    Some(Filter { curr_symbols })
                },
                dummy_callback,
                Default::default(),
            )),
        },
        Command::Dummy(_) => {
            let plutip_conf = PlutipConfigBuilder::default()
                .wallets(2)
                ._lovelace(1_000_000)
                .slot_length(0.1)
                .build()?;
            let plutip = Plutip::start(&plutip_conf)
                .await
                .expect("Plutip cannot be spawned");

            run_indexer(IndexerConfig::new(
                NodeAddress::UnixSocket(plutip.get_node_socket()),
                NetworkMagic::MAINNET,
                None,
                4,
                None,
                dummy_callback,
                Default::default(),
            ))
            .expect("Failed to spawn indexer");

            Ok(())
        }
    }
}

#[derive(Debug)]
pub struct Error();

impl ErrorPolicyProvider for Error {
    fn get_error_policy(&self) -> ErrorPolicy<Self> {
        ErrorPolicy::Skip
    }
}

// TODO(chase): Enhance dummy callback
async fn dummy_callback(_ev: Event) -> Result<(), Error> {
    Ok(())
}
