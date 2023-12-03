mod handler;

use std::{default::Default, fmt::Debug, sync::Arc};

use anyhow::Result;
use clap::Parser;
use infinity_query::indexer::{
  config::IndexerConfig,
  error::{ErrorPolicy, ErrorPolicyProvider},
  filter::Filter,
  run_indexer,
  types::{NetworkMagic, NodeAddress},
};
use oura::model::Event;

#[derive(Debug, Parser)]
struct IndexStartArgs {
  socket_path: String,
  #[arg(value_parser = clap::value_parser!(NetworkMagic))]
  network_magic: NetworkMagic,
  since_slot: Option<u64>,
  since_slot_hash: Option<String>,
  curr_symbols: Vec<String>,
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
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let args = Args::parse();

  match args.command {
    Command::Index(index_cmd) => match index_cmd {
      IndexCommand::Start(IndexStartArgs {
        socket_path,
        network_magic,
        since_slot,
        since_slot_hash,
        curr_symbols,
      }) => run_indexer(IndexerConfig {
        node_address: NodeAddress::UnixSocket(socket_path),
        network_magic,
        since_slot: since_slot.zip(since_slot_hash),
        safe_block_depth: 4,
        // TODO(chase): This is a dummy symbol - change to something meaningful.
        event_filter: Filter { curr_symbols },
        callback_fn: Arc::new(move |ev: Event| Box::pin(dummy_callback(ev))),
        retry_policy: Default::default(),
      }),
    },
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
