mod database;
mod indexer;

use anyhow::Result;
use clap::Parser;

#[derive(Debug, Parser)]
struct DummyArgs {
  dummy_arg: u64,
}

#[derive(clap::Subcommand, Debug)]
enum Command {
  /// Run the dummy command
  Dummy(DummyArgs),
}
/// Infinity Query command line interface
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
  #[command(subcommand)]
  command: Command,
}

fn main() -> Result<(), anyhow::Error> {
  let args = Args::parse();

  match args.command {
    Command::Dummy(_) => Ok(println!("Dummy")),
  }
}
