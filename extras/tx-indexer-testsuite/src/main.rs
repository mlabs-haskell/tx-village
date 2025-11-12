use anyhow::Context;
use clap::Parser;
use diesel::{
    pg::PgConnection,
    r2d2::{ConnectionManager, Pool},
};
use plutus_ledger_api::v3::{
    datum::OutputDatum,
    transaction::TransactionInput,
    value::{CurrencySymbol, Value},
};
use prettytable::{format, row, Table};
use std::{fmt::Debug, path::PathBuf};
use tokio_util::sync::CancellationToken;
use tracing::Level;
use tx_indexer::{
    config::TxIndexerConfig,
    database::diesel::sync_progress::SyncProgressTable,
    types::cardano::{BlockHash, Point},
    TxIndexer,
};
use tx_indexer_testsuite::utxo_db::{
    error::UtxoIndexerError, handler::UtxoIndexerHandler, table::utxos::UtxosTable,
};

#[derive(clap::Subcommand, Debug)]
enum Command {
    /// Run the Indexer
    Index(IndexArgs),

    /// Store blocks as files for testing
    CreateFixtures(CreateFixtureArgs),

    /// Query indexed data
    #[command(subcommand)]
    Query(QueryCommand),
}

#[derive(Debug, Parser)]
struct IndexArgs {
    /// Cardano node socket path
    #[arg(long)]
    socket_path: PathBuf,

    /// Network magic number
    #[arg(short('m'), long("magic"))]
    /// Network identified by magic number and chain info file
    network_magic: u64,

    /// Sync from this slot
    #[arg(short, long, requires = "since_block_hash")]
    since_slot: Option<u64>,

    /// Sync from this block hash
    #[arg(short('a'), long, requires = "since_slot")]
    since_block_hash: Option<String>,

    /// PostgreSQL database URL
    #[arg(long)]
    postgres_url: String,
}

#[derive(Debug, Parser)]
struct CreateFixtureArgs {
    /// Cardano node socket path
    #[arg(long)]
    socket_path: PathBuf,

    /// Network magic number
    #[arg(short('m'), long("magic"))]
    /// Network identified by magic number and chain info file
    network_magic: u64,

    /// Sync from this slot
    #[arg(short, long, requires = "since_block_hash")]
    since_slot: Option<u64>,

    /// Sync from this block hash
    #[arg(short('a'), long, requires = "since_slot")]
    since_block_hash: Option<String>,

    /// Filepath of the fixture files
    #[arg(long, default_value = "./tests/fixtures")]
    dump_path: PathBuf,

    #[arg(long, default_value = "1000")]
    slots: u64,
}

#[derive(clap::Subcommand, Debug)]
enum QueryCommand {
    /// Get UTxO set by address
    UtxosAt(UtxosAtArgs),
}

#[derive(Debug, Parser)]
struct UtxosAtArgs {
    /// Filter UTxOs by address
    #[arg(long)]
    address: String,

    /// PostgreSQL database URL
    #[arg(long)]
    postgres_url: String,
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
async fn main() -> anyhow::Result<()> {
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
        Command::Index(IndexArgs {
            socket_path,
            network_magic,
            since_slot,
            since_block_hash,
            postgres_url,
        }) => {
            let manager = ConnectionManager::<PgConnection>::new(postgres_url);

            let pg_pool = Pool::builder()
                .test_on_check_out(true)
                .build(manager)
                .expect("Could not build connection pool");

            let mut conn = pg_pool.get().unwrap();

            let since_block_hash = since_block_hash
                .map(|hex_str| {
                    Ok::<BlockHash, anyhow::Error>(BlockHash(
                        hex::decode(hex_str).context("cannot decode block hash hex")?,
                    ))
                })
                .transpose()?;

            let handler = UtxoIndexerHandler::postgres(pg_pool);

            let sync_progress = SyncProgressTable::get_or::<UtxoIndexerError>(
                &mut conn,
                since_slot,
                since_block_hash,
            )
            .context("couldn't get current sync progress from db")?;

            TxIndexer::run(TxIndexerConfig::cardano_node(
                handler,
                socket_path,
                network_magic,
                sync_progress,
            ))
            .await?;

            Ok(())
        }
        Command::CreateFixtures(CreateFixtureArgs {
            socket_path,
            network_magic,
            since_slot,
            since_block_hash,
            dump_path,
            slots,
        }) => {
            let since_block_hash = since_block_hash
                .map(|hex_str| {
                    Ok::<BlockHash, anyhow::Error>(BlockHash(
                        hex::decode(hex_str).context("cannot decode block hash hex")?,
                    ))
                })
                .transpose()?;

            let max_slot = since_slot.unwrap_or(0) + slots;

            let cancellation_token = CancellationToken::new();
            let handler =
                UtxoIndexerHandler::fixture(dump_path, max_slot, cancellation_token.clone());

            let sync_progress = since_slot
                .zip(since_block_hash)
                .map(|(slot, hash)| Point::new(hash, slot));

            let config =
                TxIndexerConfig::cardano_node(handler, socket_path, network_magic, sync_progress)
                    .with_cancellation_token(cancellation_token);

            TxIndexer::run(config).await?;

            Ok(())
        }
        Command::Query(query_subcommand) => match query_subcommand {
            QueryCommand::UtxosAt(UtxosAtArgs {
                postgres_url,
                address,
            }) => {
                let manager = ConnectionManager::<PgConnection>::new(postgres_url);

                let pg_pool = Pool::builder()
                    .test_on_check_out(true)
                    .build(manager)
                    .expect("Could not build connection pool");

                let mut conn = pg_pool.get().unwrap();

                let utxos = UtxosTable::list_by_address(&address, &mut conn)?;

                let mut table = Table::new();
                table.set_titles(row!["UTxO", "Datum", "Value"]);

                utxos.into_iter().for_each(|utxo| {
                    let tx_in = TransactionInput::from(utxo.utxo_ref);
                    let value_str = Value::from(utxo.value)
                        .0
                        .iter()
                        .flat_map(|(cur_sym, assets)| {
                            assets.iter().map(move |(tn, amount)| match cur_sym {
                                CurrencySymbol::Ada => amount.to_string(),
                                CurrencySymbol::NativeToken(symbol) => {
                                    format!("{:?}.{:?} {}", symbol.0, tn.0, amount)
                                }
                            })
                        })
                        .collect::<Vec<_>>()
                        .join("+");

                    table.add_row(row![
                        &format!("{:?}#{:?}", tx_in.transaction_id.0, tx_in.index),
                        &format!("{:?}", OutputDatum::try_from(utxo.datum).unwrap()),
                        &value_str,
                    ]);
                });

                table.set_format(*format::consts::FORMAT_NO_LINESEP_WITH_TITLE);
                table.printstd();

                Ok(())
            }
        },
    }
}
