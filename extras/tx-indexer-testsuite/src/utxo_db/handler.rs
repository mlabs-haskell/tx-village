use std::path::PathBuf;

use anyhow::Context;
use async_trait::async_trait;
use diesel::{
    r2d2::{ConnectionManager, Pool},
    Connection, PgConnection,
};
use num_bigint::BigInt;
use tokio_util::sync::CancellationToken;
use tracing::{event, info, warn, Level};
use tx_indexer::{
    database::diesel::sync_progress::SyncProgressTable,
    handler::chain_event::{ChainEvent, EventHandler},
    types::{cardano::Point, plutus::MultiEraTransaction},
};

use super::{error::UtxoIndexerError, table::utxos::UtxosTable};

#[derive(Clone)]
pub enum UtxoIndexerHandler {
    Postgres {
        db_pool: Pool<ConnectionManager<PgConnection>>,
    },
    Fixture {
        fixture_path: PathBuf,
        max_slot: u64,
        cancellation_token: CancellationToken,
    },
}

impl UtxoIndexerHandler {
    pub fn postgres(db_pool: Pool<ConnectionManager<PgConnection>>) -> Self {
        UtxoIndexerHandler::Postgres { db_pool }
    }

    pub fn fixture(
        fixture_path: PathBuf,
        max_slot: u64,
        cancellation_token: CancellationToken,
    ) -> Self {
        UtxoIndexerHandler::Fixture {
            fixture_path,
            max_slot,
            cancellation_token,
        }
    }
}

#[async_trait]
impl EventHandler for UtxoIndexerHandler {
    type Error = UtxoIndexerError;

    async fn handle(&self, event: &ChainEvent) -> Result<(), Self::Error> {
        async move {
            match self {
                UtxoIndexerHandler::Fixture {
                    fixture_path,
                    max_slot,
                    cancellation_token,
                } => {
                    if let ChainEvent::RollForward { block_slot, .. } = event {
                        if block_slot > max_slot {
                            cancellation_token.cancel();
                            info!("Reached max slot, exiting...");
                            return Ok(());
                        }
                    }

                    let chain_event_json = serde_json::to_string(&event).unwrap();
                    let mut file_path = fixture_path.clone();
                    file_path.push(format!(
                        "{}.json",
                        chrono::Local::now()
                            .timestamp_nanos_opt()
                            .context("timestamp out of range")?
                    ));

                    std::fs::write(file_path, chain_event_json)
                        .context("couldn't write to fixture path")?;

                    Ok(())
                }
                UtxoIndexerHandler::Postgres { db_pool } => {
                    let mut conn = db_pool.get().unwrap();
                    match event {
                        ChainEvent::RollForward {
                            transactions,
                            block_slot,
                            block_hash,
                            ..
                        } => {
                            for (i, transaction) in transactions.iter().enumerate() {
                                let MultiEraTransaction {
                                    id,
                                    inputs,
                                    outputs,
                                    ..
                                } = transaction;
                                let utxo_ref = &plutus_ledger_api::v3::TransactionInput {
                                    transaction_id: id.clone(),
                                    index: BigInt::from(i),
                                };

                                for output in outputs {
                                    UtxosTable::new(utxo_ref.clone(), output.clone(), *block_slot)?
                                        .store(&mut conn)?;
                                }

                                for input in inputs {
                                    UtxosTable::delete_by_id(
                                        input.clone(),
                                        *block_slot,
                                        &mut conn,
                                    )?;
                                }
                            }

                            event!(Level::INFO, name = "UTxO Stored");

                            SyncProgressTable::new(Point::new(block_hash.clone(), *block_slot))
                                .store(&mut conn)?;

                            Ok::<(), Self::Error>(())
                        }
                        ChainEvent::Rollback { block_slot, .. } => conn.transaction(|txn| {
                            let rollback_result =
                                UtxosTable::rollback_after_block(txn, *block_slot)?;

                            warn!(
                            name = "RollbackHandled",
                            ?rollback_result.deleted,
                            ?rollback_result.recovered,
                            );

                            Ok::<(), Self::Error>(())
                        }),
                        ChainEvent::Heartbeat { .. } => Ok(()),
                    }
                }
            }
        }
        .await
    }
}
