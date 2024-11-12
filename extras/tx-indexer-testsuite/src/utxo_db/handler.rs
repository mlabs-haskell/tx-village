use std::path::PathBuf;

use super::{error::UtxoIndexerError, table::utxos::UtxosTable};
use diesel::{
    r2d2::{ConnectionManager, Pool},
    Connection, PgConnection,
};
use tracing::{event, span, warn, Instrument, Level};
use tx_indexer::{
    database::diesel::sync_progress::SyncProgressTable,
    handler::{callback::EventHandler, chain_event::ChainEvent},
};

#[derive(Clone)]
pub enum UtxoIndexerHandler {
    Postgres {
        db_pool: Pool<ConnectionManager<PgConnection>>,
    },
    Fixture {
        fixture_path: PathBuf,
    },
}

impl UtxoIndexerHandler {
    pub fn postgres(db_pool: Pool<ConnectionManager<PgConnection>>) -> Self {
        UtxoIndexerHandler::Postgres { db_pool }
    }

    pub fn fixture(fixture_path: PathBuf) -> Self {
        UtxoIndexerHandler::Fixture { fixture_path }
    }
}

impl EventHandler for UtxoIndexerHandler {
    type Error = UtxoIndexerError;

    async fn handle(&self, event: ChainEvent) -> Result<(), Self::Error> {
        let span = span!(Level::DEBUG, "HandlingEvent", event=?event);
        async move {
            match self {
                UtxoIndexerHandler::Fixture { fixture_path } => {
                    let chain_event_json = serde_json::to_string(&event).unwrap();
                    let mut file_path = fixture_path.clone();
                    file_path.push(format!("{}.json", chrono::Local::now().timestamp_millis()));

                    std::fs::write(file_path, chain_event_json).unwrap();

                    Ok(())
                }
                UtxoIndexerHandler::Postgres { db_pool } => {
                    let mut conn = db_pool.get().unwrap();

                    match event {
                        ChainEvent::TransactionEvent { transaction, time } => {
                            let tx_slot = time.slot;
                            let span =
                                span!(Level::DEBUG, "HandlingTransactionEvent", ?transaction.hash);
                            async move {
                                for new_utxo in transaction.outputs {
                                    UtxosTable::new(new_utxo, tx_slot)?.store(&mut conn)?;
                                }

                                for utxo_ref in transaction.inputs {
                                    UtxosTable::delete_by_id(utxo_ref, tx_slot, &mut conn)?;
                                }

                                event!(Level::INFO, name = "UTxO Stored");
                                Ok(())
                            }
                            .instrument(span)
                            .await
                        }
                        ChainEvent::RollbackEvent { block_slot, .. } => conn.transaction(|txn| {
                            let rollback_result =
                                UtxosTable::rollback_after_block(txn, block_slot)?;

                            warn!(
                            name = "RollbackHandled",
                            ?rollback_result.deleted,
                            ?rollback_result.recovered,
                            );

                            Ok::<(), Self::Error>(())
                        }),
                        ChainEvent::SyncProgressEvent {
                            block_slot,
                            block_hash,
                            ..
                        } => {
                            SyncProgressTable::new(block_slot, block_hash)
                                .map_err(UtxoIndexerError::Internal)?
                                .store(&mut conn)?;

                            Ok(())
                        }
                    }
                }
            }
        }
        .instrument(span)
        .await
    }
}
