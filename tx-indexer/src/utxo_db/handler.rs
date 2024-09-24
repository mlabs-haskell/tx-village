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
pub struct UtxoIndexerHandler {
    pg_pool: Pool<ConnectionManager<PgConnection>>,
}

impl UtxoIndexerHandler {
    pub fn new(pg_pool: Pool<ConnectionManager<PgConnection>>) -> Self {
        UtxoIndexerHandler { pg_pool }
    }
}

impl EventHandler for UtxoIndexerHandler {
    type Error = UtxoIndexerError;

    async fn handle(&self, event: ChainEvent) -> Result<(), Self::Error> {
        let span = span!(Level::DEBUG, "HandlingEvent", event=?event);
        async move {
            let mut conn = self.pg_pool.get().unwrap();

            match event {
                ChainEvent::TransactionEvent { transaction, time } => {
                    let tx_slot = time.slot;
                    let span = span!(Level::DEBUG, "HandlingTransactionEvent", ?transaction.hash);
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
                    let rollback_result = UtxosTable::rollback_after_block(txn, block_slot)?;

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
        .instrument(span)
        .await
    }
}
