use super::{error::UtxoIndexerError, table::utxos::UtxosTable};
use sqlx::{Acquire, Connection, PgPool};
use tracing::{event, span, Instrument, Level};
use tx_indexer::{
    database::sync_progress::SyncProgressTable,
    handler::{callback::EventHandler, chain_event::ChainEvent},
};

#[derive(Clone)]
pub struct UtxoIndexerHandler {
    pg_pool: PgPool,
}

impl UtxoIndexerHandler {
    pub fn new(pg_pool: PgPool) -> Self {
        UtxoIndexerHandler { pg_pool }
    }
}

impl EventHandler for UtxoIndexerHandler {
    type Error = UtxoIndexerError;

    async fn handle(&self, event: ChainEvent) -> Result<(), Self::Error> {
        let span = span!(Level::DEBUG, "HandlingEvent", event=?event);
        async move {
            let mut conn = self.pg_pool.acquire().await.unwrap();
            let conn = conn.acquire().await.unwrap();

            match event {
                ChainEvent::TransactionEvent { transaction, time } => {
                    // TODO(chase): These unwraps shouldn't fail but maybe they should still be checked.
                    let tx_block = time.block_number;
                    let span = span!(Level::DEBUG, "HandlingTransactionEvent", ?transaction.hash);
                    async move {
                        for utxo in transaction.outputs {
                            UtxosTable::new(utxo, tx_block)?.store(conn).await?;
                        }

                        event!(Level::INFO, name = "UTxO Stored");
                        Ok(())
                    }
                    .instrument(span)
                    .await
                }
                ChainEvent::RollbackEvent { block_slot, .. } => {
                    conn.transaction(|txn| {
                        Box::pin(async move {
                            let rollback_result =
                                UtxosTable::rollback_after_block(txn, block_slot).await?;

                            event!(
                                Level::WARN,
                                name = "RollbackHandled",
                                ?rollback_result.deleted,
                                ?rollback_result.recovered,
                            );

                            Ok::<(), Self::Error>(())
                        })
                    })
                    .await
                }
                ChainEvent::SyncProgressEvent {
                    block_slot,
                    block_hash,
                    ..
                } => {
                    SyncProgressTable::new(block_slot, block_hash)
                        .map_err(UtxoIndexerError::Internal)?
                        .store(conn)
                        .await
                        .map_err(UtxoIndexerError::Internal)?;

                    Ok(())
                }
            }
        }
        .instrument(span)
        .await
    }
}
