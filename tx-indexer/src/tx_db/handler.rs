use super::{error::TxIndexerError, table::utxos::UtxosTable};
use sqlx::{Acquire, Connection, PgPool};
use tracing::{event, span, Instrument, Level};
use tx_indexer::handler::{callback::EventHandler, chain_event::ChainEvent};

#[derive(Clone)]
pub struct TxIndexerHandler {
    pg_pool: PgPool,
}

impl TxIndexerHandler {
    pub fn new(pg_pool: PgPool) -> Self {
        TxIndexerHandler { pg_pool }
    }
}

impl EventHandler for TxIndexerHandler {
    type Error = TxIndexerError;

    async fn handle(&self, event: ChainEvent) -> Result<(), Self::Error> {
        let span = span!(Level::INFO, "HandlingEvent", event=?event);
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
                ChainEvent::SyncProgressEvent { .. } => Ok(()),
            }
        }
        .instrument(span)
        .await
    }
}
