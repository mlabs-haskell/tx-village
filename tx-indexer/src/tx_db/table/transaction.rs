use plutus_ledger_api::v2::transaction::TransactionHash;
use sqlx::{FromRow, PgConnection};
use strum_macros::Display;
use tracing::{event, span, Instrument, Level};
use tx_indexer::database::plutus::{SlotDB, TransactionHashDB};

use crate::tx_db::error::TxIndexerError;

#[derive(Debug, FromRow, PartialEq, Eq)]
pub struct TransactionTable {
    pub transaction_id: TransactionHashDB,
    /// The block a transaction appeared on.
    pub created_at: SlotDB,
    pub deleted_at: Option<SlotDB>,
}

#[derive(Debug)]
pub struct RollbackResult<T>
where
    T: std::fmt::Debug,
{
    pub recovered: Vec<T>,
    pub deleted: Vec<T>,
}

impl TransactionTable {
    pub fn new(transaction_id: TransactionHash, created_at: u64) -> Self {
        Self {
            transaction_id: transaction_id.into(),
            created_at: created_at.into(),
            deleted_at: None,
        }
    }

    pub async fn store(self, conn: &mut PgConnection) -> Result<(), TxIndexerError> {
        let tx_id = TransactionHash::from(self.transaction_id.clone());
        let span = span!(Level::INFO, "SavingTx", ?tx_id);
        async move {
            sqlx::query(
                r#"
                INSERT INTO transactions (transaction_id, created_at)
                VALUES ($1, $2)
                "#,
            )
            .bind(self.transaction_id)
            .bind(self.created_at)
            .execute(conn)
            .await
            .map_err(|err| {
                event!(Level::ERROR, label=%Event::SqlxError, ?err);
                TxIndexerError::DbError(err)
            })?;

            Ok(())
        }
        .instrument(span)
        .await
    }

    pub async fn rollback_after_block(
        conn: &mut PgConnection,
        transaction_block: u64,
    ) -> Result<RollbackResult<TransactionTable>, TxIndexerError> {
        let span = span!(Level::INFO, "Rollback", %transaction_block);
        async move {
            let deleted = sqlx::query_as::<_, Self>(
                r#"
                SELECT *
                FROM transactions
                WHERE created_at > $1
                "#,
            )
            .bind(SlotDB::from(transaction_block))
            .fetch_all(&mut *conn)
            .await
            .map_err(|err| {
                event!(Level::ERROR, label=%Event::SqlxError, ?err);
                TxIndexerError::DbError(err)
            })?;

            let recovered = sqlx::query_as::<_, Self>(
                r#"
                SELECT *
                FROM transactions
                WHERE deleted_at > $1
                "#,
            )
            .bind(SlotDB::from(transaction_block))
            .fetch_all(&mut *conn)
            .await
            .map_err(|err| {
                event!(Level::ERROR, label=%Event::SqlxError, ?err);
                TxIndexerError::DbError(err)
            })?;

            sqlx::query(
                r#"
                UPDATE transactions
                SET deleted_at = NULL
                WHERE deleted_at > $1;
                "#,
            )
            .bind(SlotDB::from(transaction_block))
            .execute(&mut *conn)
            .await
            .map_err(|err| {
                event!(Level::ERROR, label=%Event::SqlxError, ?err);
                TxIndexerError::DbError(err)
            })?;

            sqlx::query(
                r#"
                DELETE FROM transactions
                WHERE created_at > $1;
                "#,
            )
            .bind(SlotDB::from(transaction_block))
            .execute(&mut *conn)
            .await
            .map_err(|err| {
                event!(Level::ERROR, label=%Event::SqlxError, ?err);
                TxIndexerError::DbError(err)
            })?;

            Ok(RollbackResult { deleted, recovered })
        }
        .instrument(span)
        .await
    }
}

#[derive(Display)]
enum Event {
    SqlxError,
}
