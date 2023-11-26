use sqlx::{
  types::chrono::{DateTime, Utc},
  FromRow, PgConnection, PgExecutor, Postgres,
};

use super::errors::TransactionDbError;

#[derive(Debug, FromRow, PartialEq, Eq)]
pub struct TransactionDbModel {
  pub transaction_id: String,
  pub transaction_hex: String,
  /// The block a transaction appeared on.
  pub block: Option<u64>,
  pub deleted_on: Option<DateTime<Utc>>,
}

// We create a trait per table
// To make methods work on Pool, PgConnection and Transaction,
// We create an intermediary trait ConnectionLike that is only implemented by those types
pub trait ConnectionLike<'c>: PgExecutor<'c> {}
impl<'c> ConnectionLike<'c> for &'c mut PgConnection {}
impl<'c> ConnectionLike<'c> for &sqlx::Pool<Postgres> {}

#[async_trait::async_trait]
pub trait TransactionSql {
  async fn save_tx(
    self,
    tx_id: &str,
    transaction_hex: &str,
    transaction_block: u64,
  ) -> Result<(), TransactionDbError>;
  async fn rollback_after_block(self, transaction_block: u64) -> Result<(), TransactionDbError>;
}

#[async_trait::async_trait]
impl<'c> TransactionSql for &'c mut PgConnection {
  async fn save_tx(
    self,
    tx_id: &str,
    transaction_hex: &str,
    transaction_block: u64,
  ) -> Result<(), TransactionDbError> {
    sqlx::query(
      r#"
          INSERT INTO transaction (tx_id, transaction_hex, transaction_block)
          VALUES ($1, $2, $3)
        "#,
    )
    .bind(tx_id)
    .bind(transaction_hex)
    // PostgreSQL doesn't have u64 support...
    .bind(transaction_block as i64)
    .execute(self)
    .await
    .map_err(TransactionDbError::SomeSqlxError)?;

    Ok(())
  }
  async fn rollback_after_block(self, transaction_block: u64) -> Result<(), TransactionDbError> {
    sqlx::query(
      r#"
          UPDATE transaction
          SET deleted_on = CURRENT_TIMESTAMP
          WHERE transaction_block > $1
        "#,
    )
    .bind(transaction_block as i64)
    .execute(self)
    .await
    .map_err(TransactionDbError::SomeSqlxError)?;

    Ok(())
  }
}
