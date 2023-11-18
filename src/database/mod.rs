use serde::{Deserialize, Serialize};
use sqlx::{FromRow, PgConnection, PgExecutor, Postgres};
use thiserror::Error;

// We create a trait per table
// To make methods work on Pool, PgConnection and Transaction,
// We create an intermediary trait ConnectionLike that is only implemented by those types
pub trait ConnectionLike<'c>: PgExecutor<'c> {}
impl<'c> ConnectionLike<'c> for &'c mut PgConnection {}
impl<'c> ConnectionLike<'c> for &sqlx::Pool<Postgres> {}

#[derive(Error, Debug)]
pub enum TransactionDbError {
  #[error("Transaction with id {0} not found ")]
  TxNotFound(String),
  #[error("Sqlx error: {0}")]
  SomeSqlxError(sqlx::Error),
}

#[derive(Serialize, Deserialize, Debug, FromRow, PartialEq, Eq)]
pub struct TransactionDbModel {
  pub transaction_id: String,
  pub status: TransactionStatus,
  /// The block a transaction appeared on.
  pub block: Option<u64>,
}

#[derive(Serialize, Deserialize, Debug, sqlx::Type, PartialEq, Eq, Clone)]
#[sqlx(type_name = "transaction_status", rename_all = "snake_case")]
pub enum TransactionStatus {
  AwaitingSignature,
  AwaitingSubmission,
  AwaitingConfirmation,
  Submitted,
  Success,
  Cancelled,
}

#[async_trait::async_trait]
pub trait TransactionSql {
  async fn set_tx_awaiting_confirmation(
    self,
    tx_id: &str,
    transaction_hex: &str,
  ) -> Result<(), TransactionDbError>;
  async fn set_tx_submitted(
    self,
    tx_id: &str,
    transaction_block: u64,
  ) -> Result<(), TransactionDbError>;
}

#[async_trait::async_trait]
impl<'c> TransactionSql for &'c mut PgConnection {
  async fn set_tx_submitted(
    self,
    tx_id: &str,
    transaction_block: u64,
  ) -> Result<(), TransactionDbError> {
    let res = sqlx::query(
      r#"
        UPDATE transaction
        SET transaction_block = $2, status = 'submitted'
        WHERE id = $1
      "#,
    )
    .bind(tx_id)
    .bind(transaction_block as i64)
    .execute(self)
    .await
    .map_err(TransactionDbError::SomeSqlxError)?;

    if res.rows_affected() != 1 {
      Err(TransactionDbError::TxNotFound(tx_id.to_string()))
    } else {
      Ok(())
    }
  }
  async fn set_tx_awaiting_confirmation(
    self,
    tx_id: &str,
    transaction_hex: &str,
  ) -> Result<(), TransactionDbError> {
    let res = sqlx::query(
      r#"
        UPDATE transaction
        SET transaction_hex = $2, status = 'awaiting_confirmation'
        WHERE id = $1
      "#,
    )
    .bind(tx_id)
    .bind(transaction_hex)
    .execute(self)
    .await
    .map_err(TransactionDbError::SomeSqlxError)?;

    if res.rows_affected() != 1 {
      Err(TransactionDbError::TxNotFound(tx_id.to_string()))
    } else {
      Ok(())
    }
  }
}
