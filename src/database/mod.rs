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
  async fn set_tx_status(
    self,
    tx_id: &str,
    tx_status: TransactionStatus,
  ) -> Result<(), TransactionDbError>;
}

#[async_trait::async_trait]
impl<'c> TransactionSql for &'c mut PgConnection {
  async fn set_tx_status(
    self,
    tx_id: &str,
    tx_status: TransactionStatus,
  ) -> Result<(), TransactionDbError> {
    let res = sqlx::query(
      r#"
        UPDATE transaction
        SET status = $2
        WHERE id = $1
      "#,
    )
    .bind(tx_id)
    .bind(tx_status)
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
