use thiserror::Error;

#[derive(Error, Debug)]
pub enum TransactionDbError {
  #[error("Transaction with id {0} not found ")]
  TxNotFound(String),
  #[error("Sqlx error: {0}")]
  SomeSqlxError(sqlx::Error),
}
