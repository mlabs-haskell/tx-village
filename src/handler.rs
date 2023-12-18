use oura::model as oura;

use sqlx::PgConnection;
use strum_macros::Display;
use tracing::{event, span, Instrument, Level};

use infinity_query::{
  database::{errors::TransactionDbError, TransactionSql},
  indexer::error::{ErrorPolicy, ErrorPolicyProvider},
};

pub struct Error(TransactionDbError);

// TODO (chase): Use this eventually.
#[allow(dead_code)]
pub async fn on_chain_event(conn: &mut PgConnection, ev: oura::Event) -> Result<(), Error> {
  let span = span!(Level::INFO, "HandlingEvent", event=?ev.context);
  async move {
    match ev.data {
      oura::EventData::Transaction(t) => {
        // TODO(chase): These unwraps shouldn't fail but maybe they should still be checked.
        let tx_id = &ev.context.tx_hash.unwrap();
        let span = span!(Level::DEBUG, "HandlingTransactionEvent", tx_id);
        async move {
          conn
            .save_tx(tx_id, &t.hash, ev.context.block_number.unwrap())
            .await
            .map_err(|err| {
              event!(Level::ERROR, label=%Event::TransactionError, ?err);
              Error(err)
            })
        }
        .instrument(span)
        .await
      }
      oura::EventData::RollBack {
        block_slot,
        block_hash: _,
      } => {
        // TODO(chase): Ignore "transaction not found" db errors.
        let span = span!(Level::DEBUG, "HandlingTransactionEvent", block_slot);
        conn
          .rollback_after_block(block_slot)
          .instrument(span)
          .await
          .map_err(|err| {
            event!(Level::ERROR, label=%Event::RollbackError, ?err);
            Error(err)
          })
      }
      _ => {
        event!(Level::DEBUG, label=%Event::Skipping);
        Ok(())
      }
    }
  }
  .instrument(span)
  .await
}

// TODO(chase): Implement proper error policy
impl ErrorPolicyProvider for Error {
  fn get_error_policy(&self) -> ErrorPolicy<Self> {
    ErrorPolicy::Skip
  }
}

#[derive(Display)]
enum Event {
  TransactionError,
  RollbackError,
  Skipping,
}
