use oura::model::{Event, EventData};

use sqlx::PgConnection;
use strum_macros::Display;
use tracing::{event, span, Level};

use infinity_query::{
  database::{errors::TransactionDbError, TransactionSql},
  indexer::error::{ErrorPolicy, ErrorPolicyProvider},
};

pub struct Error(TransactionDbError);

pub async fn on_chain_event(conn: &mut PgConnection, ev: Event) -> Result<(), Error> {
  let span = span!(Level::INFO, "on_chain_event");
  let _enter = span.enter();
  match ev.data {
    // TODO(chase): These unwraps shouldn't fail but maybe they should still be checked.
    // TODO(chase): Ignore "transaction not found" db errors.
    EventData::Transaction(t) => {
      let tx_id = &ev.context.tx_hash.unwrap();
      event!(Level::DEBUG, task=%Events::HandlingTransactionEvent, tx_id);
      conn
        .save_tx(tx_id, &t.hash, ev.context.block_number.unwrap())
        .await
        .map_err(Error)
    }
    EventData::RollBack {
      block_slot,
      block_hash: _,
    } => {
      event!(Level::DEBUG, task=%Events::HandlingRollbackEvent, block_slot);
      conn.rollback_after_block(block_slot).await.map_err(Error)
    }
    // Uninteresting events
    _ => {
      event!(Level::DEBUG, task=%Events::SkippingEvent);
      Ok(())
    }
  }
}

// TODO(chase): Implement proper error policy
impl ErrorPolicyProvider for Error {
  fn get_error_policy(&self) -> ErrorPolicy<Self> {
    ErrorPolicy::Skip
  }
}

#[derive(Display)]
enum Events {
  HandlingTransactionEvent,
  HandlingRollbackEvent,
  SkippingEvent,
}
