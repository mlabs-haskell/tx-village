use oura::model::{Event, EventData};

use sqlx::PgConnection;

use infinity_query::{
  database::{errors::TransactionDbError, TransactionSql},
  indexer::error::{ErrorPolicy, ErrorPolicyProvider},
};

pub struct Error(TransactionDbError);

pub async fn on_chain_event(conn: &mut PgConnection, ev: Event) -> Result<(), Error> {
  match ev.data {
    // TODO(chase): Ignore "transaction not found" db errors.
    EventData::Transaction(t) => conn
      .save_tx(
        // TODO(chase): These unwraps shouldn't fail but maybe they should still be checked.
        &ev.context.tx_hash.unwrap()[..],
        &t.hash,
        ev.context.block_number.unwrap(),
      )
      .await
      .map_err(Error),
    EventData::RollBack {
      block_slot,
      block_hash: _,
    } => conn.rollback_after_block(block_slot).await.map_err(Error),
    // Uninteresting events
    _ => Ok(()),
  }
}

// TODO(chase): Implement proper error policy
impl ErrorPolicyProvider for Error {
  fn get_error_policy(&self) -> ErrorPolicy<Self> {
    ErrorPolicy::Skip
  }
}
