use oura::model::{Event, EventData};

use sqlx::PgConnection;

use infinity_query::{
  database::{TransactionDbError, TransactionSql},
  indexer::error::{ErrorPolicy, ErrorPolicyProvider},
};

pub struct Error(TransactionDbError);

pub async fn on_chain_event(conn: &mut PgConnection, ev: Event) -> Result<(), Error> {
  match ev.data {
    // TODO(chase): Ignore "transaction not found" db errors.
    EventData::Transaction(_) => conn
      .set_tx_submitted(
        &ev.context.tx_hash.unwrap()[..],
        ev.context.block_number.unwrap(),
      )
      .await
      .map_err(Error),
    EventData::RollBack {
      block_slot,
      block_hash: _,
    } => conn.rm_txs_after_block(block_slot).await.map_err(Error),
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
