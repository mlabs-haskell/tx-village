use oura::model::Event;

use sqlx::PgConnection;

use infinity_query::indexer::error::{ErrorPolicy, ErrorPolicyProvider};

// TODO(chase): Implement custom error type
pub struct Error {}

// TODO(chase): Implement event handler
pub fn on_chain_event(conn: PgConnection, ev: Event) -> Result<(), Error> {
  todo!("Implement event handler")
}

impl ErrorPolicyProvider for Error {
  fn get_error_policy(&self) -> ErrorPolicy<Self> {
    ErrorPolicy::Skip
  }
}
