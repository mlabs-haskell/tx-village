use crate::{
    error::{ErrorPolicy, ErrorPolicyProvider},
    handler::{callback::EventHandler, chain_event::ChainEvent},
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DummyHandlerError {}

impl ErrorPolicyProvider for DummyHandlerError {
    fn get_error_policy(&self) -> ErrorPolicy<Self> {
        ErrorPolicy::Skip
    }
}

// TODO(chase): Enhance dummy callback
#[derive(Clone)]
pub struct DummyHandler;

impl EventHandler for DummyHandler {
    type Error = DummyHandlerError;

    async fn handle(&self, _event: ChainEvent) -> Result<(), Self::Error> {
        Ok(())
    }
}
