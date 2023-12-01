use std::{fmt::Debug, future::Future, sync::Arc};

use oura::{
  model::Event,
  pipelining::{BootstrapResult, SinkProvider, StageReceiver},
  utils::Utils,
};
use strum_macros::Display;
use tokio::runtime::Runtime;
use tracing::{event, span, Instrument, Level};

use super::{
  error::ErrorPolicyProvider,
  retry::{perform_with_retry, RetryPolicy},
};

/// This is a custom made sink for Oura. Based on a callback function.
/// The idea is similar to a webhook, but instead of calling a web endpoint - we call a function directly.
pub(crate) struct Callback<Fut> {
  pub(crate) f: fn(&Event) -> Fut,
  pub(crate) retry_policy: RetryPolicy,
  pub(crate) utils: Arc<Utils>,
}

impl<E: Debug + ErrorPolicyProvider + 'static, Fut: Future<Output = Result<(), E>> + 'static>
  SinkProvider for Callback<Fut>
{
  fn bootstrap(&self, input: StageReceiver) -> BootstrapResult {
    let span = span!(Level::INFO, "Callback::bootstrap");
    let _enter = span.enter();

    let callback_fn = self.f.clone();
    let retry_policy = self.retry_policy;
    let utils = self.utils.clone();

    let handle = span!(Level::DEBUG, "SpawningThread").in_scope(|| {
      std::thread::spawn(move || {
        let span = span!(Level::DEBUG, "EventHandlingThread");
        let _enter = span.enter();

        // Running async function sycnhronously within another thread.
        let rt = Runtime::new().unwrap();
        rt.block_on(handle_event(input, callback_fn, &retry_policy, utils))
          .or_else(|err| {
            event!(Level::ERROR, label=%Events::EventHandlerFailure, ?err);
            Err(err)
          })
          .expect("request loop failed");
      })
    });

    Ok(handle)
  }
}

// Handle a sequence of events transmitted at once.
async fn handle_event<E: Debug + ErrorPolicyProvider, Fut: Future<Output = Result<(), E>>>(
  input: StageReceiver,
  callback_fn: impl Fn(&Event) -> Fut,
  retry_policy: &RetryPolicy,
  utils: Arc<Utils>,
) -> Result<(), E> {
  let span = span!(Level::INFO, "handle_event");
  let _enter = span.enter();

  for chain_event in input.iter() {
    let span = span!(
      Level::INFO,
      "HandlingBlock",
      block_no = &chain_event.context.block_number.unwrap(),
      block_hash = &chain_event.context.block_hash.clone().unwrap(),
      slot_no = &chain_event.context.slot.unwrap(),
    );
    perform_with_retry(|| callback_fn(&chain_event), retry_policy)
      .instrument(span)
      .await
      // Notify progress to the pipeline.
      .map(|_| utils.track_sink_progress(&chain_event))?;
    // ^ This will exit the loop if an error is returned.
    // After all, `perform_with_retry` will only return error if all other options,
    // based on `ErrorPolicy`, were exhausted.
  }

  // All chain events in this sequence have been handled.
  Ok(())
}

#[derive(Display)]
pub enum Events {
  EventHandlerFailure,
}
