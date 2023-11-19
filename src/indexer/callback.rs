use std::{fmt::Debug, future::Future, sync::Arc};

use oura::{
  model::Event,
  pipelining::{BootstrapResult, SinkProvider, StageReceiver},
  utils::Utils,
};
use tokio::runtime::Runtime;

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
    let callback_fn = self.f.clone();
    let retry_policy = self.retry_policy;
    let utils = self.utils.clone();
    let handle = std::thread::spawn(move || {
      // Running async function sycnhronously within another thread.
      let rt = Runtime::new().unwrap();
      rt.block_on(handle_event(input, callback_fn, &retry_policy, utils))
        .expect("request loop failed")
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
  for event in input.iter() {
    perform_with_retry(|| callback_fn(&event), retry_policy)
      .await
      // Notify progress to the pipeline.
      .map(|_| utils.track_sink_progress(&event))?;
    // ^ This will exit the loop if an error is returned.
    // After all, `perform_with_retry` will only return error if all other options,
    // based on `ErrorPolicy`, were exhausted.
  }

  // All chain events in this sequence have been handled.
  Ok(())
}
