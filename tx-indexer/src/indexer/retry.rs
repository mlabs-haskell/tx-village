use std::{fmt::Debug, future::Future, ops::Mul, time::Duration};

use strum_macros::Display;
use tracing::{event, span, Instrument, Level};

use super::error::{ErrorPolicy, ErrorPolicyProvider};

/// Influence retrying behavior.
/// i.e How many times and how often a failed operation should be retried.
/// Given we are dealing with `ErrorPolicy::Retry`
#[derive(Debug, Copy, Clone)]
pub struct RetryPolicy {
    pub max_retries: u32,
    pub backoff_unit: Duration,
    pub backoff_factor: u32,
    pub max_backoff: Duration,
}

impl Default for RetryPolicy {
    fn default() -> Self {
        Self {
            max_retries: 20,
            backoff_unit: Duration::from_millis(5_000),
            backoff_factor: 2,
            max_backoff: Duration::from_millis(20 * 5_000),
        }
    }
}

fn compute_backoff_delay(policy: &RetryPolicy, retry: u32) -> Duration {
    let units = policy.backoff_factor.pow(retry);
    let backoff = policy.backoff_unit.mul(units);
    core::cmp::min(backoff, policy.max_backoff)
}

/// Wrap an operation with retry logic.
/// Retrying is based on ErrorPolicy associated with particular error.
/// Retries are only performed for ErrorPolicy::Retry - other errors won't cause invocation of given operation again.
pub async fn perform_with_retry<
    E: Debug + ErrorPolicyProvider,
    R: Future<Output = Result<(), E>>,
>(
    op: impl Fn() -> R,
    policy: &RetryPolicy,
) -> Result<(), E> {
    let span = span!(Level::INFO, "perform_with_retry");
    let _enter = span.enter();

    // The retry logic is based on: https://github.com/txpipe/oura/blob/27fb7e876471b713841d96e292ede40101b151d7/src/utils/retry.rs
    let mut retry = 0;

    loop {
        let span = span!(Level::DEBUG, "TryingOperation", retry_count = retry);
        let res = async {
      let result = op().await;

      match result {
        Ok(_) => {
          event!(Level::INFO, label=%Event::Success);
          Some(Ok(()))
        }
        Err(err) => match err.get_error_policy() {
          ErrorPolicy::Exit => {
            event!(Level::INFO, label=%Event::FailureExit);
            Some(Err(err))
          }
          ErrorPolicy::Skip => {
            event!(Level::WARN, label=%Event::FailureSkip, err=?err);
            Some(Ok(()))
          }
          ErrorPolicy::Call(err_f) => span!(Level::WARN, "OperationFailureCall").in_scope(|| {
            err_f(err);
            Some(Ok(()))
          }),
          ErrorPolicy::Retry if retry < policy.max_retries => {
            event!(Level::WARN, label=%Event::FailureRetry, err=?err);

            retry += 1;

            let backoff = compute_backoff_delay(policy, retry);

            event!(Level::DEBUG, label=%Event::RetryBackoff, backoff_secs=backoff.as_secs());

            std::thread::sleep(backoff);

            None
          }
          _ => {
            event!(Level::DEBUG, label=%Event::RetriesExhausted);
            Some(Err(err))
          }
        },
      }
    }
    .instrument(span)
    .await;

        if let Some(res) = res {
            break res;
        }
    }
}

#[derive(Display)]
enum Event {
    Success,
    FailureExit,
    FailureSkip,
    FailureRetry,
    RetriesExhausted,
    RetryBackoff,
}
