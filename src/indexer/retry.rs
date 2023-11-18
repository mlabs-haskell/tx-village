use std::{fmt::Debug, ops::Mul, time::Duration};

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
pub fn perform_with_retry<E: Debug + ErrorPolicyProvider>(
  op: impl Fn() -> Result<(), E>,
  policy: &RetryPolicy,
) -> Result<(), E> {
  // The retry logic is based on: https://github.com/txpipe/oura/blob/27fb7e876471b713841d96e292ede40101b151d7/src/utils/retry.rs
  let mut retry = 0;

  loop {
    let result = op();

    match result {
      Ok(_) => {
        break Ok(());
      }
      Err(err) => match err.get_error_policy() {
        ErrorPolicy::Exit => break Err(err),
        ErrorPolicy::Skip => {
          log::warn!("(skip) callback failed with: {:?}", err);
          break Ok(());
        }
        ErrorPolicy::Call(err_f) => {
          err_f(err);
          break Ok(());
        }
        ErrorPolicy::Retry if retry < policy.max_retries => {
          log::warn!("(retry) callback failed with: {:?}", err);

          retry += 1;

          let backoff = compute_backoff_delay(policy, retry);

          log::debug!(
            "backoff for {}s until next retry #{}",
            backoff.as_secs(),
            retry
          );

          std::thread::sleep(backoff);
        }
        _ => break Err(err),
      },
    }
  }
}
