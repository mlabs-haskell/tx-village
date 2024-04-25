use std::{fmt::Debug, future::Future, sync::Arc};

use oura::{
    model::Event,
    pipelining::{BootstrapResult, SinkProvider, StageReceiver},
    utils::Utils,
};
use sqlx::{pool::PoolConnection, PgPool, Postgres};
use strum_macros::Display;
use tokio::runtime::Runtime;
use tracing::{event, span, Instrument, Level};

use super::{
    error::ErrorPolicyProvider,
    retry::{perform_with_retry, RetryPolicy},
    types::{AsyncFunction, AsyncResult},
};

/// This is a custom made sink for Oura. Based on a callback function.
/// The idea is similar to a webhook, but instead of calling a web endpoint - we call a function directly.
pub(crate) struct Callback<E> {
    // https://stackoverflow.com/questions/77589520/lifetime-of-struct-with-field-of-type-boxed-async-callback-must-outlive-static
    pub(crate) handler: Arc<AsyncFunction<(Event, PoolConnection<Postgres>), AsyncResult<E>>>,
    pub(crate) retry_policy: RetryPolicy,
    pub(crate) utils: Arc<Utils>,
    pub(crate) pg_pool: PgPool,
}

impl<'a, E: Debug + ErrorPolicyProvider + 'static> SinkProvider for Callback<E> {
    fn bootstrap(&self, input: StageReceiver) -> BootstrapResult {
        let span = span!(Level::INFO, "Callback::bootstrap");
        let _enter = span.enter();

        let retry_policy = self.retry_policy;
        let utils = self.utils.clone();
        let pool = self.pg_pool.clone();

        let handler = Arc::clone(&self.handler);
        let handle = span!(Level::DEBUG, "SpawningThread").in_scope(|| {
            std::thread::spawn(move || {
                let span = span!(Level::DEBUG, "EventHandlingThread");
                let _enter = span.enter();

                // Running async function sycnhronously within another thread.
                let rt = Runtime::new().unwrap();
                rt.block_on(handle_event(
                    input,
                    |ev: Event, conn: PoolConnection<Postgres>| handler((ev, conn)),
                    &retry_policy,
                    utils,
                    pool,
                ))
                .map_err(|err| {
                    event!(Level::ERROR, label=%Events::EventHandlerFailure, ?err);
                    err
                })
                .expect("request loop failed");
            })
        });

        Ok(handle)
    }
}

// Handle a sequence of events transmitted at once.
async fn handle_event<
    'a,
    E: Debug + ErrorPolicyProvider + 'static,
    R: Future<Output = Result<(), E>>,
>(
    input: StageReceiver,
    callback_fn: impl Fn(Event, PoolConnection<Postgres>) -> R,
    retry_policy: &RetryPolicy,
    utils: Arc<Utils>,
    mut pg_pool: PgPool,
) -> Result<(), E> {
    let span = span!(Level::INFO, "handle_event");
    let _enter = span.enter();
    let pg_pool = &mut pg_pool;
    for chain_event in input.iter() {
        let span = span!(
          Level::INFO,
          "HandlingEvent",
          context=?chain_event.context
        );
        // Have to clone twice here to please the borrow checker...
        perform_with_retry(
            |conn| callback_fn(chain_event.clone(), conn),
            retry_policy,
            pg_pool,
        )
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
