use super::{
    error::ErrorPolicyProvider,
    retry::{perform_with_retry, RetryPolicy},
};
use async_trait::async_trait;
use oura::{
    model::Event,
    pipelining::{BootstrapResult, SinkProvider, StageReceiver},
    utils::Utils,
};
use sqlx::{PgConnection, PgPool};
use std::{future::Future, marker::PhantomData, sync::Arc};
use strum_macros::Display;
use tokio::runtime::Runtime;
use tracing::{event, span, Instrument, Level};

pub trait Handler {
    type Error: std::error::Error + ErrorPolicyProvider;

    fn handle<'a>(
        event: Event,
        pg_connection: &'a mut PgConnection,
    ) -> impl Future<Output = Result<(), Self::Error>>;
}

/// This is a custom made sink for Oura. Based on a callback function.
/// The idea is similar to a webhook, but instead of calling a web endpoint - we call a function directly.
pub(crate) struct Callback<H: Handler> {
    // https://stackoverflow.com/questions/77589520/lifetime-of-struct-with-field-of-type-boxed-async-callback-must-outlive-static
    pub(crate) retry_policy: RetryPolicy,
    pub(crate) utils: Arc<Utils>,
    pub(crate) pg_pool: PgPool,
    pub(crate) handler: PhantomData<H>,
}

impl<H: Handler> Callback<H> {
    pub fn new(retry_policy: RetryPolicy, utils: Arc<Utils>, pg_pool: PgPool) -> Self {
        Self {
            retry_policy,
            utils,
            pg_pool,
            handler: PhantomData::default(),
        }
    }
}

impl<H: Handler> SinkProvider for Callback<H> {
    fn bootstrap(&self, input: StageReceiver) -> BootstrapResult {
        let span = span!(Level::INFO, "Callback::bootstrap");
        let _enter = span.enter();

        let retry_policy = self.retry_policy;
        let utils = self.utils.clone();
        let pool = self.pg_pool.clone();

        let handle = span!(Level::DEBUG, "SpawningThread").in_scope(|| {
            std::thread::spawn(move || {
                let span = span!(Level::DEBUG, "EventHandlingThread");
                let _enter = span.enter();

                // Running async function sycnhronously within another thread.
                let rt = Runtime::new().unwrap();
                rt.block_on(handle_event::<H>(input, &retry_policy, utils, pool))
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
async fn handle_event<'a, H: Handler>(
    input: StageReceiver,
    retry_policy: &RetryPolicy,
    utils: Arc<Utils>,
    mut pg_pool: PgPool,
) -> Result<(), H::Error> {
    let span = span!(Level::INFO, "handle_event");
    let _enter = span.enter();
    let pg_pool = &mut pg_pool;
    for chain_event in input.into_iter() {
        let span = span!(
          Level::INFO,
          "HandlingEvent",
          context=?chain_event.context
        );
        // Have to clone twice here to please the borrow checker...
        perform_with_retry::<H>(chain_event.clone(), retry_policy, pg_pool)
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
