use std::{path::PathBuf, sync::Arc};

use pallas_network::{
    facades::NodeClient,
    miniprotocols::{
        self,
        chainsync::{BlockContent, NextResponse, Tip},
    },
};
use pallas_primitives::{alonzo, babbage, conway, KeepRaw};
use pallas_traverse::MultiEraBlock;
use thiserror::Error;
use tokio::{
    select,
    sync::mpsc::{self, Receiver, Sender},
    try_join,
};
use tokio::{sync::Mutex, time};
use tokio_util::sync::CancellationToken;
use tracing::{debug, error, info_span, Instrument};

use crate::{
    error::TxIndexerError,
    from_pallas::FromPallas,
    handler::{
        chain_event::{ChainEvent, EventHandler},
        retry::{self, RetryPolicy},
    },
    progress_tracker::SyncStatus,
    types::{
        cardano::{BlockHash, Point},
        plutus::MultiEraTransaction,
        v1, v2, v3,
    },
};

#[derive(Debug, Error)]
pub enum MPSCError {
    #[error(transparent)]
    NodeEventSendError(
        #[from]
        tokio::sync::mpsc::error::SendError<
            pallas_network::miniprotocols::chainsync::NextResponse<
                pallas_network::miniprotocols::chainsync::BlockContent,
            >,
        >,
    ),

    #[error(transparent)]
    ChainEventSendError(#[from] tokio::sync::mpsc::error::SendError<ChainEvent>),
}

pub async fn source_from_cardano_node<Handler>(
    event_handler: Handler,
    node_address: PathBuf,
    network: u64,
    since_point: Option<Point>,
    retry_policy: RetryPolicy,
    event_queue_size: usize,
    cancellation_token: CancellationToken,
) -> Result<(), TxIndexerError<Handler::Error>>
where
    Handler: EventHandler,
{
    let (node_tx, node_rx) = mpsc::channel::<NextResponse<BlockContent>>(event_queue_size);
    let (event_tx, event_rx) = mpsc::channel::<ChainEvent>(event_queue_size);
    let event_tx2 = event_tx.clone();

    let sync_status = Arc::new(Mutex::new(SyncStatus::new(
        since_point.clone().unwrap_or_default(),
    )));

    let cancelt2 = cancellation_token.clone();
    let cancelt3 = cancellation_token.clone();

    let join_handle1 = tokio::spawn(async move {
        select! {
            _ = cancellation_token.cancelled() => Ok(()),
            result = read_loop::<Handler::Error>(
            node_tx,
            node_address,
            network,
            since_point,
            ) => result
        }
    });
    let join_handle2 = tokio::spawn(async move {
        select! {
            _ = cancelt2.cancelled() => Ok(()),
            result = process::<Handler::Error>(node_rx, event_tx) => result
        }
    });

    let sync_status2 = sync_status.clone();
    let join_handle3 = tokio::spawn(handle::<Handler>(
        event_rx,
        event_handler,
        retry_policy,
        sync_status2,
    ));
    let join_handle4 = tokio::spawn(async move {
        select! {
            _ = cancelt3.cancelled() => Ok(()),
            result = heartbeat::<Handler::Error>(event_tx2, sync_status) => result
        }
    });

    let (r1, r2, r3, r4) = try_join!(join_handle1, join_handle2, join_handle3, join_handle4)?;

    r1?;
    r2?;
    r3?;
    r4?;

    Ok(())
}

async fn read_loop<HandlerError>(
    node_tx: Sender<NextResponse<BlockContent>>,
    node_address: PathBuf,
    network: u64,
    since_point: Option<Point>,
) -> Result<(), TxIndexerError<HandlerError>> {
    let mut client = NodeClient::connect(node_address, network).await.unwrap();

    if let Some(point) = since_point {
        let point = miniprotocols::Point::from(point.clone());
        client.chainsync().find_intersect(vec![point]).await?;
    }

    loop {
        let res = client.chainsync().request_or_await_next().await.unwrap();

        node_tx
            .send(res)
            .await
            .map_err(MPSCError::NodeEventSendError)?;
    }
}

async fn process<HandlerError>(
    mut node_rx: Receiver<NextResponse<BlockContent>>,
    event_tx: Sender<ChainEvent>,
) -> Result<(), TxIndexerError<HandlerError>>
where
    HandlerError: std::fmt::Display,
{
    loop {
        match node_rx.recv().await {
            None => break,

            Some(res) => match res {
                NextResponse::RollForward(BlockContent(bytes), tip) => {
                    let block = pallas_traverse::MultiEraBlock::decode(&bytes[..]).unwrap();
                    let block_slot = block.header().slot();

                    let span = info_span!("RollForward", block_slot);
                    async {
                        let event = parse_roll_forward(block, tip).await?;

                        event_tx
                            .send(event)
                            .await
                            .map_err(MPSCError::ChainEventSendError)?;

                        Ok::<(), TxIndexerError<HandlerError>>(())
                    }
                    .instrument(span)
                    .await?;
                }
                NextResponse::RollBackward(point, tip) => {
                    let Point {
                        block_slot,
                        block_hash,
                    } = point.into();
                    let span = info_span!("Rollback", block_slot, ?block_hash);
                    async {
                        let event = ChainEvent::Rollback {
                            block_slot,
                            block_hash,
                            tip: tip.0.into(),
                        };

                        event_tx
                            .send(event)
                            .await
                            .map_err(MPSCError::ChainEventSendError)?;

                        Ok::<(), TxIndexerError<HandlerError>>(())
                    }
                    .instrument(span)
                    .await?;
                }

                NextResponse::Await => {}
            },
        }
    }
    Ok(())
}

async fn parse_roll_forward<'a, HandlerError>(
    block: MultiEraBlock<'a>,
    tip: Tip,
) -> Result<ChainEvent, TxIndexerError<HandlerError>> {
    let block_hash = BlockHash(block.hash().to_vec());
    match block {
        MultiEraBlock::Byron(_) => {
            todo!()
        }
        MultiEraBlock::AlonzoCompatible(minted_block, _) => {
            let alonzo::MintedBlock {
                header,
                transaction_witness_sets,
                transaction_bodies,
                ..
            } = *minted_block;

            debug!(?header.header_body);

            let witnesses = transaction_witness_sets
                .to_vec()
                .into_iter()
                .map(|witness| alonzo::WitnessSet::from(KeepRaw::unwrap(witness)));

            let txs = transaction_bodies
                .to_vec()
                .into_iter()
                .zip(witnesses)
                .map(|(tx, witness)| {
                    let tx = KeepRaw::unwrap(tx);
                    Ok::<_, TxIndexerError<HandlerError>>(MultiEraTransaction::from(
                        v1::Transaction::from_pallas((tx, witness))?,
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?;
            debug!(?txs);

            Ok(ChainEvent::RollForward {
                block_slot: header.header_body.slot,
                block_hash,
                transactions: txs,
                tip: tip.0.into(),
            })
        }
        MultiEraBlock::Babbage(minted_block) => {
            let babbage::MintedBlock {
                header,
                transaction_witness_sets,
                transaction_bodies,
                ..
            } = *minted_block;

            debug!(?header.header_body);

            let witnesses = transaction_witness_sets
                .to_vec()
                .into_iter()
                .map(|witness| babbage::WitnessSet::from(KeepRaw::unwrap(witness)));

            let txs = transaction_bodies
                .to_vec()
                .into_iter()
                .zip(witnesses)
                .map(|(tx, witness)| {
                    let tx = babbage::TransactionBody::from(KeepRaw::unwrap(tx));
                    Ok::<_, TxIndexerError<HandlerError>>(MultiEraTransaction::from(
                        v2::Transaction::from_pallas((tx, witness))?,
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?;

            debug!(?txs);

            Ok(ChainEvent::RollForward {
                block_slot: header.header_body.slot,
                block_hash,
                transactions: txs,
                tip: tip.0.into(),
            })
        }
        MultiEraBlock::Conway(minted_block) => {
            let conway::MintedBlock {
                header,
                transaction_witness_sets,
                transaction_bodies,
                ..
            } = *minted_block;
            debug!(?header);

            let witnesses = transaction_witness_sets
                .to_vec()
                .into_iter()
                .map(|witness| conway::WitnessSet::from(KeepRaw::unwrap(witness)));

            let txs = transaction_bodies
                .to_vec()
                .into_iter()
                .zip(witnesses)
                .map(|(tx, witness)| {
                    let tx = conway::TransactionBody::from(KeepRaw::unwrap(tx));
                    Ok::<_, TxIndexerError<HandlerError>>(MultiEraTransaction::from(
                        v3::Transaction::from_pallas((tx, witness))?,
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?;

            debug!(?txs);

            Ok(ChainEvent::RollForward {
                block_slot: header.header_body.slot,
                block_hash,
                transactions: txs,
                tip: tip.0.into(),
            })
        }
        MultiEraBlock::EpochBoundary(_minted_eb_block) => todo!(),
        _ => todo!(),
    }
}

async fn heartbeat<HandlerError>(
    event_tx: Sender<ChainEvent>,
    sync_status: Arc<Mutex<SyncStatus>>,
) -> Result<(), TxIndexerError<HandlerError>> {
    loop {
        time::sleep(time::Duration::from_secs(10)).await;

        let locked = sync_status.lock().await;
        let percentage = locked.get_percentage();

        event_tx
            .send(ChainEvent::Heartbeat {
                last_synced_block: locked.last_synced_block.clone(),
                tip: locked.tip.clone(),
                percentage,
            })
            .await
            .map_err(MPSCError::ChainEventSendError)?;
    }
}

async fn handle<Handler>(
    mut event_rx: Receiver<ChainEvent>,
    event_handler: Handler,
    retry_policy: RetryPolicy,
    sync_status: Arc<Mutex<SyncStatus>>,
) -> Result<(), TxIndexerError<Handler::Error>>
where
    Handler: EventHandler,
    Handler::Error: Send + 'static,
{
    loop {
        match event_rx.recv().await {
            None => {
                debug!("Event Channel closed");
                break Ok(());
            }

            Some(event) => {
                // let span = warn_span!("handle event", ?event);
                retry::perform_with_retry(&event_handler, &event, &retry_policy)
                    // .instrument(span)
                    .await
                    .map_err(TxIndexerError::EventHandlerError)?;

                if let ChainEvent::RollForward {
                    block_slot,
                    block_hash,
                    tip,
                    ..
                } = event
                {
                    let point = Point {
                        block_hash,
                        block_slot,
                    };
                    let mut locked = sync_status.lock().await;
                    locked.update(point, tip);
                };
            }
        }
    }
}
