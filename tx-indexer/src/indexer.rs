use crate::{
    config::{TxIndexerConfig, TxIndexerSource},
    error::TxIndexerError,
    handler::chain_event::EventHandler,
    sources::{cardano_node::source_from_cardano_node, fixture_files::source_from_files},
};

pub struct TxIndexer;

impl TxIndexer {
    pub async fn run<Handler: EventHandler>(
        conf: TxIndexerConfig<Handler>,
    ) -> Result<(), TxIndexerError<Handler::Error>> {
        match conf.source {
            TxIndexerSource::CardanoNode {
                node_address,
                network_magic,
                since_point,
                event_queue_size,
            } => {
                source_from_cardano_node(
                    conf.handler,
                    node_address,
                    network_magic,
                    since_point,
                    conf.retry_policy,
                    event_queue_size,
                    conf.cancellation_token,
                )
                .await
            }

            TxIndexerSource::FixtureFiles { dir_path } => {
                source_from_files(conf.handler, dir_path).await
            }
        }
    }
}
