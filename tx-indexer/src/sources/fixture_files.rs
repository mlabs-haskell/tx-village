use std::path::PathBuf;

use anyhow::anyhow;
use futures::stream::{self, StreamExt, TryStreamExt};
use tokio::fs;

use crate::{error::TxIndexerError, handler::chain_event::EventHandler};

pub async fn source_from_files<H>(
    handler: H,
    dir_path: PathBuf,
) -> Result<(), TxIndexerError<H::Error>>
where
    H: EventHandler,
{
    let mut files = std::fs::read_dir(dir_path)
        .map_err(TxIndexerError::FixtureFileReadingError)?
        .collect::<Result<Vec<_>, _>>()
        .map_err(TxIndexerError::FixtureFileReadingError)?;

    files.sort_by_key(|entry| entry.file_name());

    let file_stream = stream::iter(files);

    let handler = &handler;
    let _: Vec<()> = file_stream
        .filter_map(|dir_entry| async move {
            let path = dir_entry.path();

            if let Some(ext) = path.extension() {
                if ext == "json" {
                    return Some(path);
                }
            };
            None
        })
        .then(|path| async move {
            let bytes = fs::read(path).await.map_err(|err| anyhow!(err))?;

            let chain_event = serde_json::from_slice(&bytes)?;

            handler
                .handle(&chain_event)
                .await
                .map_err(|err| anyhow!(err.to_string()))
        })
        .try_collect()
        .await
        .unwrap();

    Ok(())
}
