use data_encoding::HEXLOWER;
use sqlx::{FromRow, PgConnection};
use tracing::{info_span, span, Instrument, Level};

#[derive(Clone, Debug, FromRow, Eq, PartialEq)]
pub struct SyncProgressTable {
    pub block_slot: i64,
    pub block_hash: Vec<u8>,
}

impl SyncProgressTable {
    pub fn new(block_slot: u64, block_hash: String) -> Result<SyncProgressTable, anyhow::Error> {
        Ok(SyncProgressTable {
            block_slot: block_slot as i64,
            block_hash: HEXLOWER.decode(block_hash.as_bytes())?,
        })
    }

    /// Obtain the sync status of the DB
    pub async fn get(conn: &mut PgConnection) -> Result<Option<Self>, sqlx::Error> {
        let span = info_span!("Get SyncProgress");
        // Get existing entity
        sqlx::query_as::<_, Self>("SELECT block_slot, block_hash FROM sync_progress")
            .fetch_optional(conn)
            .instrument(span)
            .await
    }

    /// Save a new entity to the database.
    pub async fn store(&self, conn: &mut PgConnection) -> Result<(), sqlx::Error> {
        let span = span!(Level::INFO, "Store SyncProgress", ?self.block_slot);
        // Insert new entity
        sqlx::query(
            r#"
                INSERT INTO sync_progress (block_slot, block_hash)
                VALUES ($1, $2)
                ON CONFLICT (id)
                DO UPDATE SET
                    block_slot = EXCLUDED.block_slot,
                    block_hash = EXCLUDED.block_hash
                "#,
        )
        .bind(self.block_slot)
        .bind(self.block_hash.clone())
        .execute(conn)
        .instrument(span)
        .await?;

        Ok(())
    }

    pub async fn get_or(
        conn: &mut PgConnection,
        since_slot: Option<u64>,
        since_block: Option<String>,
    ) -> Result<Option<(u64, String)>, anyhow::Error> {
        let sync_status = Self::get(conn).await?;

        Ok(sync_status
            .map(
                |Self {
                     block_slot,
                     block_hash,
                 }| (block_slot as u64, HEXLOWER.encode(&block_hash)),
            )
            .or(since_slot.zip(since_block)))
    }
}
