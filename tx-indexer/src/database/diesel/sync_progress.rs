use diesel::{prelude::*, Connection};
use tracing::{debug, info_span, span, Level};

use crate::{
    error::TxIndexerError,
    types::cardano::{BlockHash, Point},
};

#[derive(
    Clone, Debug, Eq, PartialEq, diesel::Queryable, diesel::Selectable, diesel::Insertable,
)]
#[diesel(table_name = crate::schema::sync_progress)]
pub struct SyncProgressTable {
    pub block_slot: i64,
    pub block_hash: Vec<u8>,
    pub processed: bool,
}

impl SyncProgressTable {
    pub fn new(point: Point) -> SyncProgressTable {
        let Point {
            block_slot,
            block_hash: BlockHash(block_hash),
        } = point;
        SyncProgressTable {
            block_slot: block_slot as i64,
            block_hash,
            processed: false,
        }
    }

    /// Obtain the sync status of the DB
    pub fn get(conn: &mut diesel::PgConnection) -> Result<Option<Self>, diesel::result::Error> {
        use crate::schema::sync_progress::dsl::*;

        let span = info_span!("Get SyncProgress");
        let _entered = span.enter();
        // Get existing entity

        sync_progress
            .filter(processed.eq(true))
            .select(SyncProgressTable::as_select())
            .first(conn)
            .optional()
    }

    /// Save a new entity to the database.
    pub fn store(&self, conn: &mut diesel::PgConnection) -> Result<(), diesel::result::Error> {
        use crate::schema::sync_progress::{self, dsl::*};

        let span = span!(Level::INFO, "Store SyncProgress", ?self.block_slot);
        let _ = span.enter();

        // Find already stored unprocessed block to avoid progressing when the same block event is
        // emitted
        let already_stored = sync_progress
            .filter(processed.eq(false))
            .filter(block_slot.eq(self.block_slot))
            .select(SyncProgressTable::as_select())
            .first(conn)
            .optional()?;

        if already_stored.is_none() {
            conn.transaction::<_, diesel::result::Error, _>(|conn| {
                // Set finished block as processed
                diesel::delete(sync_progress.filter(processed.eq(true))).execute(conn)?;
                diesel::update(sync_progress.filter(processed.eq(false)))
                    .set(processed.eq(true))
                    .execute(conn)?;

                diesel::insert_into(sync_progress::table)
                    .values(self)
                    .execute(conn)?;

                Ok(())
            })?;
            debug!("Stored Sync Progress")
        } else {
            debug!("Duplicate Sync Progress event")
        }

        Ok(())
    }

    pub fn get_or<HandlerError>(
        conn: &mut diesel::PgConnection,
        since_slot: Option<u64>,
        since_block: Option<BlockHash>,
    ) -> Result<Option<Point>, TxIndexerError<HandlerError>> {
        let sync_status = Self::get(conn).map_err(TxIndexerError::SyncProgressStoreFailure)?;

        Ok(sync_status
            .map(
                |Self {
                     block_slot,
                     block_hash,
                     ..
                 }| Point {
                    block_slot: block_slot as u64,
                    block_hash: BlockHash(block_hash),
                },
            )
            .or_else(|| {
                since_slot
                    .zip(since_block)
                    .map(|(block_slot, block_hash)| Point {
                        block_hash,
                        block_slot,
                    })
            }))
    }
}
