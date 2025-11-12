use crate::types::cardano::Point;

#[derive(Debug, Clone)]
pub struct SyncStatus {
    since_slot: u64,
    pub last_synced_block: Point,
    pub tip: Option<Point>,
}

impl SyncStatus {
    pub fn new(since_point: Point) -> Self {
        SyncStatus {
            since_slot: since_point.block_slot,
            last_synced_block: since_point,
            tip: None,
        }
    }

    pub fn update(&mut self, last_synced: Point, tip: Point) {
        self.tip = Some(tip);
        self.last_synced_block = last_synced;
    }

    pub fn get_percentage(&self) -> Option<f32> {
        self.tip.as_ref().map(|synced_point| {
            let synced = self.last_synced_block.block_slot;
            let to_be_synced = synced_point.block_slot - self.since_slot;

            synced as f32 * 100.0 / to_be_synced as f32
        })
    }
}
