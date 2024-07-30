use std::sync::{atomic::AtomicUsize, Arc};

use anyhow::anyhow;
use chrono::{DateTime, Duration, Utc};
use oura::utils::ChainWellKnownInfo;
use tx_bakery::chain_query::{EraParameters, EraSummary, EraTime};

use crate::from_oura::OuraParseError;

/// A progress tracker holds information about the chain info required to calculate
/// POSIX time from slots
#[derive(Clone, Debug)]
pub struct ProgressTracker {
    pub system_start: DateTime<Utc>,
    pub era_summaries: Vec<EraSummary>,
    pub since_slot: u64,
    pub sync_progress: Arc<AtomicUsize>,
}

impl ProgressTracker {
    pub fn new(since_slot: u64, chain_info: &ChainWellKnownInfo) -> Result<Self, anyhow::Error> {
        let system_start = DateTime::from_timestamp(chain_info.byron_known_time as i64, 0).ok_or(
            anyhow!("Unable to convert shelley_known_time to to DateTime"),
        )?;

        Ok(ProgressTracker {
            system_start,
            era_summaries: chain_info_to_era_summaries(&system_start, chain_info)?,
            since_slot,
            sync_progress: Arc::new(AtomicUsize::new(0)),
        })
    }

    pub fn get_percentage(&self, slot: u64) -> Result<f32, OuraParseError> {
        let current_time = Utc::now();
        let current_slot =
            tx_bakery::time::time_into_slot(&self.era_summaries, &self.system_start, current_time)
                .map_err(OuraParseError::TimeConversionError)?;

        let synced = slot - self.since_slot;
        let to_be_synced = current_slot - self.since_slot;

        Ok(synced as f32 * 100.0 / to_be_synced as f32)
    }
}

/// Convert Oura chain info into Ogmios EraSummaries.
/// Oura does not include all eras, only Byron and Shelley, all other eras are part of
/// Shelley. This is good enough for time calculations.
fn chain_info_to_era_summaries(
    system_start_time: &DateTime<Utc>,
    chain_info: &ChainWellKnownInfo,
) -> Result<Vec<EraSummary>, anyhow::Error> {
    let byron_start = EraTime {
        time: Duration::zero(),
        slot: 0,
        epoch: 0,
    };

    let shelley_start = EraTime {
        time: DateTime::from_timestamp(chain_info.shelley_known_time as i64, 0).ok_or(anyhow!(
            "Unable to convert shelley_known_time to to DateTime"
        ))? - system_start_time,
        slot: chain_info.shelley_known_slot,
        epoch: chain_info.shelley_known_slot / chain_info.byron_epoch_length as u64,
    };

    Ok(vec![
        EraSummary {
            start: byron_start,
            end: Some(shelley_start.clone()),
            parameters: EraParameters {
                epoch_length: chain_info.byron_epoch_length as u64,
                slot_length: chain_info.byron_slot_length as u64 * 1000,
                safe_zone: Some(4320),
            },
        },
        EraSummary {
            start: shelley_start,
            end: None,
            parameters: EraParameters {
                epoch_length: chain_info.shelley_epoch_length as u64,
                slot_length: chain_info.shelley_slot_length as u64 * 1000,
                safe_zone: Some(4320),
            },
        },
    ])
}
