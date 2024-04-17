use crate::chain_query::EraSummary;
use crate::error::{Error, Result};
use anyhow::anyhow;
use cardano_serialization_lib as csl;
use chrono::{DateTime, Utc};
use plutus_ledger_api::v2::interval::{Extended, LowerBound, PlutusInterval, UpperBound};
use plutus_ledger_api::v2::transaction::{POSIXTime, POSIXTimeConversionError, POSIXTimeRange};

/// Convert a POSIX time into number of slots in the current system
pub fn posix_time_into_slot(
    era_summaries: &Vec<EraSummary>,
    sys_start: &DateTime<Utc>,
    posix_time: POSIXTime,
) -> Result<u64> {
    let abs_time: DateTime<Utc> = posix_time
        .try_into()
        .map_err(|source: POSIXTimeConversionError| Error::ConversionError(anyhow!(source)))?;

    // Time since system start
    let rel_time = abs_time - sys_start;

    if era_summaries.is_empty() {
        return Err(Error::InvalidConfiguration("era_summaries".to_string()));
    }

    let era_summary = era_summaries
        .iter()
        .filter(|era_summary| era_summary.start.time <= rel_time)
        .last()
        .ok_or(Error::InvalidPOSIXTime(format!(
            "{} is before system start at {}, or era_summaries are invalid.",
            abs_time, sys_start
        )))?;

    let time_in_era = rel_time - era_summary.start.time;
    let slots_in_era = time_in_era.num_milliseconds() as f64 / era_summary.parameters.slot_length;

    Ok(era_summary.start.slot + slots_in_era as u64)
}

pub fn time_range_into_slots(
    era_summaries: &Vec<EraSummary>,
    sys_start: &DateTime<Utc>,
    interval: POSIXTimeRange,
) -> Result<(Option<csl::utils::BigNum>, Option<csl::utils::BigNum>)> {
    let PlutusInterval {
        from:
            LowerBound {
                bound: lower_bound,
                closed: lower_closed,
            },
        to: UpperBound {
            bound: upper_bound,
            closed: upper_closed,
        },
    } = interval;

    let lb_slots = match lower_bound {
        Extended::NegInf => None,
        Extended::PosInf => Some(u64::MAX),
        Extended::Finite(posix_time) => {
            let slots = posix_time_into_slot(&era_summaries, &sys_start, posix_time)?;
            if lower_closed {
                Some(slots)
            } else {
                Some(slots + 1)
            }
        }
    };

    let ub_slots = match upper_bound {
        Extended::NegInf => Some(0),
        Extended::PosInf => None,
        Extended::Finite(posix_time) => {
            let slots = posix_time_into_slot(&era_summaries, &sys_start, posix_time)?;
            if upper_closed {
                Some(slots)
            } else {
                Some(slots - 1)
            }
        }
    };

    Ok((
        lb_slots.map(csl::utils::to_bignum),
        ub_slots.map(csl::utils::to_bignum),
    ))
}
