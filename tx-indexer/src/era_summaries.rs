use chrono::{DateTime, Duration, Utc};
use tx_bakery::chain_query::{EraParameters, EraSummary, EraTime};

pub fn system_start() -> DateTime<Utc> {
    DateTime::parse_from_rfc3339("2022-10-25T00:00:00Z")
        .unwrap()
        .to_utc()
}

pub fn era_summaries() -> [EraSummary; 7] {
    [
        EraSummary {
            start: EraTime {
                time: Duration::seconds(0),
                slot: 0,
                epoch: 0,
            },
            end: Some(EraTime {
                time: Duration::seconds(0),
                slot: 0,
                epoch: 0,
            }),
            parameters: EraParameters {
                epoch_length: 4320,
                slot_length: 20000,
                safe_zone: Some(864),
            },
        },
        EraSummary {
            start: EraTime {
                time: Duration::seconds(0),
                slot: 0,
                epoch: 0,
            },
            end: Some(EraTime {
                time: Duration::seconds(0),
                slot: 0,
                epoch: 0,
            }),
            parameters: EraParameters {
                epoch_length: 86400,
                slot_length: 1000,
                safe_zone: Some(25920),
            },
        },
        EraSummary {
            start: EraTime {
                time: Duration::seconds(0),
                slot: 0,
                epoch: 0,
            },
            end: Some(EraTime {
                time: Duration::seconds(0),
                slot: 0,
                epoch: 0,
            }),
            parameters: EraParameters {
                epoch_length: 86400,
                slot_length: 1000,
                safe_zone: Some(25920),
            },
        },
        EraSummary {
            start: EraTime {
                time: Duration::seconds(0),
                slot: 0,
                epoch: 0,
            },
            end: Some(EraTime {
                time: Duration::seconds(0),
                slot: 0,
                epoch: 0,
            }),
            parameters: EraParameters {
                epoch_length: 86400,
                slot_length: 1000,
                safe_zone: Some(25920),
            },
        },
        EraSummary {
            start: EraTime {
                time: Duration::seconds(0),
                slot: 0,
                epoch: 0,
            },
            end: Some(EraTime {
                time: Duration::seconds(259200),
                slot: 259200,
                epoch: 3,
            }),
            parameters: EraParameters {
                epoch_length: 86400,
                slot_length: 1000,
                safe_zone: Some(25920),
            },
        },
        EraSummary {
            start: EraTime {
                time: Duration::seconds(259200),
                slot: 259200,
                epoch: 3,
            },
            end: Some(EraTime {
                time: Duration::seconds(55814400),
                slot: 55814400,
                epoch: 646,
            }),
            parameters: EraParameters {
                epoch_length: 86400,
                slot_length: 1000,
                safe_zone: Some(25920),
            },
        },
        EraSummary {
            start: EraTime {
                time: Duration::seconds(55814400),
                slot: 55814400,
                epoch: 646,
            },
            end: Some(EraTime {
                time: Duration::seconds(93571200),
                slot: 93571200,
                epoch: 1083,
            }),
            parameters: EraParameters {
                epoch_length: 86400,
                slot_length: 1000,
                safe_zone: Some(25920),
            },
        },
    ]
}
