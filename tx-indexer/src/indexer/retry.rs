use super::{
    callback::Handler,
    error::{ErrorPolicy, ErrorPolicyProvider},
    types::{ChainEvent, ChainEventTime, TransactionEventRecord},
};
use ::oura::model::{MintRecord, OutputAssetRecord};
use cardano_serialization_lib as csl;
use data_encoding::HEXLOWER;
use num_bigint::BigInt;
use num_traits::FromPrimitive;
use oura::model as oura;
use plutus_ledger_api::v2::{
    address::Address,
    crypto::LedgerBytes,
    datum::{Datum, DatumHash, OutputDatum},
    script::{MintingPolicyHash, ScriptHash},
    transaction::{TransactionHash, TransactionInput, TransactionOutput},
    value::{CurrencySymbol, TokenName, Value},
};
use sqlx::{Acquire, PgPool};
use std::{fmt::Debug, ops::Mul, time::Duration};
use strum_macros::Display;
use tracing::{event, span, Instrument, Level};
use tx_bakery::utils::csl_to_pla::TryToPLA;

/// Influence retrying behavior.
/// i.e How many times and how often a failed operation should be retried.
/// Given we are dealing with `ErrorPolicy::Retry`
#[derive(Debug, Copy, Clone)]
pub struct RetryPolicy {
    pub max_retries: u32,
    pub backoff_unit: Duration,
    pub backoff_factor: u32,
    pub max_backoff: Duration,
}

impl Default for RetryPolicy {
    fn default() -> Self {
        Self {
            max_retries: 20,
            backoff_unit: Duration::from_millis(5_000),
            backoff_factor: 2,
            max_backoff: Duration::from_millis(20 * 5_000),
        }
    }
}

fn compute_backoff_delay(policy: &RetryPolicy, retry: u32) -> Duration {
    let units = policy.backoff_factor.pow(retry);
    let backoff = policy.backoff_unit.mul(units);
    core::cmp::min(backoff, policy.max_backoff)
}

/// Wrap an operation with retry logic.
/// Retrying is based on ErrorPolicy associated with particular error.
/// Retries are only performed for ErrorPolicy::Retry - other errors won't cause invocation of given operation again.
pub async fn perform_with_retry<H: Handler>(
    handler: &H,
    oura_event: oura::Event,
    policy: &RetryPolicy,
    pg_pool: &mut PgPool,
) -> Result<(), H::Error> {
    let span = span!(Level::DEBUG, "perform_with_retry");
    let _enter = span.enter();

    // TODO(chase): Should we handle Oura to PLA parse failure?
    let (event_time, event) = parse_oura_event(oura_event).unwrap();

    // The retry logic is based on: https://github.com/txpipe/oura/blob/27fb7e876471b713841d96e292ede40101b151d7/src/utils/retry.rs
    let mut retry = 0;

    loop {
        // TODO(szg251): Handle errors properly
        let mut conn = pg_pool.acquire().await.unwrap();
        let actual_conn = conn.acquire().await.unwrap();
        let span = span!(Level::DEBUG, "TryingOperation", retry_count = retry);
        let res = async {
          let result = handler.handle(event_time.clone(), event.clone(), actual_conn)
            .instrument(span!(Level::DEBUG, "UserDefinedHandler")).await;

          match result {
            Ok(_) => {
              event!(Level::DEBUG, label=%Event::Success);
              Some(Ok(()))
            }
            Err(err) => match err.get_error_policy() {
              ErrorPolicy::Exit => {
                event!(Level::ERROR, label=%Event::FailureExit);
                Some(Err(err))
              }
              ErrorPolicy::Skip => {
                event!(Level::WARN, label=%Event::FailureSkip, err=?err);
                Some(Ok(()))
              }
              ErrorPolicy::Call(err_f) => span!(Level::WARN, "OperationFailureCall").in_scope(|| {
                err_f(err);
                Some(Ok(()))
              }),
              ErrorPolicy::Retry if retry < policy.max_retries => {
                event!(Level::WARN, label=%Event::FailureRetry, err=?err);

                retry += 1;

                let backoff = compute_backoff_delay(policy, retry);

                event!(Level::DEBUG, label=%Event::RetryBackoff, backoff_secs=backoff.as_secs());

                std::thread::sleep(backoff);

                None
              }
              _ => {
                event!(Level::DEBUG, label=%Event::RetriesExhausted);
                Some(Err(err))
              }
            },
          }
        }
        .instrument(span)
        .await;

        if let Some(res) = res {
            break res;
        }
    }
}

#[derive(Display)]
enum Event {
    Success,
    FailureExit,
    FailureSkip,
    FailureRetry,
    RetriesExhausted,
    RetryBackoff,
}

fn parse_oura_event(ev: oura::Event) -> Result<(ChainEventTime, ChainEvent), OuraParseError> {
    let time = ChainEventTime {
        // These unwraps should not fail.
        block_hash: ev.context.block_hash.unwrap(),
        block_number: ev.context.block_number.unwrap(),
        slot: ev.context.slot.unwrap(),
    };
    let event = match ev.data {
        oura::EventData::Transaction(dat) => {
            ChainEvent::TransactionEvent(parse_oura_transaction(dat)?)
        }
        _ => panic!("absurd: Indexer filter should only allow transaction event variant."),
    };
    Ok((time, event))
}

fn parse_oura_transaction(
    tx: oura::TransactionRecord,
) -> Result<TransactionEventRecord, OuraParseError> {
    Ok(TransactionEventRecord {
        hash: TransactionHash::from_oura(tx.hash)?,
        fee: tx.fee,
        size: tx.size,
        // All these unwraps should succeed since we enable `include_transaction_details` in the mapper config.
        inputs: tx
            .inputs
            .unwrap()
            .into_iter()
            .map(|oura::TxInputRecord { tx_id, index }| {
                Ok(TransactionInput {
                    transaction_id: TransactionHash::from_oura(tx_id)?,
                    index: BigInt::from_oura(index)?,
                })
            })
            .collect::<Result<_, _>>()?,
        outputs: tx
            .outputs
            .unwrap()
            .into_iter()
            .map(
                |oura::TxOutputRecord {
                     address,
                     amount,
                     assets,
                     datum_hash,
                     inline_datum,
                 }| {
                    Ok(TransactionOutput {
                        address: Address::from_oura(address)?,
                        datum: match (datum_hash, inline_datum) {
                            (None, None) => OutputDatum::None,
                            (_, Some(datm)) => {
                                OutputDatum::InlineDatum(Datum::from_oura(datm.plutus_data)?)
                            }
                            (Some(dh), _) => OutputDatum::DatumHash(DatumHash::from_oura(dh)?),
                        },
                        // NOTE(chase): There is currently no way to know about reference scripts with Oura.
                        reference_script: None,
                        value: Value::ada_value(&BigInt::from_oura(amount)?)
                            + Value::from_oura(assets.unwrap_or_default())?,
                    })
                },
            )
            .collect::<Result<_, _>>()?,
        mint: Value::from_oura(tx.mint.unwrap())?,
        plutus_data: tx
            .plutus_data
            .unwrap_or_default()
            .into_iter()
            .map(
                |oura::PlutusDatumRecord {
                     plutus_data,
                     datum_hash,
                 }| {
                    Ok((
                        DatumHash::from_oura(datum_hash)?,
                        Datum::from_oura(plutus_data)?,
                    ))
                },
            )
            .collect::<Result<_, _>>()?,
    })
}

/// Convert an Oura transaction record type to its plutus-ledger-api counterpart
trait FromOura<T> {
    fn from_oura(value: T) -> Result<Self, OuraParseError>
    where
        Self: Sized;
}

impl FromOura<u64> for BigInt {
    fn from_oura(value: u64) -> Result<Self, OuraParseError> {
        BigInt::from_u64(value).ok_or(OuraParseError::BigIntFromU64(value))
    }
}

impl FromOura<i64> for BigInt {
    fn from_oura(value: i64) -> Result<Self, OuraParseError> {
        BigInt::from_i64(value).ok_or(OuraParseError::BigIntFromI64(value))
    }
}

impl FromOura<String> for LedgerBytes {
    fn from_oura(value: String) -> Result<Self, OuraParseError> {
        Ok(LedgerBytes(
            HEXLOWER
                .decode(&value.clone().into_bytes()[..])
                .map_err(|_| OuraParseError::HashFromString(value))?,
        ))
    }
}

impl FromOura<String> for TransactionHash {
    fn from_oura(value: String) -> Result<Self, OuraParseError> {
        Ok(TransactionHash(LedgerBytes::from_oura(value)?))
    }
}

impl FromOura<String> for DatumHash {
    fn from_oura(value: String) -> Result<Self, OuraParseError> {
        Ok(DatumHash(LedgerBytes::from_oura(value)?))
    }
}

impl FromOura<String> for CurrencySymbol {
    fn from_oura(value: String) -> Result<Self, OuraParseError> {
        Ok(if value.is_empty() {
            CurrencySymbol::Ada
        } else {
            CurrencySymbol::NativeToken(MintingPolicyHash(ScriptHash(LedgerBytes::from_oura(
                value,
            )?)))
        })
    }
}

impl FromOura<serde_json::Value> for Datum {
    fn from_oura(value: serde_json::Value) -> Result<Self, OuraParseError> {
        serde_json::from_value(value.clone())
            .ok()
            .and_then(|y: csl::plutus::PlutusData| y.try_to_pla().ok())
            .map(Datum)
            .ok_or(OuraParseError::DataFromJSON(value))
    }
}

impl FromOura<String> for Address {
    fn from_oura(value: String) -> Result<Self, OuraParseError> {
        csl::address::Address::from_bech32(&value)
            .ok()
            .and_then(|x| x.try_to_pla().ok())
            .ok_or(OuraParseError::AddressFromString(value))
    }
}

impl FromOura<Vec<OutputAssetRecord>> for Value {
    fn from_oura(value: Vec<OutputAssetRecord>) -> Result<Self, OuraParseError> {
        value.iter().try_fold(Value::new(), |acc, x| {
            let amt = BigInt::from_oura(x.amount)?;
            Ok(acc.insert_token(
                &CurrencySymbol::from_oura(x.policy.clone())?,
                &TokenName::from_string(&x.asset),
                &amt,
            ))
        })
    }
}

impl FromOura<Vec<MintRecord>> for Value {
    fn from_oura(value: Vec<MintRecord>) -> Result<Self, OuraParseError> {
        value.iter().try_fold(Value::new(), |acc, x| {
            let amt = BigInt::from_oura(x.quantity)?;
            Ok(acc.insert_token(
                &CurrencySymbol::from_oura(x.policy.clone())?,
                &TokenName::from_string(&x.asset),
                &amt,
            ))
        })
    }
}

#[derive(thiserror::Error, Debug)]
enum OuraParseError {
    #[error("Unable to parse bigint from u64: {0}")]
    BigIntFromU64(u64),
    #[error("Unable to parse bigint from i64: {0}")]
    BigIntFromI64(i64),
    #[error("Unable to parse hash from string: {0}")]
    HashFromString(String),
    #[error("Unable to parse Address from bech32 string: {0}")]
    AddressFromString(String),
    #[error("Unable to parse Datum from JSON: {0}")]
    DataFromJSON(serde_json::Value),
}
