use super::error::OgmiosError;
use crate::chain_query::{self, FullTransactionOutput};
use crate::utils::csl_to_pla::TryToPLA;
use crate::utils::pla_to_csl::TryToCSLWithDef;
use anyhow::anyhow;
use cardano_serialization_lib as csl;
use chrono::Duration;
use data_encoding::HEXLOWER;
use jsonrpsee::core::traits::ToRpcParams;
use num_bigint::BigInt;
use plutus_ledger_api as pla;
use serde::{Deserialize, Serialize};
use serde_json::value::{RawValue, Value as JsonValue};
use std::collections::BTreeMap;
use std::str::FromStr;

pub type Result<T> = std::result::Result<T, OgmiosError>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct OgmiosHealth {
    // These fields are not parsed:
    //
    // "metrics": {
    //     "totalUnrouted": 1,
    //     "totalMessages": 30029,
    //     "runtimeStats": {
    //         "gcCpuTime": 1233009354,
    //         "cpuTime": 81064672549,
    //         "maxHeapSize": 41630,
    //         "currentHeapSize": 1014
    //     },
    //     "totalConnections": 10,
    //     "sessionDurations": {
    //         "max": 57385,
    //         "mean": 7057,
    //         "min": 0
    //     },
    //     "activeConnections": 0
    // },
    // "startTime": "2021-03-15T16:16:41.470782977Z",
    // "lastTipUpdate": "2021-03-15T16:28:36.853115034Z",
    // "lastKnownTip": {
    //     "hash": "c29428f386c701c1d1ba1fd259d4be78921ee9ee6c174eac898245ceb55e8061",
    //     "blockNo": 5034297,
    //     "slot": 15520688
    // },
    #[serde(rename(deserialize = "networkSynchronization"))]
    pub network_synchronization: f32,
    #[serde(rename(deserialize = "currentEra"))]
    pub current_era: String,
    #[serde(rename(deserialize = "connectionStatus"))]
    pub connection_status: String,
    #[serde(rename(deserialize = "currentEpoch"))]
    pub current_epoch: i64,
    #[serde(rename(deserialize = "slotInEpoch"))]
    pub slot_in_epoch: i64,
    pub version: String,
    pub network: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct QueryLedgerStateUtxoByAddressParams {
    pub addresses: Vec<String>,
}

impl ToRpcParams for QueryLedgerStateUtxoByAddressParams {
    fn to_rpc_params(self) -> std::result::Result<Option<Box<RawValue>>, serde_json::Error> {
        serde_json::value::to_raw_value(&self).map(Some)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct OutputReference {
    id: TransactionId,
    index: u32,
}

impl TryFrom<pla::v2::transaction::TransactionInput> for OutputReference {
    type Error = OgmiosError;

    fn try_from(
        i: pla::v2::transaction::TransactionInput,
    ) -> std::result::Result<Self, Self::Error> {
        let id = TransactionId::from(i.transaction_id);
        let index = u32::try_from(i.index).map_err(|err| OgmiosError::ConversionError {
            label: "BigInt to u32".into(),
            source: anyhow!(err),
        })?;
        Ok(Self { id, index })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct QueryLedgerStateUtxoByOutputReferenceParams {
    #[serde(rename = "outputReferences")]
    pub output_references: Vec<OutputReference>,
}

impl ToRpcParams for QueryLedgerStateUtxoByOutputReferenceParams {
    fn to_rpc_params(self) -> std::result::Result<Option<Box<RawValue>>, serde_json::Error> {
        serde_json::value::to_raw_value(&self).map(Some)
    }
}

pub(crate) type QueryNetworkStartTimeResponse = String;

pub(crate) type QueryLedgerStateEraSummariesResponse = Vec<EraSummary>;

pub(crate) type QueryLedgerStateUtxoResponse = Vec<Utxo>;

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum QueryLedgerStateTipResponse {
    Origin(String),
    Point { slot: u64, id: String },
}

impl From<QueryLedgerStateTipResponse> for chain_query::ChainTip {
    fn from(tip: QueryLedgerStateTipResponse) -> chain_query::ChainTip {
        match tip {
            QueryLedgerStateTipResponse::Origin(_) => Self::Origin,
            QueryLedgerStateTipResponse::Point { slot, id } => Self::Point { slot, id },
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct EraSummary {
    pub start: EraTime,
    pub end: Option<EraTime>,
    pub parameters: EraParams,
}

impl TryFrom<EraSummary> for chain_query::EraSummary {
    type Error = OgmiosError;

    fn try_from(era_summary: EraSummary) -> std::result::Result<Self, Self::Error> {
        Ok(Self {
            start: era_summary.start.try_into()?,
            end: era_summary.end.map(|end| end.try_into()).transpose()?,
            parameters: era_summary.parameters.into(),
        })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct EraTime {
    pub time: Seconds,
    pub slot: u64,
    pub epoch: u64,
}

impl TryFrom<EraTime> for chain_query::EraTime {
    type Error = OgmiosError;

    fn try_from(era_time: EraTime) -> std::result::Result<chain_query::EraTime, Self::Error> {
        Ok(chain_query::EraTime {
            time: Duration::try_seconds(era_time.time.seconds as i64).ok_or(
                OgmiosError::ConversionError {
                    label: "EraSummary start".to_string(),
                    source: anyhow!("Unable to convert seconds."),
                },
            )?,
            slot: era_time.slot,
            epoch: era_time.epoch,
        })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct EraParams {
    #[serde(rename(deserialize = "epochLength"))]
    pub epoch_length: u64,
    #[serde(rename(deserialize = "slotLength"))]
    pub slot_length: MilliSeconds,
    #[serde(rename(deserialize = "safeZone"))]
    pub safe_zone: Option<u64>,
}

impl From<EraParams> for chain_query::EraParameters {
    fn from(era_summary: EraParams) -> chain_query::EraParameters {
        chain_query::EraParameters {
            epoch_length: era_summary.epoch_length,
            slot_length: era_summary.slot_length.milliseconds,
            safe_zone: era_summary.safe_zone,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct Seconds {
    pub seconds: f64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct MilliSeconds {
    pub milliseconds: f64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct Utxo {
    pub transaction: TransactionId,
    pub index: u32,
    pub address: String,
    pub value: Value,
    #[serde(rename(deserialize = "datumHash"))]
    pub datum_hash: Option<String>,
    pub datum: Option<String>,
    pub script: Option<Script>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub(crate) struct TransactionId {
    pub id: String,
}

impl ToRpcParams for TransactionId {
    fn to_rpc_params(self) -> std::result::Result<Option<Box<RawValue>>, serde_json::Error> {
        serde_json::value::to_raw_value(&self).map(Some)
    }
}

type Value = BTreeMap<String, BTreeMap<String, u64>>;

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum Script {
    Native {
        language: String,
        json: JsonValue,
        cbor: String,
    },
    Plutus {
        language: String,
        cbor: String,
    },
}

impl TryFrom<&Utxo> for pla::v2::transaction::TransactionInput {
    type Error = OgmiosError;

    fn try_from(resp: &Utxo) -> Result<Self> {
        Ok(Self {
            transaction_id: (&resp.transaction).try_into()?,
            index: resp.index.into(),
        })
    }
}

impl TryFrom<&TransactionId> for pla::v2::transaction::TransactionHash {
    type Error = OgmiosError;

    fn try_from(resp: &TransactionId) -> Result<Self> {
        Ok(Self(pla::v2::crypto::LedgerBytes(
            HEXLOWER
                .decode(&resp.id.clone().into_bytes())
                .map_err(|source| OgmiosError::ConversionError {
                    label: "TransactionHash".to_string(),
                    source: anyhow!(source),
                })?,
        )))
    }
}

impl TryFrom<TransactionId> for pla::v2::transaction::TransactionHash {
    type Error = OgmiosError;

    fn try_from(resp: TransactionId) -> Result<pla::v2::transaction::TransactionHash> {
        (&resp).try_into()
    }
}

impl From<&pla::v2::transaction::TransactionHash> for TransactionId {
    fn from(tx_hash: &pla::v2::transaction::TransactionHash) -> TransactionId {
        let pla::v2::transaction::TransactionHash(pla::v2::crypto::LedgerBytes(bytes)) = tx_hash;
        TransactionId {
            id: HEXLOWER.encode(&bytes),
        }
    }
}

impl From<pla::v2::transaction::TransactionHash> for TransactionId {
    fn from(tx_hash: pla::v2::transaction::TransactionHash) -> TransactionId {
        From::from(&tx_hash)
    }
}

impl TryFrom<&Utxo> for FullTransactionOutput {
    type Error = OgmiosError;

    fn try_from(utxo: &Utxo) -> Result<FullTransactionOutput> {
        let value = pla::v2::value::Value(
            utxo.value
                .iter()
                .map(|(cur_sym, tokens)| {
                    let cur_sym: Result<_> = if cur_sym == "ada" {
                        Ok(pla::v2::value::CurrencySymbol::Ada)
                    } else {
                        Ok(pla::v2::value::CurrencySymbol::NativeToken(
                            pla::v2::script::MintingPolicyHash(pla::v2::script::ScriptHash(
                                pla::v2::crypto::LedgerBytes(
                                    HEXLOWER.decode(&cur_sym.clone().into_bytes()).map_err(
                                        |source| OgmiosError::ConversionError {
                                            label: "MintingPolicyHash".to_string(),
                                            source: anyhow!(source),
                                        },
                                    )?,
                                ),
                            )),
                        ))
                    };

                    let tokens: Result<_> = tokens
                        .iter()
                        .map(|(token_name, amount)| {
                            let token_name: Result<_> = if token_name == "lovelace" {
                                Ok(pla::v2::value::TokenName::ada())
                            } else {
                                Ok(pla::v2::value::TokenName(pla::v2::crypto::LedgerBytes(
                                    HEXLOWER.decode(&token_name.clone().into_bytes()).map_err(
                                        |source| OgmiosError::ConversionError {
                                            label: "TokenName".to_string(),
                                            source: anyhow!(source),
                                        },
                                    )?,
                                )))
                            };
                            Ok((token_name?, BigInt::from(*amount)))
                        })
                        .collect();

                    Ok((cur_sym?, tokens?))
                })
                .collect::<Result<BTreeMap<pla::v2::value::CurrencySymbol, _>>>()?,
        );

        let datum = if let Some(dh) = &utxo.datum_hash {
            let bytes = HEXLOWER
                .decode(&dh.clone().into_bytes())
                .map_err(|source| OgmiosError::ConversionError {
                    label: "DatumHash".to_string(),
                    source: anyhow!(source),
                })?;

            pla::v2::datum::OutputDatum::DatumHash(pla::v2::datum::DatumHash(
                pla::v2::crypto::LedgerBytes(bytes),
            ))
        } else if let Some(d) = &utxo.datum {
            let plutus_data = &csl::plutus::PlutusData::from_hex(&d).map_err(|source| {
                OgmiosError::ConversionError {
                    label: "PlutusData".to_string(),
                    source: anyhow!(source),
                }
            })?;
            pla::v2::datum::OutputDatum::InlineDatum(pla::v2::datum::Datum(
                plutus_data.try_to_pla()?,
            ))
        } else {
            pla::v2::datum::OutputDatum::None
        };

        let reference_script = utxo
            .script
            .clone()
            .map(|script| -> Result<_> {
                match script {
                    Script::Native { cbor, .. } => {
                        let script = csl::NativeScript::from_hex(&cbor).map_err(|source| {
                            OgmiosError::ConversionError {
                                label: "NativeScript".to_string(),
                                source: anyhow!(source),
                            }
                        })?;

                        Ok(crate::utils::script::Script::NativeScript(script))
                    }
                    Script::Plutus { cbor, language } => {
                        let script =
                            csl::plutus::PlutusScript::from_hex(&cbor).map_err(|source| {
                                OgmiosError::ConversionError {
                                    label: "PlutusScript".to_string(),
                                    source: anyhow!(source),
                                }
                            })?;

                        let plutus_version = match &language[..] {
                            "plutus:v1" => crate::utils::script::PlutusVersion::V1,
                            "plutus:v2" => crate::utils::script::PlutusVersion::V2,
                            _ => Err(OgmiosError::ConversionError {
                                label: "Plutus language".to_string(),
                                source: anyhow!("Couldn't parse Plutus language version."),
                            })?,
                        };

                        Ok(crate::utils::script::Script::PlutusScript(
                            script,
                            plutus_version,
                        ))
                    }
                }
            })
            .transpose()?;

        Ok(FullTransactionOutput {
            address: csl::address::Address::from_bech32(&utxo.address)
                .map_err(|source| OgmiosError::ConversionError {
                    label: "Address".to_string(),
                    source: anyhow!(source),
                })?
                .try_to_pla()?,
            value,
            datum,
            reference_script,
        })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct EvaluateTransactionParams {
    pub transaction: TransactionCbor,
    #[serde(rename(deserialize = "additionalUtxo"))]
    pub additional_utxo: Vec<Utxo>,
}

impl ToRpcParams for EvaluateTransactionParams {
    fn to_rpc_params(self) -> std::result::Result<Option<Box<RawValue>>, serde_json::Error> {
        serde_json::value::to_raw_value(&self).map(Some)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct TransactionCbor {
    pub cbor: String,
}

pub(crate) type EvaluateTransactionResponse = Vec<Budgets>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct Budgets {
    pub validator: Validator,
    pub budget: Budget,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct Validator {
    pub purpose: String,
    pub index: u64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct Budget {
    pub memory: u64,
    pub cpu: u64,
}

pub(crate) type SubmitTransactionParams = EvaluateTransactionParams;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct SubmitTransactionResponse {
    pub transaction: TransactionId,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct AcquireMempoolResponse {
    pub acquired: String,
    pub slot: u64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct ReleaseMempoolResponse {
    pub released: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub(crate) enum NextTransactionResponse {
    // We could request a full transaction
    TransactionId { transaction: Option<TransactionId> },
}

pub type QueryLedgerStateProtocolParametersResponse = ProtocolParameters;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProtocolParameters {
    #[serde(rename(deserialize = "minFeeCoefficient"))]
    pub min_fee_coefficient: u64,
    #[serde(rename(deserialize = "minFeeConstant"))]
    pub min_fee_constant: AdaOnly,
    #[serde(rename(deserialize = "minUtxoDepositCoefficient"))]
    pub min_utxo_deposit_coefficient: u64,
    #[serde(rename(deserialize = "minUtxoDepositConstant"))]
    pub min_utxo_deposit_constant: AdaOnly,
    #[serde(rename(deserialize = "maxBlockBodySize"))]
    pub max_block_body_size: Bytes,
    #[serde(rename(deserialize = "maxBlockHeaderSize"))]
    pub max_block_header_size: Bytes,
    #[serde(rename(deserialize = "maxTransactionSize"))]
    pub max_transaction_size: Option<Bytes>,
    #[serde(rename(deserialize = "maxValueSize"))]
    pub max_value_size: Option<Bytes>,
    #[serde(rename(deserialize = "extraEntropy"))]
    pub extra_entropy: Option<Nonce>,
    #[serde(rename(deserialize = "stakeCredentialDeposit"))]
    pub stake_credential_deposit: AdaOnly,
    #[serde(rename(deserialize = "stakePoolDeposit"))]
    pub stake_pool_deposit: AdaOnly,
    #[serde(rename(deserialize = "stakePoolRetirementEpochBound"))]
    pub stake_pool_retirement_epoch_bound: u64,
    #[serde(rename(deserialize = "stakePoolPledgeInfluence"))]
    pub stake_pool_pledge_influence: Ratio,
    #[serde(rename(deserialize = "minStakePoolCost"))]
    pub min_stake_pool_cost: AdaOnly,
    #[serde(rename(deserialize = "desiredNumberOfStakePools"))]
    pub desired_number_of_stake_pools: u64,
    #[serde(rename(deserialize = "federatedBlockProductionRatio"))]
    pub federated_block_production_ratio: Option<Ratio>,
    #[serde(rename(deserialize = "monetaryExpansion"))]
    pub monetary_expansion: Ratio,
    #[serde(rename(deserialize = "treasuryExpansion"))]
    pub treasury_expansion: Ratio,
    #[serde(rename(deserialize = "collateralPercentage"))]
    pub collateral_percentage: Option<u64>,
    #[serde(rename(deserialize = "maxCollateralInputs"))]
    pub max_collateral_inputs: Option<u64>,
    #[serde(rename(deserialize = "plutusCostModels"))]
    pub plutus_cost_models: Option<CostModels>,
    #[serde(rename(deserialize = "scriptExecutionPrices"))]
    pub script_execution_prices: Option<ScriptExecutionPrices>,
    #[serde(rename(deserialize = "maxExecutionUnitsPerTransaction"))]
    pub max_execution_units_per_transaction: Option<ExecutionUnits>,
    #[serde(rename(deserialize = "maxExecutionUnitsPerBlock"))]
    pub max_execution_units_per_block: Option<ExecutionUnits>,
    #[serde(rename(deserialize = "stakePoolVotingThresholds"))]
    pub stake_pool_voting_thresholds: Option<StakePoolVotingThresholds>,
    #[serde(rename(deserialize = "constitutionalCommitteeMinSize"))]
    pub constitutional_committee_min_size: Option<u64>,
    #[serde(rename(deserialize = "constitutionalCommitteeMaxTermLength"))]
    pub constitutional_committee_max_term_length: Option<u64>,
    #[serde(rename(deserialize = "governanceActionLifetime"))]
    pub governance_action_lifetime: Option<Epoch>,
    #[serde(rename(deserialize = "governanceActionDeposit"))]
    pub governance_action_deposit: Option<AdaOnly>,
    #[serde(rename(deserialize = "delegateRepresentativeVotingThresholds"))]
    pub delegate_representative_voting_thresholds: Option<DelegateRepresentativeVotingThresholds>,
    #[serde(rename(deserialize = "delegateRepresentativeDeposit"))]
    pub delegate_representative_deposit: Option<AdaOnly>,
    #[serde(rename(deserialize = "delegateRepresentativeMaxIdleTime"))]
    pub delegate_representative_max_idle_time: Option<Epoch>,
    pub version: ProtocolVersion,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Bytes {
    pub bytes: u64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AdaOnly {
    pub ada: LovelaceOnly,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LovelaceOnly {
    pub lovelace: i64,
}

pub type Nonce = String;

pub type Ratio = String;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ScriptExecutionPrices {
    pub memory: Ratio,
    pub cpu: Ratio,
}

pub type CostModels = BTreeMap<String, Vec<i64>>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExecutionUnits {
    pub memory: i64,
    pub cpu: i64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StakePoolVotingThresholds {
    #[serde(rename(deserialize = "noConfidence"))]
    pub no_confidence: Ratio,
    #[serde(rename(deserialize = "constitutionalCommittee"))]
    pub constitutional_committee: ConstitutionalCommittee,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConstitutionalCommittee {
    pub default: Ratio,
    #[serde(rename(deserialize = "stateOfNoConfidence"))]
    pub state_of_no_confidence: Ratio,
}
pub type Epoch = u64;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DelegateRepresentativeVotingThresholds {
    #[serde(rename(deserialize = "noConfidence"))]
    pub no_confidence: Ratio,
    pub constitution: Ratio,
    #[serde(rename(deserialize = "constitutionalCommittee"))]
    pub constitutional_committee: ConstitutionalCommittee,
    #[serde(rename(deserialize = "hardforkInitiation"))]
    pub hardfork_initiation: Ratio,
    #[serde(rename(deserialize = "propocolParametersUpdate"))]
    pub propocol_parameters_update: ProtocolParametersUpdate,
    #[serde(rename(deserialize = "treasuryWithdrawals"))]
    pub treasury_withdrawals: Ratio,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProtocolParametersUpdate {
    pub network: Ratio,
    pub economic: Ratio,
    pub technical: Ratio,
    pub governance: Ratio,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProtocolVersion {
    pub major: u32,
    pub minor: u32,
    pub patch: Option<u32>,
}

impl TryFrom<ProtocolParameters> for chain_query::ProtocolParameters {
    type Error = OgmiosError;

    fn try_from(pparams: ProtocolParameters) -> Result<Self> {
        let script_execution_prices = match pparams.script_execution_prices {
            None => None,
            Some(ex_prices) => {
                let mem_prices = parse_ratio(ex_prices.memory)?;
                let cpu_prices = parse_ratio(ex_prices.cpu)?;
                Some(csl::plutus::ExUnitPrices::new(
                    &csl::UnitInterval::new(
                        &csl::utils::to_bignum(mem_prices.0),
                        &csl::utils::to_bignum(mem_prices.1),
                    ),
                    &csl::UnitInterval::new(
                        &csl::utils::to_bignum(cpu_prices.0),
                        &csl::utils::to_bignum(cpu_prices.1),
                    ),
                ))
            }
        };

        Ok(chain_query::ProtocolParameters {
            min_fee_coefficient: csl::utils::to_bignum(pparams.min_fee_coefficient as u64),
            min_fee_constant: csl::utils::to_bignum(pparams.min_fee_constant.ada.lovelace as u64),
            min_utxo_deposit_coefficient: csl::utils::to_bignum(
                pparams.min_utxo_deposit_coefficient as u64,
            ),
            min_utxo_deposit_constant: csl::utils::to_bignum(
                pparams.min_utxo_deposit_constant.ada.lovelace as u64,
            ),
            stake_pool_deposit: csl::utils::to_bignum(
                pparams.stake_pool_deposit.ada.lovelace as u64,
            ),
            stake_credential_deposit: csl::utils::to_bignum(
                pparams.stake_credential_deposit.ada.lovelace as u64,
            ),
            max_value_size: pparams.max_value_size.map(|x| x.bytes as u32),

            max_transaction_size: pparams.max_transaction_size.map(|x| x.bytes as u32),
            script_execution_prices,
            plutus_cost_models: pparams.plutus_cost_models.map(to_costmdls),
        })
    }
}

fn parse_ratio(ratio: Ratio) -> Result<(u64, u64)> {
    let (left, right) = ratio.split_once('/').ok_or(OgmiosError::ConversionError {
        label: "Ratio".to_string(),
        source: anyhow!("Not a ratio"),
    })?;
    Ok((
        FromStr::from_str(left).map_err(|source: std::num::ParseIntError| {
            OgmiosError::ConversionError {
                label: "Ratio numerator".to_string(),
                source: anyhow!(source),
            }
        })?,
        FromStr::from_str(right).map_err(|source: std::num::ParseIntError| {
            OgmiosError::ConversionError {
                label: "Ratio denumerator".to_string(),
                source: anyhow!(source),
            }
        })?,
    ))
}

fn to_costmdls(cost_models: CostModels) -> csl::plutus::Costmdls {
    let mut costmdls = csl::plutus::Costmdls::new();

    cost_models.iter().for_each(|(lang, costs)| {
        let mut cost_model = csl::plutus::CostModel::new();
        costs.iter().enumerate().for_each(|(index, model)| {
            let _ = cost_model.set(index, &model.try_to_csl().unwrap());
        });
        let language = match &lang[..] {
            "plutus:v1" => csl::plutus::Language::new_plutus_v1(),
            "plutus:v2" => csl::plutus::Language::new_plutus_v2(),
            _ => panic!("Unknown Plutus language version"),
        };
        costmdls.insert(&language, &cost_model);
    });
    costmdls
}

pub fn to_redeemer_tag(str: &str) -> Result<csl::plutus::RedeemerTag> {
    match str {
        "spend" => Ok(csl::plutus::RedeemerTag::new_spend()),
        "certificate" => Ok(csl::plutus::RedeemerTag::new_cert()),
        "mint" => Ok(csl::plutus::RedeemerTag::new_mint()),
        "withdrawal" => Ok(csl::plutus::RedeemerTag::new_reward()),
        _ => Err(OgmiosError::ConversionError {
            label: "RedeemerTag".to_string(),
            source: anyhow!("Invalid RedeemerTag"),
        }),
    }
}
