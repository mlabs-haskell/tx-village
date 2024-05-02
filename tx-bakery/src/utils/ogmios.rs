use self::api::*;
use self::error::{OgmiosError, Result};
use crate::chain_query::{
    ChainQuery, ChainQueryError, ChainTip, EraSummary, FullTransactionOutput, Network,
    ProtocolParameters,
};
use crate::submitter::{Submitter, SubmitterError};
use crate::utils::pla_to_csl::TryToCSL;
use anyhow::anyhow;
use cardano_serialization_lib as csl;
use chrono::{DateTime, Utc};
use derive_builder::Builder;
use jsonrpsee::core::client::ClientT;
use jsonrpsee::core::params::ObjectParams;
use jsonrpsee::core::traits::ToRpcParams;
use jsonrpsee::rpc_params;
use jsonrpsee::ws_client::{WsClient, WsClientBuilder};
use plutus_ledger_api::v2::address::Address;
use plutus_ledger_api::v2::transaction::{TransactionHash, TransactionInput};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::process::Stdio;
use std::time;
use tokio::process::{Child, Command};

mod api;
mod error;

/// Ogmios client for interacting with the blockchain
pub struct Ogmios {
    /// Handler is only populated if we manage the runtime of Ogmios
    handler: Option<Child>,
    config: OgmiosClientConfig,
    client: WsClient,
}

impl ChainQuery for Ogmios {
    fn get_network(&self) -> Network {
        self.config.network.clone()
    }

    async fn query_system_start(&self) -> std::result::Result<DateTime<Utc>, ChainQueryError> {
        let resp: QueryNetworkStartTimeResponse = self
            .request("queryNetwork/startTime", rpc_params![])
            .await?;

        Ok(DateTime::parse_from_rfc3339(&resp)
            .map_err(|source| OgmiosError::ConversionError {
                label: "SystemStart datetime".to_string(),
                source: anyhow!(source),
            })?
            .to_utc())
    }

    async fn query_era_summaries(&self) -> std::result::Result<Vec<EraSummary>, ChainQueryError> {
        let resp: QueryLedgerStateEraSummariesResponse = self
            .request("queryLedgerState/eraSummaries", rpc_params![])
            .await?;

        Ok(resp
            .into_iter()
            .map(EraSummary::try_from)
            .collect::<Result<_>>()?)
    }

    /// Query protocol parameters and cost models for all languages
    async fn query_protocol_params(
        &self,
    ) -> std::result::Result<ProtocolParameters, ChainQueryError> {
        let resp: QueryLedgerStateProtocolParametersResponse = self
            .request("queryLedgerState/protocolParameters", rpc_params![])
            .await?;

        Ok(resp.try_into()?)
    }

    /// Query current last slot of the chain
    async fn query_tip(&self) -> std::result::Result<ChainTip, ChainQueryError> {
        let resp: QueryLedgerStateTipResponse =
            self.request("queryLedgerState/tip", rpc_params![]).await?;
        Ok(resp.into())
    }

    async fn query_utxos_by_addr(
        &self,
        address: &Address,
    ) -> std::result::Result<BTreeMap<TransactionInput, FullTransactionOutput>, ChainQueryError>
    {
        let addr: csl::address::Address = address
            .try_to_csl_with(self.config.network.to_network_id())
            .map_err(|csl_err| OgmiosError::TryFromPLAError(csl_err))?;

        let addr = addr.to_bech32(Some("addr".to_owned())).map_err(|source| {
            OgmiosError::ConversionError {
                label: "Address to Bech32".to_string(),
                source: anyhow!(source),
            }
        })?;
        let params = QueryLedgerStateUtxoByAddressParams {
            addresses: vec![addr.to_string()],
        };

        let resp: QueryLedgerStateUtxoResponse =
            self.request("queryLedgerState/utxo", params).await?;

        decode_query_ledger_state_utxo_response(resp)
    }

    async fn query_utxos_by_ref(
        &self,
        references: Vec<&TransactionInput>,
    ) -> std::result::Result<BTreeMap<TransactionInput, FullTransactionOutput>, ChainQueryError>
    {
        let output_references = references
            .into_iter()
            .map(Clone::clone)
            .map(OutputReference::try_from)
            .collect::<std::result::Result<Vec<_>, _>>()?;

        let params = QueryLedgerStateUtxoByOutputReferenceParams { output_references };

        let resp: QueryLedgerStateUtxoResponse =
            self.request("queryLedgerState/utxo", params).await?;

        decode_query_ledger_state_utxo_response(resp)
    }
}

fn decode_query_ledger_state_utxo_response(
    resp: QueryLedgerStateUtxoResponse,
) -> std::result::Result<BTreeMap<TransactionInput, FullTransactionOutput>, ChainQueryError> {
    resp.iter()
        .map(|utxo| {
            Ok((
                TransactionInput::try_from(utxo).map_err(|source| {
                    OgmiosError::ConversionError {
                        label: "TransactionInput".to_string(),
                        source: anyhow!(source),
                    }
                })?,
                FullTransactionOutput::try_from(utxo).map_err(|source| {
                    OgmiosError::ConversionError {
                        label: "TransactionInput".to_string(),
                        source: anyhow!(source),
                    }
                })?,
            ))
        })
        .collect()
}

impl Submitter for Ogmios {
    /// Evaluate a transaction and return execution budgets for each script
    async fn evaluate_transaction(
        &self,
        tx_builder: &csl::tx_builder::TransactionBuilder,
        plutus_scripts: &Vec<csl::plutus::PlutusScript>,
        redeemers: &Vec<csl::plutus::Redeemer>,
    ) -> std::result::Result<
        BTreeMap<(csl::plutus::RedeemerTag, csl::utils::BigNum), csl::plutus::ExUnits>,
        SubmitterError,
    > {
        let mut tx_builder = tx_builder.clone();

        tx_builder.set_fee(&csl::utils::to_bignum(0));

        let mut witness_set = csl::TransactionWitnessSet::new();

        let mut script_witnesses = csl::plutus::PlutusScripts::new();

        plutus_scripts
            .iter()
            .for_each(|script| script_witnesses.add(script));

        let mut redeemer_witnesses = csl::plutus::Redeemers::new();

        redeemers
            .iter()
            .for_each(|redeemer| redeemer_witnesses.add(&redeemer));

        witness_set.set_plutus_scripts(&script_witnesses);
        witness_set.set_redeemers(&redeemer_witnesses);

        let tx_body = tx_builder
            .build()
            .map_err(|err| SubmitterError(anyhow::anyhow!("Transaction builder error: {}", err)))?;
        let tx = csl::Transaction::new(&tx_body, &witness_set, None);

        let params = EvaluateTransactionParams {
            transaction: TransactionCbor { cbor: tx.to_hex() },
            additional_utxo: Vec::new(),
        };

        let resp: EvaluateTransactionResponse = self.request("evaluateTransaction", params).await?;

        resp.into_iter()
            .map(|budgets| {
                Ok((
                    (
                        to_redeemer_tag(&budgets.validator.purpose)?,
                        csl::utils::to_bignum(budgets.validator.index),
                    ),
                    csl::plutus::ExUnits::new(
                        &csl::utils::to_bignum(budgets.budget.memory),
                        &csl::utils::to_bignum(budgets.budget.cpu),
                    ),
                ))
            })
            .collect()
    }

    async fn submit_transaction(
        &self,
        tx: &csl::Transaction,
    ) -> std::result::Result<TransactionHash, SubmitterError> {
        let params = SubmitTransactionParams {
            transaction: TransactionCbor { cbor: tx.to_hex() },
            additional_utxo: Vec::new(),
        };

        let resp: Result<SubmitTransactionResponse> =
            self.request("submitTransaction", params).await;

        Ok(resp?.transaction.try_into()?)
    }

    async fn await_tx_confirm(
        &self,
        tx_hash: &TransactionHash,
    ) -> std::result::Result<(), SubmitterError> {
        loop {
            let _ = self.acquire_mempool().await?;

            // TODO: hasTransaction returns a false even when there's a transaction in mempool
            // Bug report: https://github.com/CardanoSolutions/ogmios/issues/376
            //
            // let has_tx = self.has_transaction(tx_hash).await?;
            let mut has_tx = false;
            while let NextTransactionResponse::TransactionId {
                transaction: Some(resp),
            } = self.next_transaction().await?
            {
                has_tx = has_tx || resp == tx_hash.into();

                if has_tx {
                    break;
                }
            }

            if !has_tx {
                let _ = self.release_mempool().await?;
                return Ok(());
            }
        }
    }
}

impl Ogmios {
    pub async fn start(config: &OgmiosConfig) -> Result<Self> {
        let handler = Command::new("ogmios")
            .arg("--strict-rpc")
            .arg("--node-socket")
            .arg(&config.node_socket)
            .arg("--node-config")
            .arg(&config.node_config)
            .arg("--host")
            .arg(&config.host)
            .arg("--port")
            .arg(config.port.to_string())
            .arg("--timeout")
            .arg(config.timeout.to_string())
            .arg("--max-in-flight")
            .arg(config.max_in_flight.to_string())
            .stdout(if config.verbose {
                Stdio::inherit()
            } else {
                Stdio::null()
            })
            .stderr(if config.verbose {
                Stdio::inherit()
            } else {
                Stdio::null()
            })
            .kill_on_drop(true)
            .spawn()
            .map_err(|err| OgmiosError::StartupError(anyhow!(err)))?;

        let url = format!("ws://{}:{}", config.host, config.port.to_string());

        let giveup_time = chrono::Local::now() + time::Duration::from_secs(config.startup_timeout);
        loop {
            let health = Self::health(&config.host, config.port).await;
            if health
                .as_ref()
                .map_or(false, |h| h.network_synchronization == 1.0)
            {
                let client = WsClientBuilder::default().build(&url).await?;
                let service = Self {
                    handler: Some(handler),
                    config: config.clone().into(),
                    client,
                };

                return Ok(service);
            } else {
                if chrono::Local::now() > giveup_time {
                    return match health {
                        Err(err) => Err(OgmiosError::StartupError(anyhow!(
                            "health request failed: {:?}",
                            err
                        ))),
                        Ok(health) => Err(OgmiosError::StartupError(anyhow!(
                            "couldn't sync: {:?}",
                            health
                        ))),
                    };
                }
                tokio::time::sleep(time::Duration::from_secs(1)).await;
            }
        }
    }

    pub async fn connect(config: &OgmiosClientConfig) -> Result<Self> {
        let url = format!("ws://{}:{}", config.host, config.port.to_string());

        let giveup_time = chrono::Local::now() + time::Duration::from_secs(config.startup_timeout);
        loop {
            let health = Self::health(&config.host, config.port).await;
            if health
                .as_ref()
                .map_or(false, |h| h.network_synchronization == 1.0)
            {
                let client = WsClientBuilder::default().build(&url).await?;
                let service = Self {
                    handler: None,
                    config: config.clone(),
                    client,
                };

                return Ok(service);
            } else {
                if chrono::Local::now() > giveup_time {
                    return match health {
                        Err(err) => Err(OgmiosError::StartupError(anyhow!(
                            "health request failed: {:?}",
                            err
                        ))),
                        Ok(health) => Err(OgmiosError::StartupError(anyhow!(
                            "couldn't sync: {:?}",
                            health
                        ))),
                    };
                }
                tokio::time::sleep(time::Duration::from_secs(1)).await;
            }
        }
    }

    async fn acquire_mempool(&self) -> Result<u64> {
        let resp: AcquireMempoolResponse = self.request("acquireMempool", rpc_params![]).await?;
        Ok(resp.slot)
    }

    // TODO: hasTransaction returns a false even when there's a transaction in mempool
    // Bug report: https://github.com/CardanoSolutions/ogmios/issues/376
    // async fn has_transaction(&self, transaction_hash: &TransactionHash) -> Result<bool> {
    //     let params = TransactionId::from(transaction_hash);
    //     self.request("hasTransaction", params).await
    // }

    async fn next_transaction(&self) -> Result<NextTransactionResponse> {
        let mut params = ObjectParams::new();
        params.insert("fields", ()).unwrap();
        self.request("nextTransaction", params).await
    }

    async fn release_mempool(&self) -> Result<ReleaseMempoolResponse> {
        self.request("releaseMempool", rpc_params![]).await
    }

    async fn health(host: &str, port: u32) -> Result<OgmiosHealth> {
        let url = format!("http://{}:{}/health", host, port.to_string());
        Ok(reqwest::Client::new()
            .get(url)
            .send()
            .await?
            .json::<OgmiosHealth>()
            .await?)
    }

    pub fn config(&self) -> OgmiosClientConfig {
        self.config.clone()
    }

    /// Kill ogmios process
    pub async fn kill(&mut self) -> Result<()> {
        if let Some(ref mut handler) = self.handler {
            Ok(handler.kill().await?)
        } else {
            Ok(())
        }
    }

    /// Make a request to ogmios JSON RPC
    /// Ogmios slightly deviates from the JSON RPC standard, so I couldn't use a 3rd party library
    /// for this
    async fn request<P, U>(&self, method: &str, params: P) -> Result<U>
    where
        U: serde::de::DeserializeOwned + Serialize,
        P: ToRpcParams + Send,
    {
        self.client
            .request(method, params)
            .await
            .map_err(|source| OgmiosError::JSONRpcError(source))
    }
}

#[derive(Debug, Builder, Clone, Deserialize)]
pub struct OgmiosConfig {
    #[builder(default = r#""".try_into().unwrap()"#)]
    pub node_socket: PathBuf,
    #[builder(default = r#"".node.config".try_into().unwrap()"#)]
    pub node_config: PathBuf,
    #[builder(default = r#""127.0.0.1".to_string()"#)]
    pub host: String,
    #[builder(default = "1337")]
    pub port: u32,
    #[builder(default = "false")]
    pub verbose: bool,
    #[builder(default = "Network::Testnet")]
    pub network: Network,
    #[builder(default = "90")]
    pub timeout: u32,
    #[builder(default = "1000")]
    pub max_in_flight: u32,
    #[builder(default = "90")]
    pub startup_timeout: u64,
}

#[derive(Debug, Builder, Clone, Deserialize)]
pub struct OgmiosClientConfig {
    #[builder(default = r#""127.0.0.1".to_string()"#)]
    pub host: String,
    #[builder(default = "1337")]
    pub port: u32,
    #[builder(default = "false")]
    pub verbose: bool,
    #[builder(default = "Network::Testnet")]
    pub network: Network,
    #[builder(default = "90")]
    pub startup_timeout: u64,
}

impl From<OgmiosConfig> for OgmiosClientConfig {
    fn from(config: OgmiosConfig) -> OgmiosClientConfig {
        OgmiosClientConfig {
            host: config.host,
            port: config.port,
            verbose: config.verbose,
            network: config.network,
            startup_timeout: config.startup_timeout,
        }
    }
}
