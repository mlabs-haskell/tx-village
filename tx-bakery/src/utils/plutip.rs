use crate::chain_query::Network;
use crate::wallet::WalletError;
use anyhow::anyhow;
use data_encoding::HEXLOWER;
use derive_builder::Builder;
use nix::sys::signal::{self, Signal};
use nix::unistd::Pid;
use plutus_ledger_api::v2::crypto::{Ed25519PubKeyHash, LedgerBytes};
use serde::Deserialize;
use std::process::{Child, Command, Stdio};
use std::time;
use thiserror::Error;
use tokio;
use tokio::fs;

use super::key_wallet::KeyWallet;
use super::key_wallet::KeyWalletError;

#[derive(Error, Debug)]
pub enum PlutipError {
    #[error("Failed to start Plutip - {label}: {source}")]
    StartupError {
        label: String,
        source: anyhow::Error,
    },

    #[error(transparent)]
    KeyWalletError(#[from] KeyWalletError),
}

impl From<PlutipError> for WalletError {
    fn from(err: PlutipError) -> WalletError {
        WalletError(anyhow!(err))
    }
}

#[derive(Debug, Builder, Clone, Deserialize)]
pub struct PlutipConfig {
    #[builder(default = r#""plutip-info.json".to_string()"#, setter(skip))]
    dump_path: String,
    #[builder(default = r#"".wallets".to_string()"#, setter(skip))]
    wallets_dir: String,
    #[builder(default = "false")]
    verbose: bool,
    #[builder(default = "1")]
    wallets: u32,
    #[builder(default = "1")]
    _ada: u32,
    #[builder(default = "1")]
    _lovelace: u32,
    #[builder(default = "1")]
    _utxos: u32,
    #[builder(default = "0.2")]
    slot_length: f32,
    #[builder(default = "80")]
    epoch_size: u32,
}

#[derive(Debug, serde::Deserialize)]
struct PlutipInfo {
    #[serde(rename(deserialize = "ciNodeSocket"))]
    node_socket: String,
    #[serde(rename(deserialize = "ciWallets"))]
    wallets: Vec<(String, String)>,
}

pub struct Plutip {
    handler: Child,
    config: PlutipConfig,
    info: PlutipInfo,
}

impl Plutip {
    const NETWORK: Network = Network::Mainnet;

    pub async fn start(config: &PlutipConfig) -> Result<Self, PlutipError> {
        let args = [
            "--dump-info-json",
            &config.dump_path,
            "--wallets-dir",
            &config.wallets_dir,
            "--wallets",
            &config.wallets.to_string(),
            "--slot-len",
            &format!("{}s", config.slot_length),
            "--epoch-size",
            &config.epoch_size.to_string(),
        ];

        let handler = Command::new("local-cluster")
            .args(args)
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
            .spawn()
            .map_err(|source| PlutipError::StartupError {
                label: "Starting cluster".to_string(),
                source: anyhow!(source),
            })?;

        loop {
            let metadata = fs::metadata(&config.dump_path).await;
            if metadata.map_or(false, |m| m.is_file()) {
                break;
            } else {
                tokio::time::sleep(time::Duration::from_secs(1)).await;
            }
        }

        let info = Self::fetch_info(&config.dump_path).await?;
        Ok(Self {
            handler,
            config: config.clone(),
            info,
        })
    }

    /// Fetch Plutip info (node socket path, wallet pkh, etc.)
    async fn fetch_info(path: &str) -> Result<PlutipInfo, PlutipError> {
        let info_str =
            fs::read_to_string(path)
                .await
                .map_err(|source| PlutipError::StartupError {
                    label: "Reading Plutip info file".to_string(),
                    source: anyhow!(source),
                })?;

        serde_json::from_str(&info_str).map_err(|source| PlutipError::StartupError {
            label: "Parsing Plutip info file".to_string(),
            source: anyhow!(source),
        })
    }

    fn get_wallet_pkh(&self, wallet_idx: usize) -> Ed25519PubKeyHash {
        let pkh_str = &self.info.wallets[wallet_idx].0;

        Ed25519PubKeyHash(LedgerBytes(
            HEXLOWER.decode(&pkh_str.to_owned().into_bytes()).unwrap(),
        ))
    }

    pub async fn get_wallet(&self, wallet_idx: usize) -> Result<KeyWallet, PlutipError> {
        let path = format!(
            "{}/signing-key-{}.skey",
            self.config.wallets_dir,
            HEXLOWER.encode(&self.get_wallet_pkh(wallet_idx).0 .0)
        );
        Ok(KeyWallet::new(&path, None).await?)
    }

    pub async fn get_own_wallet(&self) -> Result<KeyWallet, PlutipError> {
        self.get_wallet(0).await
    }

    /// Get the path to the active cardano-node socket
    pub fn get_node_socket(&self) -> String {
        self.info.node_socket.clone()
    }

    pub fn get_network(&self) -> Network {
        Self::NETWORK
    }

    /// Get the path cardano-node configuration file
    pub async fn get_node_config_path(&self) -> String {
        let mut path = fs::canonicalize(&self.info.node_socket).await.unwrap();
        path.pop();
        path.pop();
        path.push("pool-1");
        path.push("node.config");

        path.to_str().unwrap().to_string()
    }

    /// Kill plutip process
    pub fn kill(&mut self) -> Result<(), std::io::Error> {
        self.cleanup()?;
        let plutip_pid = i32::try_from(self.handler.id()).map(Pid::from_raw).unwrap();
        signal::kill(plutip_pid, Signal::SIGINT)?;
        let _ = self.handler.wait();
        Ok(())
    }

    /// Cleanup all resources used by plutip
    pub fn cleanup(&mut self) -> Result<(), std::io::Error> {
        std::fs::remove_file(&self.config.dump_path)?;
        std::fs::remove_dir_all(&self.config.wallets_dir)
    }
}

impl Drop for Plutip {
    fn drop(&mut self) {
        self.kill().expect("Failed to clean up after Plutip.");
    }
}
