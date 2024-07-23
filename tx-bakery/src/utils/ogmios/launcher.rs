use crate::chain_query::Network;
use anyhow::anyhow;
use derive_builder::Builder;
use serde::Deserialize;

use std::path::PathBuf;
use std::process::Stdio;
use tokio::process::{Child, Command};
use tokio::sync::Mutex;

use super::error::{OgmiosError, Result};

#[derive(Debug, Builder, Clone, Deserialize)]
pub struct OgmiosLauncherConfig {
    #[builder(default = r#""".try_into().unwrap()"#)]
    pub node_socket: PathBuf,
    #[builder(default = r#"".node.config".try_into().unwrap()"#)]
    pub node_config: PathBuf,
    #[builder(default = r#""127.0.0.1".to_string()"#)]
    pub host: String,
    #[builder(default = "1337")]
    pub port: u16,
    #[builder(default = "false")]
    pub verbose: bool,
    #[builder(default = r#"Network::Testnet"#)]
    pub network: Network,
    #[builder(default = "180")]
    pub timeout: u32,
    #[builder(default = "1000")]
    pub max_in_flight: u32,
    #[builder(default = "90")]
    pub startup_timeout: u64,
}

pub struct OgmiosLauncher {
    handler: Mutex<Option<Child>>,
    config: OgmiosLauncherConfig,
}

impl OgmiosLauncher {
    pub async fn start(config: OgmiosLauncherConfig) -> Result<Self> {
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
            .map_err(|err| OgmiosError::StartupError(anyhow!("Unable to spawn ogmios: {}", err)))?;

        Ok(Self {
            handler: Mutex::new(Some(handler)),
            config,
        })
    }

    pub fn get_config(&self) -> &OgmiosLauncherConfig {
        &self.config
    }

    /// Kill ogmios process
    pub async fn kill(&mut self) {
        let mut guard = self.handler.lock().await;

        if let Some(mut child) = guard.take() {
            let _ = child.kill().await;
            let _ = child.wait().await;
        }
    }
}
