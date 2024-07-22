use crate::chain_query::Network;
use crate::utils::ogmios::client::OgmiosClientConfigBuilder;
use anyhow::anyhow;
use derive_builder::Builder;
use serde::Deserialize;

use std::ops::Deref;
use std::path::PathBuf;
use std::process::Stdio;
use tokio::process::{Child, Command};
use tokio::sync::{RwLock, RwLockReadGuard};
use url::Url;

use super::client::OgmiosClient;
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
    handler: RwLock<Option<Child>>,
    config: OgmiosLauncherConfig,
}

pub struct GuardedOgmiosClient<'a> {
    _guard: RwLockReadGuard<'a, Option<Child>>,
    client: OgmiosClient,
}

impl<'a> Deref for GuardedOgmiosClient<'a> {
    type Target = OgmiosClient;

    fn deref(&self) -> &Self::Target {
        &self.client
    }
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
            handler: RwLock::new(Some(handler)),
            config,
        })
    }

    pub fn get_config(&self) -> &OgmiosLauncherConfig {
        &self.config
    }

    /// Kill ogmios process
    pub async fn kill(&mut self) {
        let mut guard = self.handler.write().await;

        if let Some(mut child) = guard.take() {
            let _ = child.kill().await;
            let _ = child.wait().await;
        }
    }

    /// Connect to the ogmios service managed by the launcher
    pub async fn connect<'a>(&'a self) -> Result<GuardedOgmiosClient<'a>> {
        let guard = self.handler.read().await;

        if guard.is_none() {
            return Err(OgmiosError::StartupError(anyhow!(
                "The ogmios service managed by this launcher has been killed"
            )));
        }

        let config = self.get_config();

        let url = match config.host.as_str() {
            "localhost" | "127.0.0.1" | "0.0.0.0" => {
                if config.port != 0 {
                    Some(Url::parse(&format!("http://127.0.0.1:{}", config.port)).unwrap())
                } else {
                    None
                }
            }
            _ => None,
        }
        .ok_or(OgmiosError::StartupError(anyhow!(
            "Unable to determine the url endpoint of the ogmios backend"
        )))?;

        let client_config = OgmiosClientConfigBuilder::default()
            .url(url)
            .network(config.network.clone())
            .startup_timeout(300)
            .build()
            .unwrap();

        let client = OgmiosClient::connect(client_config).await?;

        Ok(GuardedOgmiosClient {
            _guard: guard,
            client,
        })
    }
}
