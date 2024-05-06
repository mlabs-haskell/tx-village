use core::str::FromStr;
use oura::utils::ChainWellKnownInfo;
use oura::{
    sources::MagicArg,
    utils::{PREPROD_MAGIC, PREVIEW_MAGIC},
};
use pallas::network::miniprotocols::MAINNET_MAGIC;
use sqlx::PgPool;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::BufReader;
use std::thread::JoinHandle;
use strum_macros::Display;

/// Simple description on how to connect to a local or remote node.
/// Used to build Oura source config.
pub enum NodeAddress {
    /// Path to Unix node.socket
    UnixSocket(String),
    // Hostname and port number for TCP connection to remote node
    TcpAddress(String, u64),
}

/// Typed network magic restricted to specific networks fully supported by Oura.
#[derive(Clone, Debug, Display)]
pub enum NetworkName {
    PREPROD,
    PREVIEW,
    MAINNET,
}

#[derive(Clone, Debug)]
pub struct NetworkConfig {
    pub magic: u64,
    pub node_config_path: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NetworkNameParseErr;

impl fmt::Display for NetworkNameParseErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "provided string was not `preprod` or `preview` or `mainnet`".fmt(f)
    }
}
impl Error for NetworkNameParseErr {}

impl FromStr for NetworkName {
    type Err = NetworkNameParseErr;
    fn from_str(s: &str) -> Result<NetworkName, Self::Err> {
        match &s.to_lowercase()[..] {
            "preprod" => Ok(NetworkName::PREPROD),
            "preview" => Ok(NetworkName::PREVIEW),
            "mainnet" => Ok(NetworkName::MAINNET),
            _ => Err(NetworkNameParseErr),
        }
    }
}

pub trait IsNetworkConfig {
    /// Convert to Oura's `MagicArg`, which is just a `u64`.
    fn to_magic_arg(&self) -> MagicArg;
    /// Obtain `ChainWellKnownInfo` corresponding to the network.
    fn to_chain_info(&self) -> ChainWellKnownInfo;
}

impl IsNetworkConfig for NetworkName {
    fn to_magic_arg(&self) -> MagicArg {
        MagicArg(match self {
            NetworkName::PREPROD => PREPROD_MAGIC,
            NetworkName::PREVIEW => PREVIEW_MAGIC,
            NetworkName::MAINNET => MAINNET_MAGIC,
        })
    }

    fn to_chain_info(&self) -> ChainWellKnownInfo {
        match self {
            NetworkName::PREPROD => ChainWellKnownInfo::preprod(),
            NetworkName::PREVIEW => ChainWellKnownInfo::preview(),
            NetworkName::MAINNET => ChainWellKnownInfo::mainnet(),
        }
    }
}

impl IsNetworkConfig for NetworkConfig {
    fn to_magic_arg(&self) -> MagicArg {
        MagicArg(self.magic)
    }

    fn to_chain_info(&self) -> ChainWellKnownInfo {
        let file =
            File::open(self.node_config_path.clone()).expect("Chain Info not found at given path");
        let reader = BufReader::new(file);
        serde_json::from_reader(reader).expect("Invalid JSON format for ChainWellKnownInfo")
    }
}

// Structure holding the thread handles associated to the indexer. These threads are never-ending.
pub struct Indexer {
    pub source_handle: JoinHandle<()>,
    pub filter_handle: Option<JoinHandle<()>>,
    pub sink_handle: JoinHandle<()>,
    pub pg_pool: PgPool,
}
