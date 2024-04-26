use core::str::FromStr;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::BufReader;
use std::thread::JoinHandle;

use oura::utils::ChainWellKnownInfo;
use oura::{
    sources::MagicArg,
    utils::{PREPROD_MAGIC, PREVIEW_MAGIC},
};
use pallas::network::miniprotocols::MAINNET_MAGIC;
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
pub enum NetworkMagic {
    PREPROD,
    PREVIEW,
    MAINNET,
}

#[derive(Clone, Debug)]
pub struct NetworkMagicRaw {
    pub magic: u64,
    pub chain_info_path: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NetworkMagicParseErr;

impl fmt::Display for NetworkMagicParseErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "provided string was not `preprod` or `preview` or `mainnet`".fmt(f)
    }
}
impl Error for NetworkMagicParseErr {}

impl FromStr for NetworkMagic {
    type Err = NetworkMagicParseErr;
    fn from_str(s: &str) -> Result<NetworkMagic, Self::Err> {
        match &s.to_lowercase()[..] {
            "preprod" => Ok(NetworkMagic::PREPROD),
            "preview" => Ok(NetworkMagic::PREVIEW),
            "mainnet" => Ok(NetworkMagic::MAINNET),
            _ => Err(NetworkMagicParseErr),
        }
    }
}

pub trait IsNetworkMagic {
    /// Convert to Oura's `MagicArg`, which is just a `u64`.
    fn to_magic_arg(&self) -> MagicArg;
    /// Obtain `ChainWellKnownInfo` corresponding to the network.
    fn to_chain_info(&self) -> ChainWellKnownInfo;
}

impl IsNetworkMagic for NetworkMagic {
    fn to_magic_arg(&self) -> MagicArg {
        MagicArg(match self {
            NetworkMagic::PREPROD => PREPROD_MAGIC,
            NetworkMagic::PREVIEW => PREVIEW_MAGIC,
            NetworkMagic::MAINNET => MAINNET_MAGIC,
        })
    }

    fn to_chain_info(&self) -> ChainWellKnownInfo {
        match self {
            NetworkMagic::PREPROD => ChainWellKnownInfo::preprod(),
            NetworkMagic::PREVIEW => ChainWellKnownInfo::preview(),
            NetworkMagic::MAINNET => ChainWellKnownInfo::mainnet(),
        }
    }
}

impl IsNetworkMagic for NetworkMagicRaw {
    fn to_magic_arg(&self) -> MagicArg {
        MagicArg(self.magic)
    }

    fn to_chain_info(&self) -> ChainWellKnownInfo {
        let file =
            File::open(self.chain_info_path.clone()).expect("Chain Info not found at given path");
        let reader = BufReader::new(file);
        serde_json::from_reader(reader).expect("Invalid JSON format for ChainWellKnownInfo")
    }
}

// Structure holding the thread handles associated to the indexer. These threads are never-ending.
pub struct Indexer {
    pub source_handle: JoinHandle<()>,
    pub filter_handle: Option<JoinHandle<()>>,
    pub sink_handle: JoinHandle<()>,
}
