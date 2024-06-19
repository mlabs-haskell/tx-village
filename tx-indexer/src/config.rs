use crate::{
    filter::Filter,
    handler::{callback::EventHandler, retry::RetryPolicy},
};
use anyhow::anyhow;
use core::str::FromStr;
use oura::{sources::MagicArg, utils::ChainWellKnownInfo};
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::BufReader;
use strum_macros::Display;

pub struct TxIndexerConfig<H: EventHandler> {
    pub handler: H,
    pub node_address: NodeAddress,
    pub network: NetworkConfig,
    /// Slot number and hash as hex string (optional).
    /// If not provided, sync will begin from the tip of the chain.
    pub since_slot: Option<(u64, String)>,
    /// Minimum depth a block has to be from the tip for it to be considered "confirmed"
    /// See: https://oura.txpipe.io/v1/advanced/rollback_buffer
    pub safe_block_depth: usize,
    // Filter transaction events by specific component(s).
    pub event_filter: Filter,
    /// Retry policy - how much to retry for each event callback failure
    /// This only takes effect on ErrorPolicy for a particular error is `Retry`.
    /// Once retries are exhausted, the handler will error (same treatment as ErrorPolicy::Exit)
    pub retry_policy: RetryPolicy,
}

impl<H: EventHandler> TxIndexerConfig<H> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        handler: H,
        node_address: NodeAddress,
        network: NetworkConfig,
        since_slot: Option<(u64, String)>,
        safe_block_depth: usize,
        event_filter: Filter,
        retry_policy: RetryPolicy,
    ) -> Self {
        Self {
            handler,
            node_address,
            network,
            since_slot,
            safe_block_depth,
            event_filter,
            retry_policy,
        }
    }
}

/// Simple description on how to connect to a local or remote node.
/// Used to build Oura source config.
pub enum NodeAddress {
    /// Path to Unix node.socket
    UnixSocket(String),
    /// Hostname and port number for TCP connection to remote node
    TcpAddress(String, u16),
}

/// Typed network magic restricted to specific networks fully supported by Oura.
#[derive(Clone, Debug, Display)]
pub enum NetworkName {
    PREPROD,
    PREVIEW,
    MAINNET,
}

#[derive(Clone, Debug)]
pub enum NetworkConfig {
    ConfigPath {
        node_config_path: String,
        magic: u64,
    },
    WellKnown(NetworkName),
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

impl NetworkConfig {
    pub fn to_magic_arg(&self) -> MagicArg {
        MagicArg(match self {
            NetworkConfig::WellKnown(network_name) => match network_name {
                NetworkName::PREPROD => pallas::network::miniprotocols::PRE_PRODUCTION_MAGIC,
                NetworkName::PREVIEW => pallas::network::miniprotocols::PREVIEW_MAGIC,
                NetworkName::MAINNET => pallas::network::miniprotocols::MAINNET_MAGIC,
            },
            NetworkConfig::ConfigPath { magic, .. } => *magic,
        })
    }

    pub fn to_chain_info(&self) -> Result<ChainWellKnownInfo, anyhow::Error> {
        Ok(match self {
            NetworkConfig::WellKnown(network_name) => match network_name {
                NetworkName::PREPROD => ChainWellKnownInfo::preprod(),
                NetworkName::PREVIEW => ChainWellKnownInfo::preview(),
                NetworkName::MAINNET => ChainWellKnownInfo::mainnet(),
            },
            NetworkConfig::ConfigPath {
                node_config_path, ..
            } => {
                let file = File::open(node_config_path.clone())
                    .map_err(|err| anyhow!("Chain Info not found at given path: {}", err))?;
                let reader = BufReader::new(file);
                serde_json::from_reader(reader).expect("Invalid JSON format for ChainWellKnownInfo")
            }
        })
    }
}

// Encapsulating usage of deprecated stuff (impossible to construct struct without it).
// This avoids having to put "#![allow(deprecated)]" on the top of this file.
pub mod deprecation_usage {
    #![allow(deprecated)]

    use oura::mapper::Config as MapperConfig;
    use oura::sources::n2c::Config as N2CConfig;
    use oura::sources::n2n::Config as N2NConfig;
    use oura::sources::{AddressArg, IntersectArg, MagicArg, PointArg};

    pub fn n2c_config(
        addr: AddressArg,
        magic: MagicArg,
        since_slot: Option<(u64, String)>,
        safe_block_depth: usize,
    ) -> N2CConfig {
        N2CConfig {
            address: addr,
            magic: Some(magic),
            intersect: since_slot
                .map(|since_slot| IntersectArg::Point(PointArg(since_slot.0, since_slot.1))),
            mapper: MapperConfig {
                include_transaction_details: true,
                ..Default::default()
            },
            min_depth: safe_block_depth,
            retry_policy: None,
            finalize: None,
            // Deprecated fields
            since: None,
            well_known: None,
        }
    }

    pub fn n2n_config(
        addr: AddressArg,
        magic: MagicArg,
        since_slot: Option<(u64, String)>,
        safe_block_depth: usize,
    ) -> N2NConfig {
        N2NConfig {
            address: addr,
            magic: Some(magic),
            intersect: since_slot
                .map(|since_slot| IntersectArg::Point(PointArg(since_slot.0, since_slot.1))),
            mapper: MapperConfig {
                include_transaction_details: true,
                ..Default::default()
            },
            min_depth: safe_block_depth,
            retry_policy: None,
            finalize: None,
            // Deprecated fields
            since: None,
            well_known: None,
        }
    }
}

pub use self::deprecation_usage::*;
