use core::str::FromStr;
use std::fmt;
use std::pin::Pin;
use std::{error::Error, future::Future};

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

impl NetworkMagic {
    /// Convert typed `NetworkMagic` to Oura's `MagicArg`, which is just a `u64`.
    pub fn to_magic_arg(self) -> MagicArg {
        MagicArg(match self {
            NetworkMagic::PREPROD => PREPROD_MAGIC,
            NetworkMagic::PREVIEW => PREVIEW_MAGIC,
            NetworkMagic::MAINNET => MAINNET_MAGIC,
        })
    }
}

// https://stackoverflow.com/questions/77589520/lifetime-of-struct-with-field-of-type-boxed-async-callback-must-outlive-static
pub type AsyncResult<E> = dyn Future<Output = Result<(), E>> + Send + Sync;
pub type AsyncFunction<Arg, Result> = dyn Fn(Arg) -> Pin<Box<Result>> + Send + Sync;
