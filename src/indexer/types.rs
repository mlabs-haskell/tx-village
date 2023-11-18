use oura::{
  sources::MagicArg,
  utils::{PREPROD_MAGIC, PREVIEW_MAGIC},
};
use pallas::network::miniprotocols::MAINNET_MAGIC;

/// Simple description on how to connect to a local or remote node.
/// Used to build Oura source config.
pub enum NodeAddress {
  /// Path to Unix node.socket
  UnixSocket(String),
  // Hostname and port number for TCP connection to remote node
  TcpAddress(String, u64),
}

/// Typed network magic restricted to specific networks fully supported by Oura.
pub enum NetworkMagic {
  PREPROD,
  PREVIEW,
  MAINNET,
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
