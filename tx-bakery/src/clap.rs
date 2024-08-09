use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Clone, Parser)]
pub struct KeyWalletOpts {
    /// Payment signing key
    #[arg(long, value_name = "FILE")]
    pub signing_key_file: PathBuf,

    /// Optional staking signing key
    #[arg(long, value_name = "FILE")]
    pub staking_signing_key_file: Option<PathBuf>,
}
