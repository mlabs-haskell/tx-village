use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Clone, Parser)]
struct KeyWalletOpts {
    /// Payment signing key
    #[arg(long, value_name = "FILE")]
    signing_key_file: PathBuf,

    /// Optional staking signing key
    #[arg(long, value_name = "FILE")]
    staking_signing_key_file: Option<PathBuf>,
}
