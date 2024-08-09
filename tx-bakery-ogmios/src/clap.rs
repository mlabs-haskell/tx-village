use clap::Parser;
use tx_bakery::chain_query::Network;
use url::Url;

#[derive(Debug, Clone, Parser)]
struct OgmiosOpts {
    /// URL of the Ogmios service
    #[arg(long, value_name = "URL", default_value = "http://127.0.0.1:1337")]
    pub ogmios_url: Url,

    /// Cardano network type (mainnet | testnet|)
    #[arg(long, value_name = "NETWORK", default_value = "testnet")]
    pub network: Network,
}
