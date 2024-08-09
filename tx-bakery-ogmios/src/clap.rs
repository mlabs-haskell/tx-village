use anyhow::anyhow;
use clap::Parser;
use tx_bakery::chain_query::Network;
use url::Url;

use crate::client::{OgmiosClientConfig, OgmiosClientConfigBuilder};

#[derive(Debug, Clone, Parser)]
pub struct OgmiosOpts {
    /// URL of the Ogmios service
    #[arg(long, value_name = "URL", default_value = "http://127.0.0.1:1337")]
    pub ogmios_url: Url,

    /// Cardano network type (mainnet | testnet|)
    #[arg(long, value_name = "NETWORK", default_value = "testnet")]
    pub network: Network,
}

impl TryFrom<OgmiosOpts> for OgmiosClientConfig {
    type Error = anyhow::Error;
    fn try_from(opts: OgmiosOpts) -> Result<OgmiosClientConfig, anyhow::Error> {
        OgmiosClientConfigBuilder::default()
            .url(opts.ogmios_url)
            .network(opts.network)
            .build()
            .map_err(|err| anyhow!("Couldn't build OgmiosClientConfig: {}", err))
    }
}
