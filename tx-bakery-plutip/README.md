# Transaction Bakery - Plutip client

This is a client for the Plutip, a Cardano local cluster used for testing of
Cardano dApps.

## How to use

In the example below, we're using Plutip to spin up a fresh Cardano testnet,
connect Ogmios to it, and run an example transaction (the details of this
transaction are removed, the full example can be found under
[extras/tx-bakery-testsuite/tests/tests/main.rs](../extras/tx-bakery-testsuite/tests/tests/main.rs).

```rust
#[tokio::test]
#[serial]
async fn test_is_eq_validator() -> Result<()> {
    let verbose = false;
    // Launching Plutip with the default configuration
    // Once the handler goes out of scope, the local cluster will be killed
    let plutip_config = PlutipConfigBuilder::default()
        .verbose(verbose)
        .build()
        .unwrap();
    let plutip = Plutip::start(plutip_config).await.unwrap();

    // Launching Ogmios and connecting it to a node from the Plutip local cluster
    // Once the handler goes out of scope, Ogmios will be killed
    let ogmios_config = OgmiosLauncherConfigBuilder::default()
        .node_socket(plutip.get_node_socket())
        .node_config(plutip.get_node_config_path().await)
        .network(plutip.get_network())
        .host("127.0.0.1".into())
        .port(1337)
        .verbose(verbose)
        .build()
        .unwrap();
    let ogmios_launcher = OgmiosLauncher::start(ogmios_config).await.unwrap();

    // Connecting the Ogmios Client to the running Ogmios instance
    let ogmios_client_config = OgmiosClientConfigBuilder::default()
        .network(plutip.get_network())
        .url(Url::parse("http://127.0.0.1:1337").unwrap())
        .build()
        .unwrap();
    let ogmios_client = OgmiosClient::connect(ogmios_client_config).await.unwrap();

    // Get a funded wallet
    let wallet = plutip.get_own_wallet().await.unwrap();

    // Run transaction using the wallet and Ogmios
    let tx_hash_lock_a = lock_eq_datum::build_and_submit(
        &wallet,
        &ogmios,
        &ogmios,
        eq_validator.clone(),
        &example_eq_datum_a,
    )
    .await?;

    ogmios.await_tx_confirm(&tx_hash_lock_a).await?;
};
```
