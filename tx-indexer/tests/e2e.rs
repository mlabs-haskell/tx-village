#[cfg(test)]
mod e2e_tests {
    use num_bigint::BigInt;
    use plutus_ledger_api::{
        json::Json,
        plutus_data::IsPlutusData,
        v2::{
            crypto::LedgerBytes,
            datum::OutputDatum,
            redeemer::Redeemer,
            transaction::{TransactionHash, TransactionInfo, TransactionOutput, TxInInfo},
            value::{AssetClass, CurrencySymbol, TokenName, Value},
        },
    };
    use serial_test::serial;
    use std::{collections::BTreeMap, sync::mpsc};
    use tracing::Level;
    use tx_bakery::{
        chain_query::ChainQuery,
        submitter::Submitter,
        tx_info_builder::TxScaffold,
        utils::{
            ogmios::{
                client::OgmiosClient,
                launcher::{OgmiosLauncher, OgmiosLauncherConfigBuilder},
            },
            plutip::{Plutip, PlutipConfigBuilder},
            script::ScriptOrRef,
        },
        wallet::Wallet,
        ChangeStrategy, CollateralStrategy, TxBakery, TxWithCtx,
    };
    use tx_indexer::{
        config::{NetworkConfig, NetworkName, NodeAddress, TxIndexerConfig},
        error::{ErrorPolicy, ErrorPolicyProvider},
        filter::Filter,
        handler::{
            callback::EventHandler,
            chain_event::{ChainEvent, TransactionEventRecord},
        },
        TxIndexer,
    };

    #[tokio::test]
    #[serial]
    async fn e2e_mint() -> std::result::Result<(), oura::Error> {
        // Set up tracing logger (logs to stdout).
        let collector = tracing_subscriber::fmt()
            .with_max_level(Level::ERROR)
            // build but do not install the subscriber.
            .finish();
        tracing::subscriber::set_global_default(collector)?;

        let verbose = false;
        let plutip_config = PlutipConfigBuilder::default()
            .verbose(verbose)
            .build()
            .unwrap();
        let plutip = Plutip::start(plutip_config).await.unwrap();

        let ogmios_launcher_config = OgmiosLauncherConfigBuilder::default()
            .node_socket(plutip.get_node_socket())
            .node_config(plutip.get_node_config_path().await)
            .network(plutip.get_network())
            .verbose(verbose)
            .build()
            .unwrap();
        let ogmios_launcher = OgmiosLauncher::start(ogmios_launcher_config).await.unwrap();
        let ogmios = ogmios_launcher.connect().await.unwrap();

        let (observer_sender, observer_receiver) = mpsc::channel();
        TxIndexer::run(TxIndexerConfig::new(
            ObserveHandler { observer_sender },
            NodeAddress::UnixSocket(
                plutip
                    .get_node_socket()
                    .into_os_string()
                    .into_string()
                    .unwrap(),
            ),
            NetworkConfig::WellKnown(NetworkName::MAINNET),
            None,
            4,
            Filter {
                curr_symbols: vec![Json::from_json_string(
                    "\"ec56549aaed71fba1ba7174672831cc20aac44c4a3b4607c38bed7f3\"",
                )
                .unwrap()],
            },
            Default::default(),
        ))
        .await
        .expect("Failed to spawn indexer");

        let (tx_hash, tx_info) = test_mint(&plutip, &ogmios).await;

        let tx_rec = observer_receiver.recv().unwrap();
        assert_eq_tx(tx_hash, tx_info, tx_rec);

        Ok(())
    }

    fn assert_eq_tx(
        expected_hash: TransactionHash,
        expected_info: TransactionInfo,
        actual_tx: TransactionEventRecord,
    ) {
        assert_eq!(expected_hash, actual_tx.hash);
        // In the case of inputs, we don't want any extra actual inputs. Only the expected ones.
        assert_eq_vec_unsorted(
            &actual_tx.inputs,
            &expected_info
                .inputs
                .into_iter()
                .map(|x| x.reference)
                .collect::<Vec<_>>(),
        );
        // In the case of outputs, there may be extra change outputs.
        // Therefore, left: expected, right: actual
        assert_eq_vec_loose(
            &expected_info.outputs,
            // We need to ignore ada since it doesn't appear in expected tx info but
            // does in the actual transaction.
            &actual_tx
                .outputs
                .iter()
                .map(|TxInInfo { output, .. }| {
                    let mut output = output.clone();
                    output.value = output
                        .value
                        .filter(|cs, _, _| *cs != CurrencySymbol::Ada)
                        .normalize();

                    output
                })
                .collect(),
        );
        assert_eq!(expected_info.mint, actual_tx.mint);
    }

    // Ensures two vectors are equal regardless of order of elements. (i.e, they contain same elements)
    fn assert_eq_vec_unsorted<T: Eq + std::fmt::Debug>(left: &Vec<T>, right: &Vec<T>) {
        assert_eq!(left.len(), right.len());
        assert_eq_vec_loose(left, right);
    }

    // Loose equality. Ensures that all elements in `left` appear in `right`
    // This means that `right` may be a superset of `left`.
    fn assert_eq_vec_loose<T: Eq + std::fmt::Debug>(left: &Vec<T>, right: &Vec<T>) {
        /* TODO(chase): This could be more efficient with sets but not all required traits are implemented
        across the board. */
        for x in left {
            assert!(
                right.iter().any(|y| x == y),
                "Could not find any matches for {:?} in {:?}",
                x,
                right
            );
        }
    }

    async fn test_mint(
        plutip: &Plutip,
        ogmios: &OgmiosClient,
    ) -> (TransactionHash, TransactionInfo) {
        let mp: Vec<u8> = Json::from_json_string("\"WD8BAAAyIlMwA0kBBFtFUV0AFTM1c0ZuHN1oASQUgmKTCpmAGkkWW0VRXSBWYWxpZGF0aW9uIGZhaWxlZAAWVzk=\"").unwrap();
        let minting_policy = ScriptOrRef::from_bytes(mp).unwrap().as_minting_policy();

        let wallet = plutip.get_own_wallet().await.unwrap();
        let own_utxos = ogmios
            .query_utxos_by_addr(&wallet.get_change_addr())
            .await
            .unwrap();

        let cur_sym = CurrencySymbol::NativeToken(minting_policy.0.clone());
        let token_name = TokenName(LedgerBytes(vec![1, 2]));

        let fee_input = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let mint_asset = AssetClass {
            currency_symbol: cur_sym.clone(),
            token_name: token_name.clone(),
        };

        let tx_info = TxScaffold::new()
            .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
            .add_mint(mint_asset, 1, Redeemer(BigInt::from(1234).to_plutus_data()))
            .add_output(TransactionOutput {
                address: wallet.get_change_addr(),
                value: Value::new().insert_token(&cur_sym, &token_name, &BigInt::from(1)),
                datum: OutputDatum::None,
                reference_script: None,
            })
            .build();

        let scripts = BTreeMap::from([minting_policy.1.clone().with_script_hash()]);

        let collateral_utxo = own_utxos
            .iter()
            .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
            .expect("Cannot find spendable input UTxO.");

        let collateral = CollateralStrategy::Explicit(TxInInfo {
            reference: collateral_utxo.0.clone(),
            output: collateral_utxo.1.into(),
        });

        let tx_bakery = TxBakery::init(ogmios).await.unwrap();

        let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());
        let tx = TxWithCtx::new(&tx_info, &scripts, &collateral, &change_strategy);

        let tx_hash = tx_bakery
            .bake_and_deliver(ogmios, &wallet, tx)
            .await
            .unwrap();

        ogmios.await_tx_confirm(&tx_hash).await.unwrap();

        (tx_hash, tx_info)
    }

    #[derive(thiserror::Error, Debug)]
    pub(crate) enum ObserveHandlerError {}

    impl ErrorPolicyProvider for ObserveHandlerError {
        fn get_error_policy(&self) -> ErrorPolicy<Self> {
            ErrorPolicy::Skip
        }
    }

    #[derive(Clone)]
    struct ObserveHandler {
        observer_sender: mpsc::Sender<TransactionEventRecord>,
    }

    impl EventHandler for ObserveHandler {
        type Error = ObserveHandlerError;

        async fn handle(&self, ev: ChainEvent) -> Result<(), Self::Error> {
            if let ChainEvent::TransactionEvent { transaction, .. } = ev {
                self.observer_sender.send(transaction).unwrap();
                Ok(())
            } else {
                Ok(())
            }
        }
    }
}
