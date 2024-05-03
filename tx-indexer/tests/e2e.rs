#[cfg(test)]
mod e2e_tests {
    use std::{collections::BTreeMap, sync::mpsc};

    use num_bigint::BigInt;
    use oura::model::{Event, EventData, TransactionRecord};
    use plutus_ledger_api::{
        json::Json,
        plutus_data::IsPlutusData,
        v2::{
            crypto::LedgerBytes,
            datum::OutputDatum,
            redeemer::Redeemer,
            transaction::{TransactionOutput, TxInInfo},
            value::{AssetClass, CurrencySymbol, TokenName, Value},
        },
    };
    use sqlx::PgConnection;
    use tracing::Level;
    use tx_bakery::{
        chain_query::ChainQuery,
        submitter::Submitter,
        tx_info_builder::TxScaffold,
        utils::{
            ogmios::{Ogmios, OgmiosConfigBuilder},
            plutip::{Plutip, PlutipConfigBuilder},
            script::ScriptOrRef,
        },
        wallet::Wallet,
        ChangeStrategy, CollateralStrategy, TxBakery, TxWithCtx,
    };
    use tx_indexer::indexer::{
        callback::Handler,
        config::IndexerConfig,
        error::{ErrorPolicy, ErrorPolicyProvider},
        filter::Filter,
        run_indexer,
        types::{NetworkMagic, NodeAddress},
    };

    #[tokio::test]
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
        let plutip = Plutip::start(&plutip_config).await.unwrap();

        let ogmios_config = OgmiosConfigBuilder::default()
            .node_socket(plutip.get_node_socket())
            .node_config(plutip.get_node_config_path().await)
            .network(plutip.get_network())
            .verbose(verbose)
            .build()
            .unwrap();
        let ogmios = Ogmios::start(&ogmios_config).await.unwrap();

        let (observer_channel, rx) = mpsc::channel();
        run_indexer(IndexerConfig::new(
            ObserveHandler { observer_channel },
            NodeAddress::UnixSocket(plutip.get_node_socket()),
            NetworkMagic::MAINNET,
            None,
            4,
            Some(Filter {
                curr_symbols: vec![Json::from_json_string(
                    "\"ec56549aaed71fba1ba7174672831cc20aac44c4a3b4607c38bed7f3\"",
                )
                .unwrap()],
            }),
            Default::default(),
            "postgresql://postgres@localhost:5555/txvillage".to_string(),
        ))
        .await
        .expect("Failed to spawn indexer");

        test_mint(&plutip, &ogmios).await;

        let tx_record = rx.recv().unwrap();
        println!("{:?}", tx_record);

        Ok(())
    }

    async fn test_mint(plutip: &Plutip, ogmios: &Ogmios) {
        let mp: std::vec::Vec<u8> = Json::from_json_string("\"WD8BAAAyIlMwA0kBBFtFUV0AFTM1c0ZuHN1oASQUgmKTCpmAGkkWW0VRXSBWYWxpZGF0aW9uIGZhaWxlZAAWVzk=\"").unwrap();
        let minting_policy =
            ScriptOrRef::from_bytes(mp, tx_bakery::utils::script::PlutusVersion::V2)
                .unwrap()
                .as_minting_policy();

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

        let validators = BTreeMap::new();
        let minting_policies = BTreeMap::from([minting_policy.clone()]);

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
        let tx = TxWithCtx::new(
            &tx_info,
            &validators,
            &minting_policies,
            &collateral,
            &change_strategy,
        );

        let tx_hash = tx_bakery
            .bake_and_deliver(ogmios, &wallet, tx)
            .await
            .unwrap();

        ogmios.await_tx_confirm(&tx_hash).await.unwrap();
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
        observer_channel: mpsc::Sender<TransactionRecord>,
    }

    impl Handler for ObserveHandler {
        type Error = ObserveHandlerError;

        async fn handle<'a>(
            &self,
            ev: Event,
            _pg_connection: &'a mut PgConnection,
        ) -> Result<(), Self::Error> {
            match ev.data {
                EventData::Transaction(dat) => self.observer_channel.send(dat).unwrap(),
                _ => (),
            };
            Ok(())
        }
    }
}
