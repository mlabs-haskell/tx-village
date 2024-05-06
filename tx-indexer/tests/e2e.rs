#[cfg(test)]
mod e2e_tests {
    use std::{collections::BTreeMap, str::FromStr, sync::mpsc};

    use cardano_serialization_lib as csl;
    use num_bigint::BigInt;
    use num_traits::FromPrimitive;
    use oura::model::{
        Event, EventData, MintRecord, OutputAssetRecord, PlutusDatumRecord, TransactionRecord,
        TxInputRecord, TxOutputRecord,
    };
    use plutus_ledger_api::{
        json::Json,
        plutus_data::IsPlutusData,
        v2::{
            address::Address,
            crypto::LedgerBytes,
            datum::{Datum, DatumHash, OutputDatum},
            redeemer::Redeemer,
            transaction::{TransactionHash, TransactionInfo, TransactionOutput, TxInInfo},
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
            csl_to_pla::{FromCSL, TryToPLA},
            ogmios::{Ogmios, OgmiosConfigBuilder},
            pla_to_csl::TryToCSLWithDef,
            plutip::{Plutip, PlutipConfigBuilder},
            script::ScriptOrRef,
        },
        wallet::Wallet,
        ChangeStrategy, CollateralStrategy, TxBakery, TxWithCtx,
    };
    use tx_indexer::{
        aux::ParseCurrencySymbol,
        indexer::{
            callback::Handler,
            config::IndexerConfig,
            error::{ErrorPolicy, ErrorPolicyProvider},
            filter::Filter,
            run_indexer,
            types::{NetworkName, NodeAddress},
        },
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
            NodeAddress::UnixSocket(
                plutip
                    .get_node_socket()
                    .into_os_string()
                    .into_string()
                    .unwrap(),
            ),
            NetworkName::MAINNET,
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

        let (tx_hash, tx_info) = test_mint(&plutip, &ogmios).await;

        let tx_rec = rx.recv().unwrap();
        assert_eq_tx(tx_hash, tx_info, tx_rec);

        Ok(())
    }

    fn assert_eq_tx(
        expected_hash: TransactionHash,
        expected_info: TransactionInfo,
        actual_tx: TransactionRecord,
    ) {
        assert!(expected_hash.oura_eq(&actual_tx.hash));
        assert!(expected_info.inputs.oura_eq(&actual_tx.inputs.unwrap()));
        assert!(expected_info.outputs.oura_eq(&actual_tx.outputs.unwrap()));
        assert!(expected_info.mint.oura_eq(&actual_tx.mint.unwrap()));
    }

    trait OuraEq<O> {
        fn oura_eq(&self, actual: &O) -> bool;
    }

    // Loose equality. Does not ensure order is the same in both vectors.
    impl<O, T: OuraEq<O>> OuraEq<Vec<O>> for Vec<T> {
        fn oura_eq(&self, actual: &Vec<O>) -> bool {
            /* TODO(chase): This could be more efficient with sets but not all required traits are implemented
            across the board. */
            self.len() == actual.len() && self.iter().all(|t| actual.iter().any(|o| t.oura_eq(o)))
        }
    }

    impl<O, T: OuraEq<O>> OuraEq<Option<O>> for Option<T> {
        fn oura_eq(&self, actual: &Option<O>) -> bool {
            match (self, actual) {
                (None, None) => true,
                (Some(x), Some(y)) => x.oura_eq(y),
                _ => false,
            }
        }
    }

    impl OuraEq<String> for TransactionHash {
        fn oura_eq(&self, actual: &String) -> bool {
            format!("{:?}", &self) == *actual
        }
    }

    impl OuraEq<u64> for BigInt {
        fn oura_eq(&self, actual: &u64) -> bool {
            BigInt::from_u64(*actual).map_or(false, |x| *self == x)
        }
    }

    impl OuraEq<TxInputRecord> for TxInInfo {
        fn oura_eq(&self, actual: &TxInputRecord) -> bool {
            self.reference.transaction_id.oura_eq(&actual.tx_id)
                && self.reference.index.oura_eq(&actual.index)
        }
    }

    impl OuraEq<String> for Address {
        fn oura_eq(&self, actual: &String) -> bool {
            csl::address::Address::from_bech32(&actual)
                .ok()
                .and_then(|actual_csl| actual_csl.try_to_pla().ok())
                .map_or(false, |actual_pla: Address| *self == actual_pla)
        }
    }

    impl OuraEq<String> for DatumHash {
        fn oura_eq(&self, actual: &String) -> bool {
            format!("{:?}", &self) == *actual
        }
    }

    impl OuraEq<PlutusDatumRecord> for Datum {
        fn oura_eq(&self, actual: &PlutusDatumRecord) -> bool {
            // TODO(chase): Could we use the Hash impl for PLA Datum instead?
            self.try_to_csl()
                .ok()
                .map_or(false, |x: csl::plutus::PlutusData| {
                    <DatumHash as FromCSL<csl::crypto::DataHash>>::from_csl(
                        &csl::utils::hash_plutus_data(&x),
                    )
                    .oura_eq(&actual.datum_hash)
                })
        }
    }

    impl OuraEq<Vec<OutputAssetRecord>> for Value {
        fn oura_eq(&self, actual: &Vec<OutputAssetRecord>) -> bool {
            actual
                .iter()
                .try_fold(Value::new(), |acc, x| {
                    let amt = BigInt::from_u64(x.amount)?;
                    let cs: ParseCurrencySymbol = FromStr::from_str(&x.policy).ok()?;
                    Some(acc.insert_token(&cs.0, &TokenName::from_string(&x.asset), &amt))
                })
                .map_or(false, |actual_value| {
                    self.clone().normalize() == actual_value.normalize()
                })
        }
    }

    impl OuraEq<Vec<MintRecord>> for Value {
        fn oura_eq(&self, actual: &Vec<MintRecord>) -> bool {
            actual
                .iter()
                .try_fold(Value::new(), |acc, x| {
                    let amt = BigInt::from_i64(x.quantity)?;
                    let cs: ParseCurrencySymbol = FromStr::from_str(&x.policy).ok()?;
                    Some(acc.insert_token(&cs.0, &TokenName::from_string(&x.asset), &amt))
                })
                .map_or(false, |actual_value| {
                    self.clone().normalize() == actual_value.normalize()
                })
        }
    }

    impl OuraEq<TxOutputRecord> for TransactionOutput {
        fn oura_eq(&self, actual: &TxOutputRecord) -> bool {
            let value_without_ada = self
                .value
                .clone()
                .filter(|cs, tk, _| *cs != CurrencySymbol::Ada && *tk != TokenName::ada());
            self.address.oura_eq(&actual.address)
                && self.value.get_ada_amount().oura_eq(&actual.amount)
                && OuraEq::<Vec<OutputAssetRecord>>::oura_eq(
                    &value_without_ada,
                    &actual.assets.as_ref().unwrap_or(&Vec::new()),
                )
                && match &self.datum {
                    OutputDatum::None => actual.datum_hash == None && actual.inline_datum == None,
                    OutputDatum::DatumHash(x) => actual
                        .datum_hash
                        .as_ref()
                        .map_or(false, |actual_hash| x.oura_eq(&actual_hash)),
                    OutputDatum::InlineDatum(x) => actual
                        .inline_datum
                        .as_ref()
                        .map_or(false, |actual_inline_datum| x.oura_eq(&actual_inline_datum)),
                }
        }
    }

    async fn test_mint(plutip: &Plutip, ogmios: &Ogmios) -> (TransactionHash, TransactionInfo) {
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
