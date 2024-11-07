#[cfg(test)]
mod fixture_tests {
    use std::{path::PathBuf, str::FromStr};

    use diesel::{
        prelude::*,
        r2d2::{ConnectionManager, Pool},
        PgConnection,
    };
    use plutus_ledger_api::v3::{address::Address, transaction::TransactionInput};
    use serial_test::serial;
    use tracing::Level;
    use tx_indexer::database::plutus as db;
    use tx_indexer::{config::TxIndexerConfig, TxIndexer};
    use tx_indexer_testsuite::schema::utxos::dsl::*;
    use tx_indexer_testsuite::utxo_db::{handler::UtxoIndexerHandler, table::utxos::UtxosTable};

    #[tokio::test]
    #[serial]
    async fn fixture_replay() -> std::result::Result<(), oura::Error> {
        // Set up tracing logger (logs to stdout).
        let collector = tracing_subscriber::fmt()
            .with_max_level(Level::ERROR)
            // build but do not install the subscriber.
            .finish();
        tracing::subscriber::set_global_default(collector)?;

        let manager =
            ConnectionManager::<PgConnection>::new("postgres://tx_indexer@127.0.0.1:5555");

        let pg_pool = Pool::builder()
            .test_on_check_out(true)
            .build(manager)
            .expect("Could not build connection pool");

        let mut conn = pg_pool.get().unwrap();
        diesel::delete(utxos).execute(&mut conn)?;

        let handler = UtxoIndexerHandler::postgres(pg_pool);

        TxIndexer::run(TxIndexerConfig::source_from_fixtures(
            handler,
            PathBuf::from("tests/fixtures"),
            Default::default(),
        ))
        .await
        .expect("Failed to spawn indexer")
        .join()
        .unwrap();

        let tx_ref = TransactionInput::from_str(
            "e819ce5140d3ec3e4d00d163d49a1de0625b410a3aaab149000eaf7a1aca6d4a#0",
        )
        .unwrap();

        let utxo = utxos
            .find(db::TransactionInput::try_from(tx_ref).unwrap())
            .select(UtxosTable::as_select())
            .first(&mut conn)?;

        assert_eq!(
            Address::try_from(utxo.address)
                .unwrap()
                .with_extra_info(1)
                .to_string(),
            "addr1wy5yehcpw4e3r32rltrww40e6ezdckr9v9l0ehptsxeynlgpemay4"
        );

        Ok(())
    }
}
