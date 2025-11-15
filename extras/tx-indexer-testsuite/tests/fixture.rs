#[cfg(test)]
mod fixture_tests {
    use std::{path::PathBuf, str::FromStr};

    use anyhow::Context;
    use diesel::{
        prelude::*,
        r2d2::{ConnectionManager, Pool},
        PgConnection,
    };
    use plutus_ledger_api::v3::transaction::TransactionInput;
    use serial_test::serial;
    use tracing::Level;
    use tx_indexer::database::plutus::db_types as db;
    use tx_indexer::{config::TxIndexerConfig, TxIndexer};
    use tx_indexer_testsuite::schema::utxos::dsl::*;
    use tx_indexer_testsuite::utxo_db::{handler::UtxoIndexerHandler, table::utxos::UtxosTable};

    #[tokio::test]
    #[serial]
    async fn fixture_replay() -> anyhow::Result<()> {
        // Set up tracing logger (logs to stdout).
        let collector = tracing_subscriber::fmt()
            .with_max_level(Level::ERROR)
            // build but do not install the subscriber.
            .finish();
        tracing::subscriber::set_global_default(collector)?;

        let manager =
            ConnectionManager::<PgConnection>::new("postgres://127.0.0.1:5555/tx_indexer");

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
        ))
        .await
        .context("Failed to spawn indexer")?;

        let tx_ref = TransactionInput::from_str(
            "a8fa4293645facb2a0332f4dfc442dff3fc9ca021c95ee908df5d9605e3825be#0",
        )
        .unwrap();

        let utxo = utxos
            .find(db::TransactionInput::try_from(tx_ref).context("couldn't convert utxo ref")?)
            .select(UtxosTable::as_select())
            .first(&mut conn)?;

        assert_eq!(
            utxo.address,
            "addr_test1vp8cprhse9pnnv7f4l3n6pj0afq2hjm6f7r2205dz0583egagfjah"
        );

        Ok(())
    }
}
