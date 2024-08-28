#[cfg(test)]
mod plutus_database_roundtrips {
    use anyhow::{anyhow, Context, Result};
    use num_bigint::BigInt;
    use pla::goldens::{
        v1::{
            sample_address, sample_currency_symbol, sample_datum_hash, sample_ed25519_pub_key_hash,
            sample_staking_credential, sample_token_name, sample_transaction_hash,
            sample_transaction_input, sample_value,
        },
        v2::{sample_output_datum, sample_transaction_output},
    };
    use plutus_ledger_api as pla;
    use sqlx::{FromRow, PgConnection, PgPool};
    use tx_indexer::database::plutus::*;

    #[tokio::test]
    async fn db_tests() {
        let pg_pool = PgPool::connect("postgres://tx_indexer@127.0.0.1:5555")
            .await
            .unwrap();
        let mut conn = pg_pool.acquire().await.unwrap();
        clean(&mut conn).await.unwrap();

        currency_symbol(&mut conn)
            .await
            .with_context(|| "CurrencySymbol")
            .unwrap();
        token_name(&mut conn)
            .await
            .with_context(|| "TokenName")
            .unwrap();
        tx_hash(&mut conn)
            .await
            .with_context(|| "TransactionHash")
            .unwrap();
        pub_key_hash(&mut conn)
            .await
            .with_context(|| "Ed25519PubKeyHash")
            .unwrap();
        script_hash(&mut conn)
            .await
            .with_context(|| "ScriptHash")
            .unwrap();
        datum_hash(&mut conn)
            .await
            .with_context(|| "DatumHash")
            .unwrap();
        slot(&mut conn).await.with_context(|| "Slot").unwrap();
        plutus_data(&mut conn)
            .await
            .with_context(|| "PlutusData")
            .unwrap();
        cred(&mut conn).await.with_context(|| "Credential").unwrap();
        chain_pointer(&mut conn)
            .await
            .with_context(|| "ChainPointer")
            .unwrap();
        staking_cred(&mut conn)
            .await
            .with_context(|| "StakingCredential")
            .unwrap();
        address(&mut conn).await.with_context(|| "Address").unwrap();
        asset_quantity(&mut conn)
            .await
            .with_context(|| "AssetQuantity")
            .unwrap();
        value(&mut conn).await.with_context(|| "Value").unwrap();
        tx_in(&mut conn)
            .await
            .with_context(|| "TransactionInput")
            .unwrap();
        datum(&mut conn)
            .await
            .with_context(|| "OutputDatum")
            .unwrap();
        tx_out(&mut conn)
            .await
            .with_context(|| "TransactionOutput")
            .unwrap();
    }

    #[derive(Debug, Clone, FromRow, PartialEq, Eq, Default)]
    pub struct TestDB {
        cur_sym: Option<CurrencySymbol>,
        token_name: Option<TokenName>,
        tx_hash: Option<TransactionHash>,
        pub_key_hash: Option<Ed25519PubKeyHash>,
        script_hash: Option<ScriptHash>,
        datum_hash: Option<DatumHash>,
        slot: Option<Slot>,
        plutus_data: Option<PlutusData>,
        cred: Option<Credential>,
        chain_pointer: Option<ChainPointer>,
        staking_cred: Option<StakingCredential>,
        address: Option<Address>,
        asset_quantity: Option<AssetQuantity>,
        value: Option<Value>,
        tx_in: Option<TransactionInput>,
        datum: Option<OutputDatum>,
        tx_out: Option<TransactionOutput>,
        tx_in_info: Option<TxInInfo>,
    }

    async fn write(test: TestDB, conn: &mut PgConnection) -> Result<()> {
        sqlx::query("INSERT INTO testdb (
              cur_sym,
              token_name,
              tx_hash,
              pub_key_hash,
              script_hash,
              datum_hash,
              slot,
              plutus_data,
              cred,
              chain_pointer,
              staking_cred,
              address,
              asset_quantity,
              value,
              tx_in,
              datum,
              tx_out,
              tx_in_info) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18)")
              .bind(test.cur_sym)
              .bind(test.token_name)
              .bind(test.tx_hash)
              .bind(test.pub_key_hash)
              .bind(test.script_hash)
              .bind(test.datum_hash)
              .bind(test.slot)
              .bind(test.plutus_data)
              .bind(test.cred)
              .bind(test.chain_pointer)
              .bind(test.staking_cred)
              .bind(test.address)
              .bind(test.asset_quantity)
              .bind(test.value)
              .bind(test.tx_in)
              .bind(test.datum)
              .bind(test.tx_out)
              .bind(test.tx_in_info)
            .execute(conn)
            .await?;
        Ok(())
    }

    async fn read(conn: &mut PgConnection) -> Result<TestDB> {
        sqlx::query_as("SELECT * FROM testdb")
            .fetch_one(conn)
            .await
            .map_err(|err| anyhow!(err))
    }

    async fn clean(conn: &mut PgConnection) -> Result<()> {
        sqlx::query("TRUNCATE testdb;").execute(conn).await?;
        Ok(())
    }

    async fn write_read_assert(testdb: TestDB, conn: &mut PgConnection) -> Result<()> {
        write(testdb.clone(), &mut *conn).await?;
        assert_eq!(testdb, read(&mut *conn).await?);
        clean(&mut *conn).await
    }

    async fn currency_symbol(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            cur_sym: Some(sample_currency_symbol().into()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn token_name(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            token_name: Some(sample_token_name().into()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn tx_hash(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            tx_hash: Some(sample_transaction_hash().into()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn pub_key_hash(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            pub_key_hash: Some(sample_ed25519_pub_key_hash().into()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn script_hash(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            script_hash: Some(sample_script_hash().into()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn datum_hash(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            datum_hash: Some(sample_datum_hash().into()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn slot(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            slot: Some(854321.into()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn plutus_data(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            plutus_data: Some(sample_plutus_data().try_into().unwrap()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn cred(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            cred: Some(sample_credential().into()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn chain_pointer(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            chain_pointer: Some(sample_chain_pointer().try_into().unwrap()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn staking_cred(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            staking_cred: Some(sample_staking_credential().try_into().unwrap()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn address(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            address: Some(sample_address().try_into().unwrap()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn asset_quantity(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            asset_quantity: Some(
                (
                    sample_currency_symbol(),
                    sample_token_name(),
                    BigInt::from(15470),
                )
                    .try_into()
                    .unwrap(),
            ),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn value(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            value: Some(sample_value().try_into().unwrap()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn tx_in(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            tx_in: Some(sample_transaction_input().try_into().unwrap()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn datum(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            datum: Some(sample_output_datum().try_into().unwrap()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    async fn tx_out(conn: &mut PgConnection) -> Result<()> {
        let testdb = TestDB {
            tx_out: Some(sample_transaction_output().try_into().unwrap()),
            ..Default::default()
        };

        write_read_assert(testdb.clone(), &mut *conn).await
    }

    fn sample_plutus_data() -> pla::plutus_data::PlutusData {
        pla::plutus_data::PlutusData::constr(
            1,
            vec![pla::plutus_data::PlutusData::bytes(
                "Something".as_bytes().to_vec(),
            )],
        )
    }

    fn sample_script_hash() -> pla::v2::script::ScriptHash {
        pla::v2::script::ScriptHash(pla::v2::crypto::LedgerBytes([1].repeat(28).to_vec()))
    }

    fn sample_credential() -> pla::v2::address::Credential {
        pla::v2::address::Credential::Script(pla::v2::script::ValidatorHash(sample_script_hash()))
    }

    fn sample_chain_pointer() -> pla::v2::address::ChainPointer {
        pla::v2::address::ChainPointer {
            slot_number: pla::v2::address::Slot(134561.into()),
            transaction_index: pla::v2::address::TransactionIndex(4.into()),
            certificate_index: pla::v2::address::CertificateIndex(10.into()),
        }
    }
}
