use crate::utxo_db::error::UtxoIndexerError;
use plutus_ledger_api::v2::transaction::{TransactionInput, TxInInfo};
use sqlx::{FromRow, PgConnection};
use strum_macros::Display;
use tracing::{event, span, Instrument, Level};
use tx_indexer::database::plutus as db;

#[derive(Debug, FromRow, PartialEq, Eq)]
pub struct UtxosTable {
    pub utxo_ref: db::TransactionInput,
    pub value: db::Value,
    pub address: db::Address,
    pub datum: db::OutputDatum,

    pub created_at: db::Slot,
    pub deleted_at: Option<db::Slot>,
}

#[derive(Debug)]
pub struct RollbackResult<T>
where
    T: std::fmt::Debug,
{
    pub recovered: Vec<T>,
    pub deleted: Vec<T>,
}

impl UtxosTable {
    pub fn new(utxo: TxInInfo, created_at: u64) -> Result<Self, db::DBTypeConversionError> {
        Ok(Self {
            utxo_ref: utxo.reference.try_into()?,
            value: utxo.output.value.try_into()?,
            address: utxo.output.address.try_into()?,
            datum: utxo.output.datum.try_into()?,

            created_at: created_at.into(),
            deleted_at: None,
        })
    }

    pub async fn store(self, conn: &mut PgConnection) -> Result<(), UtxoIndexerError> {
        let utxo_ref = TransactionInput::from(self.utxo_ref.clone());
        let span = span!(Level::INFO, "StoringUTxO", ?utxo_ref);
        async move {
            sqlx::query(
                r#"
                INSERT INTO utxos (utxo_ref, value, address, datum, created_at)
                VALUES ($1, $2, $3, $4, $5)
                "#,
            )
            .bind(self.utxo_ref)
            .bind(self.value)
            .bind(self.address)
            .bind(self.datum)
            .bind(self.created_at)
            .execute(conn)
            .await
            .map_err(|err| {
                event!(Level::ERROR, label=%Event::SqlxError, ?err);
                UtxoIndexerError::DbError(err)
            })?;

            Ok(())
        }
        .instrument(span)
        .await
    }

    pub async fn rollback_after_block(
        conn: &mut PgConnection,
        transaction_block: u64,
    ) -> Result<RollbackResult<UtxosTable>, UtxoIndexerError> {
        let span = span!(Level::INFO, "Rollback", %transaction_block);
        async move {
            let deleted = sqlx::query_as::<_, Self>(
                r#"
                SELECT *
                FROM utxos
                WHERE created_at > $1
                "#,
            )
            .bind(db::Slot::from(transaction_block))
            .fetch_all(&mut *conn)
            .await
            .map_err(|err| {
                event!(Level::ERROR, label=%Event::SqlxError, ?err);
                UtxoIndexerError::DbError(err)
            })?;

            let recovered = sqlx::query_as::<_, Self>(
                r#"
                SELECT *
                FROM utxos
                WHERE deleted_at > $1
                "#,
            )
            .bind(db::Slot::from(transaction_block))
            .fetch_all(&mut *conn)
            .await
            .map_err(|err| {
                event!(Level::ERROR, label=%Event::SqlxError, ?err);
                UtxoIndexerError::DbError(err)
            })?;

            sqlx::query(
                r#"
                UPDATE utxos
                SET deleted_at = NULL
                WHERE deleted_at > $1;
                "#,
            )
            .bind(db::Slot::from(transaction_block))
            .execute(&mut *conn)
            .await
            .map_err(|err| {
                event!(Level::ERROR, label=%Event::SqlxError, ?err);
                UtxoIndexerError::DbError(err)
            })?;

            sqlx::query(
                r#"
                DELETE FROM utxos
                WHERE created_at > $1;
                "#,
            )
            .bind(db::Slot::from(transaction_block))
            .execute(&mut *conn)
            .await
            .map_err(|err| {
                event!(Level::ERROR, label=%Event::SqlxError, ?err);
                UtxoIndexerError::DbError(err)
            })?;

            Ok(RollbackResult { deleted, recovered })
        }
        .instrument(span)
        .await
    }
}

#[derive(Display)]
enum Event {
    SqlxError,
}
