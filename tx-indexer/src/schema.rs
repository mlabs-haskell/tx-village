// @generated automatically by Diesel CLI.

pub mod sql_types {
    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "address", schema = "plutus"))]
    pub struct Address;

    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "asset_quantity", schema = "plutus"))]
    pub struct AssetQuantity;

    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "chain_pointer", schema = "plutus"))]
    pub struct ChainPointer;

    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "credential", schema = "plutus"))]
    pub struct Credential;

    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "hash28", schema = "plutus"))]
    pub struct Hash28;

    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "hash32", schema = "plutus"))]
    pub struct Hash32;

    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "output_datum", schema = "plutus"))]
    pub struct OutputDatum;

    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "staking_credential", schema = "plutus"))]
    pub struct StakingCredential;

    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "transaction_input", schema = "plutus"))]
    pub struct TransactionInput;

    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "transaction_output", schema = "plutus"))]
    pub struct TransactionOutput;

    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "tx_in_info", schema = "plutus"))]
    pub struct TxInInfo;
}

diesel::table! {
    use crate::database::plutus::*;

    sync_progress (processed) {
        block_slot -> Int8,
        block_hash -> Bytea,
        processed -> Bool,
    }
}

diesel::table! {
    use crate::database::plutus::*;
    use super::sql_types::Hash32;
    use super::sql_types::Hash28;
    use super::sql_types::Credential;
    use super::sql_types::ChainPointer;
    use super::sql_types::StakingCredential;
    use super::sql_types::Address;
    use super::sql_types::AssetQuantity;
    use super::sql_types::TransactionInput;
    use super::sql_types::OutputDatum;
    use super::sql_types::TransactionOutput;
    use super::sql_types::TxInInfo;

    testdb (id) {
        id -> Int8,
        cur_sym -> Nullable<Bytea>,
        token_name -> Nullable<Bytea>,
        tx_hash -> Nullable<Hash32>,
        pub_key_hash -> Nullable<Hash28>,
        script_hash -> Nullable<Hash28>,
        datum_hash -> Nullable<Hash32>,
        slot -> Nullable<Int8>,
        plutus_data -> Nullable<Jsonb>,
        cred -> Nullable<Credential>,
        chain_pointer -> Nullable<ChainPointer>,
        staking_cred -> Nullable<StakingCredential>,
        address -> Nullable<Address>,
        asset_quantity -> Nullable<AssetQuantity>,
        value -> Nullable<Array<Nullable<AssetQuantity>>>,
        tx_in -> Nullable<TransactionInput>,
        datum -> Nullable<OutputDatum>,
        tx_out -> Nullable<TransactionOutput>,
        tx_in_info -> Nullable<TxInInfo>,
    }
}

diesel::table! {
    use crate::database::plutus::*;
    use super::sql_types::TransactionInput;
    use super::sql_types::AssetQuantity;
    use super::sql_types::Address;
    use super::sql_types::OutputDatum;

    utxos (utxo_ref) {
        utxo_ref -> TransactionInput,
        value -> Array<Nullable<AssetQuantity>>,
        address -> Address,
        datum -> OutputDatum,
        created_at -> Int8,
        deleted_at -> Nullable<Int8>,
    }
}

diesel::allow_tables_to_appear_in_same_query!(
    sync_progress,
    testdb,
    utxos,
);
