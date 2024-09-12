diesel::table! {
    use crate::database::plutus::*;
    use diesel::sql_types::{Bytea, Bool, Int8};

    sync_progress (processed) {
        block_slot -> Int8,
        block_hash -> Bytea,
        processed -> Bool,
    }
}

diesel::table! {
    use crate::database::plutus::*;
    use diesel::sql_types::{Array, Nullable, Int8};

    utxos (utxo_ref) {
        utxo_ref -> PgTransactionInput,
        value -> PgValue,
        address -> PgAddress,
        datum -> PgOutputDatum,
        created_at -> PgSlot,
        deleted_at -> Nullable<PgSlot>,
    }
}

diesel::table! {
    use crate::database::plutus::*;
    use diesel::sql_types::{Array, Nullable, Int8};

    testdb (id) {
        id -> Int8,
        cur_sym -> Nullable<PgCurrencySymbol>,
        token_name -> Nullable<PgTokenName>,
        tx_hash -> Nullable<PgTransactionHash>,
        pub_key_hash -> Nullable<PgEd25519PubKeyHash>,
        script_hash -> Nullable<PgScriptHash>,
        datum_hash -> Nullable<PgDatumHash>,
        slot -> Nullable<PgSlot>,
        plutus_data -> Nullable<PgPlutusData>,
        cred -> Nullable<PgCredential>,
        chain_pointer -> Nullable<PgChainPointer>,
        staking_cred -> Nullable<PgStakingCredential>,
        address -> Nullable<PgAddress>,
        asset_quantity -> Nullable<PgAssetQuantity>,
        value -> Nullable<PgValue>,
        tx_in -> Nullable<PgTransactionInput>,
        datum -> Nullable<PgOutputDatum>,
        tx_out -> Nullable<PgTransactionOutput>,
        tx_in_info -> Nullable<PgTxInInfo>,
    }
}

diesel::allow_tables_to_appear_in_same_query!(sync_progress, utxos,);
