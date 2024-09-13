diesel::table! {
    use crate::database::plutus::sql_types::*;
    use diesel::sql_types::*;

    sync_progress (processed) {
        block_slot -> Int8,
        block_hash -> Bytea,
        processed -> Bool,
    }
}

diesel::table! {
    use crate::database::plutus::sql_types::*;
    use diesel::sql_types::*;

    testdb (id) {
        id -> Int8,
        cur_sym -> Nullable<CurrencySymbol>,
        token_name -> Nullable<TokenName>,
        tx_hash -> Nullable<TransactionHash>,
        pub_key_hash -> Nullable<Ed25519PubKeyHash>,
        script_hash -> Nullable<ScriptHash>,
        datum_hash -> Nullable<DatumHash>,
        slot -> Nullable<Slot>,
        plutus_data -> Nullable<PlutusData>,
        cred -> Nullable<Credential>,
        chain_pointer -> Nullable<ChainPointer>,
        staking_cred -> Nullable<StakingCredential>,
        address -> Nullable<Address>,
        asset_quantity -> Nullable<AssetQuantity>,
        value -> Nullable<Value>,
        tx_in -> Nullable<TransactionInput>,
        datum -> Nullable<OutputDatum>,
        tx_out -> Nullable<TransactionOutput>,
        tx_in_info -> Nullable<TxInInfo>,
    }
}

diesel::table! {
    use crate::database::plutus::sql_types::*;
    use diesel::sql_types::*;

    utxos (utxo_ref) {
        utxo_ref -> TransactionInput,
        value -> Value,
        address -> Address,
        datum -> OutputDatum,
        created_at -> Slot,
        deleted_at -> Nullable<Slot>,
    }
}

diesel::allow_tables_to_appear_in_same_query!(sync_progress, testdb, utxos,);
