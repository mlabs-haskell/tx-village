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

diesel::allow_tables_to_appear_in_same_query!(sync_progress, utxos,);
