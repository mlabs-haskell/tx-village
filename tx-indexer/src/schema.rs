diesel::table! {
    use crate::database::plutus::sql_types::*;
    use diesel::sql_types::*;

    sync_progress (processed) {
        block_slot -> Int8,
        block_hash -> Bytea,
        processed -> Bool,
    }
}
