CREATE TABLE transactions (
  transaction_id Plutus.TxId NOT NULL,
  created_at Plutus.Slot NOT NULL,
  deleted_at Plutus.Slot
);
