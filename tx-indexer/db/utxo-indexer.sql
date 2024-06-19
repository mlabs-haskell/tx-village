CREATE TABLE utxos (
  utxo_ref Plutus.TxOutRef NOT NULL,
  value Plutus.Value NOT NULL,
  address Plutus.Address NOT NULL,
  datum Plutus.OutputDatum NOT NULL,

  created_at Plutus.Slot NOT NULL,
  deleted_at Plutus.Slot
);
