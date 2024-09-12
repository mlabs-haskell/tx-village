-- Your SQL goes here

CREATE TABLE testdb (
  id INT8,
  cur_sym Plutus.CurrencySymbol,
  token_name Plutus.TokenName,
  tx_hash Plutus.TransactionHash,
  pub_key_hash Plutus.Ed25519PubKeyHash,
  script_hash Plutus.ScriptHash,
  datum_hash Plutus.DatumHash,
  slot Plutus.Slot,
  plutus_data Plutus.PlutusData,
  cred Plutus.Credential,
  chain_pointer Plutus.ChainPointer,
  staking_cred Plutus.StakingCredential,
  address Plutus.Address,
  asset_quantity Plutus.AssetQuantity,
  value Plutus.Value,
  tx_in Plutus.TransactionInput,
  datum Plutus.OutputDatum,
  tx_out Plutus.TransactionOutput,
  tx_in_info Plutus.TxInInfo
);

