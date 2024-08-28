CREATE SCHEMA Plutus;

SET search_path TO Plutus;

CREATE DOMAIN Hash28 AS
    BYTEA CHECK (LENGTH(value) = 28);

CREATE DOMAIN Hash32 AS
    BYTEA CHECK (LENGTH(value) = 32);

CREATE DOMAIN CurrencySymbol AS 
    BYTEA CHECK (LENGTH(value) = 28 OR LENGTH(value) = 0);

CREATE DOMAIN TokenName As
    BYTEA CHECK (LENGTH(value) <= 32);

CREATE DOMAIN TransactionHash AS Hash32;

CREATE DOMAIN Ed25519PubKeyHash AS Hash28;

CREATE DOMAIN ScriptHash AS Hash28;

CREATE DOMAIN DatumHash AS Hash32;

CREATE DOMAIN Slot AS BIGINT;

CREATE DOMAIN PlutusData AS JSONB;

CREATE TYPE Credential AS (
    pub_key_hash Ed25519PubKeyHash,
    script_hash ScriptHash
);

CREATE TYPE ChainPointer AS (
    slot_num BIGINT,
    tx_idx BIGINT,
    cert_idx BIGINT
);


CREATE TYPE StakingCredential AS (
    staking_hash Credential,
    staking_ptr ChainPointer
);

CREATE TYPE Address AS (
    credential Credential,
    staking_credential StakingCredential
);

CREATE TYPE AssetQuantity AS (
    currency_symbol CurrencySymbol,
    token_name TokenName,
    amount BIGINT
);

CREATE DOMAIN Value AS AssetQuantity[];

CREATE TYPE TransactionInput AS (
    tx_id TransactionHash,
    tx_idx BIGINT
);

CREATE TYPE OutputDatum AS (
    datum_hash DatumHash,
    -- FIXME(szg251): Couldn't use PlutusData domain type becauseof a
    -- deserialization error: `expected value at line 1 column 1`
    inline_datum JSONB
);

CREATE TYPE TransactionOutput AS (
    address Address,
    assets AssetQuantity[],
    datum OutputDatum,
    reference_script ScriptHash
);

CREATE TYPE TxInInfo AS (
    reference TransactionInput,
    output TransactionOutput
);

