CREATE SCHEMA Plutus;

SET search_path TO Plutus;

CREATE DOMAIN Hash28 AS
   BYTEA CHECK (LENGTH(value) = 28);

CREATE DOMAIN Hash32 AS
   BYTEA CHECK (LENGTH(value) = 32);

CREATE DOMAIN CurrencySymbol AS Hash28;

CREATE DOMAIN TokenName As
    BYTEA CHECK (LENGTH(value) <= 32);

CREATE DOMAIN TxId AS Hash32;

CREATE DOMAIN PubKeyHash AS Hash28;

CREATE DOMAIN ScriptHash AS Hash28;

CREATE DOMAIN DatumHash AS Hash28;

CREATE DOMAIN Slot AS BIGINT;

CREATE TYPE Credential_ AS (
    pub_key_hash PubKeyHash,
    script_hash ScriptHash
);

CREATE DOMAIN Credential AS
    Credential_ CHECK (
        ((value).pub_key_hash IS NULL AND (value).script_hash IS NOT NULL)
        OR (((value).pub_key_hash IS NOT NULL AND (value).script_hash IS NULL))
    );

CREATE TYPE StakingPtr_ AS (
    slot_num INTEGER,
    tx_idx INTEGER,
    cert_idx INTEGER
);

CREATE DOMAIN StakingPtr AS StakingPtr_ CHECK (
    (value).slot_num IS NOT NULL AND (value).tx_idx IS NOT NULL AND (value).cert_idx IS NOT NULL);

CREATE TYPE StakingCredential_ AS (
    staking_hash Credential,
    staking_tr StakingPtr
);

CREATE DOMAIN StakingCredential AS
    StakingCredential_ CHECK (
        ((value).staking_hash IS NULL AND (value).staking_tr IS NOT NULL)
        OR (((value).staking_hash IS NOT NULL AND (value).staking_tr IS NULL))
    );

CREATE TYPE Address_ AS (
    credential Credential,
    staking_credential StakingCredential
);

CREATE DOMAIN Address AS Address_ CHECK ((value).credential IS NOT NULL);

CREATE TYPE AssetQuantity_ AS (
    currency_symbol CurrencySymbol,
    token_name TokenName,
    amount BIGINT
);

CREATE DOMAIN AssetQuantity AS
    AssetQuantity_ CHECK ((value).currency_symbol IS NOT NULL AND (value).token_name IS NOT NULL AND (value).amount IS NOT NULL);

CREATE TYPE Value_ AS (
    assets AssetQuantity[],
    lovelace BIGINT
);

CREATE DOMAIN Value AS
    Value_ CHECK ((value).assets IS NOT NULL AND (value).lovelace IS NOT NULL);

CREATE TYPE TxOutRef_ AS (
    tx_id TxId,
    tx_idx BIGINT
);

CREATE DOMAIN TxOutRef AS
    TxOutRef_ CHECK ((value).tx_id IS NOT NULL AND (value).tx_idx IS NOT NULL);

CREATE TYPE OutputDatum_ AS (
    datum_hash DatumHash,
    inline_datum JSONB
);

CREATE DOMAIN OutputDatum AS
    OutputDatum_ CHECK (
        ((value).datum_hash IS NULL AND (value).inline_datum IS NOT NULL)
        OR (((value).datum_hash IS NOT NULL AND (value).inline_datum IS NULL))
    );

CREATE TYPE TxOut_ AS (
    address Address,
    assets Value,
    datum OutputDatum,
    reference_script ScriptHash
);

CREATE DOMAIN TxOut AS TxOut_ CHECK ((value).address IS NOT NULL AND (value).assets IS NOT NULL);

CREATE TYPE TxInInfo_ AS (
    ref TxOutRef,
    resolved TxOut
);

CREATE DOMAIN TxInInfo AS TxInInfo_ CHECK ((value).ref IS NOT NULL AND (value).resolved IS NOT NULL);
