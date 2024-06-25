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

CREATE TYPE Credential_ AS (
    pub_key_hash Ed25519PubKeyHash,
    script_hash ScriptHash
);

CREATE DOMAIN Credential AS Credential_ 
    CHECK ((value) IS NULL OR (
        ((value).pub_key_hash IS NULL AND NOT (value).script_hash IS NULL) OR
        ((NOT (value).pub_key_hash IS NULL AND (value).script_hash IS NULL))
    ));

CREATE TYPE ChainPointer_ AS (
    slot_num INTEGER,
    tx_idx INTEGER,
    cert_idx INTEGER
);

CREATE DOMAIN ChainPointer AS ChainPointer_ 
    CHECK ((value) IS NULL OR (
        NOT (value).slot_num IS NULL AND
        NOT (value).tx_idx IS NULL AND
        NOT (value).cert_idx IS NULL
    ));

CREATE TYPE StakingCredential_ AS (
    staking_hash Credential,
    staking_ptr ChainPointer
);

CREATE DOMAIN StakingCredential AS StakingCredential_
    CHECK ((value) IS NULL OR (
        ((value).staking_hash IS NULL AND NOT (value).staking_ptr IS NULL) OR
        (NOT (value).staking_hash IS NULL AND (value).staking_ptr IS NULL)
    ));

CREATE TYPE Address_ AS (
    credential Credential,
    staking_credential StakingCredential
);

CREATE DOMAIN Address AS Address_
    CHECK ((value) IS NULL OR (
        NOT (value).credential IS NULL
    ));

CREATE TYPE AssetQuantity_ AS (
    currency_symbol CurrencySymbol,
    token_name TokenName,
    amount BIGINT
);

CREATE DOMAIN AssetQuantity AS AssetQuantity_
    CHECK ((value) IS NULL OR (
        NOT (value).currency_symbol IS NULL AND
        NOT (value).token_name IS NULL AND
        NOT (value).amount IS NULL
    ));

CREATE DOMAIN Value AS AssetQuantity[];

CREATE TYPE TransactionInput_ AS (
    tx_id TransactionHash,
    tx_idx BIGINT
);

CREATE DOMAIN TransactionInput AS TransactionInput_
    CHECK ((value) IS NULL OR (
        NOT (value).tx_id IS NULL AND
        NOT (value).tx_idx IS NULL
    ));

CREATE TYPE OutputDatum AS (
    datum_hash DatumHash,
    inline_datum PlutusData
);

CREATE TYPE TransactionOutput_ AS (
    address Address,
    assets Value,
    datum OutputDatum,
    reference_script ScriptHash
);

CREATE DOMAIN TransactionOutput AS TransactionOutput_
    CHECK ((value) IS NULL OR (
        NOT (value).address IS NULL AND
        NOT (value).assets IS NULL
    ));

CREATE TYPE TxInInfo_ AS (
    reference TransactionInput,
    output TransactionOutput
);

CREATE DOMAIN TxInInfo AS TxInInfo_
    CHECK ((value) IS NULL OR (
        NOT (value).reference IS NULL AND 
        NOT (value).output IS NULL
    ));
