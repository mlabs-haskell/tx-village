CREATE SCHEMA Plutus;

SET search_path TO Plutus;

CREATE DOMAIN Hash28 AS
   BYTEA NOT NULL CHECK (LENGTH(value) = 28);

CREATE DOMAIN Hash32 AS
   BYTEA NOT NULL CHECK (LENGTH(value) = 32);

CREATE TYPE CurrencySymbol AS Hash28;

CREATE DOMAIN TokenName As
    BYTEA NOT NULL CHECK (LENGTH(value) <= 32);

CREATE TYPE TxId AS Hash32;

CREATE TYPE PubKeyHash AS Hash28;

CREATE TYPE ScriptHash AS Hash28;

CREATE TYPE DatumHash AS Hash28;

CREATE TABLE Credential (
    id BIGSERIAL PRIMARY KEY
);

CREATE TABLE PubKeyCredential (
    id BIGINT REFERENCES Credential(id),
    pub_key_hash PubKeyHash UNIQUE NOT NULL,
    PRIMARY KEY id
);

CREATE TABLE ScriptCredential (
    id BIGINT REFERENCES Credential(id),
    script_hash ScriptHash UNIQUE NOT NULL,
    PRIMARY KEY id
);

CREATE TABLE StakingCredential (
    id BIGSERIAL PRIMARY KEY
);

CREATE TABLE StakingHash (
    id BIGINT REFERENCES StakingCredential(id),
    staking_hash BIGINT NOT NULL REFERENCES Credential(id) UNIQUE,
    PRIMARY KEY id
);

CREATE TABLE StakingPtr (
    id BIGINT REFERENCES StakingCredential(id),
    slot_num INTEGER NOT NULL,
    tx_idx INTEGER NOT NULL,
    cert_idx INTEGER NOT NULL,
    UNIQUE (slot_num, tx_idx, cert_idx),
    PRIMARY KEY id
);

CREATE TABLE Address (
    id BIGSERIAL PRIMARY KEY,
    credential_id BIGINT NOT NULL REFERENCES Credential(id),
    staking_credential_id BIGINT NOT NULL REFERENCES StakingCredential(id),
    UNIQUE (credential_id, staking_credential_id)
);

CREATE TABLE Asset (
    id BIGSERIAL PRIMARY KEY,
    currency_symbol CurrencySymbol NOT NULL,
    token_name TokenName NOT NULL,
    UNIQUE (currency_symbol, token_name)
);

CREATE TABLE Value (
    id BIGSERIAL PRIMARY KEY,
    lovelace BIGINT DEFAULT 0
);

CREATE TABLE ValueComponent (
    owner_id BIGINT REFERENCES Value(id),
    asset BIGINT REFERENCES Asset(id) UNIQUE,
    amount BIGINT NOT NULL,
    PRIMARY KEY owner_id
);

CREATE TYPE TxOutRef_ AS (
    tx_id TxId,
    tx_idx BIGINT
);

CREATE DOMAIN TxOutRef AS
    TxOutRef_ CHECK ((value).tx_id IS NOT NULL AND (value).tx_idx IS NOT NULL);

CREATE TABLE OutputDatum (
    id BIGSERIAL PRIMRAY KEY
);

CREATE TABLE DatumHash (
    id BIGINT REFERENCES OutputDatum(id),
    datum_hash DatumHash UNIQUE NOT NULL
);

CREATE TABLE InlineDatum (
    id BIGINT REFERENCES OutputDatum(id),
    inline_datum JSONB NOT NULL
);

CREATE TABLE TxOut (
    id BIGSERIAL PRIMARY KEY,
    address_id BIGINT NOT NULL REFERENCES Address(id),
    value_id BIGINT NOT NULL REFERENCES Value(id),
    datum_id BIGINT NOT NULL REFERENCES OutputDatum(id),
    reference_script ScriptHash,
);

CREATE TABLE TxInInfo (
    id BIGSERIAL PRIMARY KEY,
    ref TxOutRef NOT NULL,
    resolved BIGINT NOT NULL REFERENCES TxOut(id)
);

CREATE TABLE TxInfo (
    id BIGSERIAL PRIMARY KEY,
    tx_id TxId NOT NULL UNIQUE,
    fee_id BIGINT NOT NULL REFERENCES Value(id),
    mint_id BIGINT NOT NULL REFERENCES Value(id)
);

CREATE TABLE TxInfoInputs (
    id BIGINT REFERENCES TxInfo(id),
    input_id BIGINT REFERENCES TxInInfo(id),
    PRIMARY KEY (id, input_id)
);

CREATE TABLE TxInfoReferenceInputs (
    id BIGINT REFERENCES TxInfo(id),
    input_id BIGINT REFERENCES TxInInfo(id),
    PRIMARY KEY (id, input_id)
);

CREATE TABLE TxInfoOutputs (
    id BIGINT REFERENCES TxInfo(id),
    output_id BIGINT REFERENCES TxOut(id),
    PRIMARY KEY (id, output_id)
);

CREATE TABLE TxInfoSignatories (
    id BIGINT REFERENCES TxInfo(id),
    pub_key_hash PubKeyHash,
    PRIMARY KEY (id, output_id)
);

CREATE TABLE TxInfoData (
    id BIGINT REFERENCES TxInfo(id),
    datum_hash DatumHash,
    datum JSONB,
    PRIMARY KEY (id, datum_hash, datum)
);
