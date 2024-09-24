--
-- PostgreSQL database dump
--

-- Dumped from database version 16.3
-- Dumped by pg_dump version 16.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plutus; Type: SCHEMA; Schema: -; Owner: tx_indexer
--

CREATE SCHEMA plutus;


ALTER SCHEMA plutus OWNER TO tx_indexer;

--
-- Name: chain_pointer; Type: TYPE; Schema: plutus; Owner: tx_indexer
--

CREATE TYPE plutus.chain_pointer AS (
	slot_num bigint,
	tx_idx bigint,
	cert_idx bigint
);


ALTER TYPE plutus.chain_pointer OWNER TO tx_indexer;

--
-- Name: hash28; Type: DOMAIN; Schema: plutus; Owner: tx_indexer
--

CREATE DOMAIN plutus.hash28 AS bytea
	CONSTRAINT hash28_check CHECK ((length(VALUE) = 28));


ALTER DOMAIN plutus.hash28 OWNER TO tx_indexer;

--
-- Name: ed25519_pub_key_hash; Type: DOMAIN; Schema: plutus; Owner: tx_indexer
--

CREATE DOMAIN plutus.ed25519_pub_key_hash AS plutus.hash28;


ALTER DOMAIN plutus.ed25519_pub_key_hash OWNER TO tx_indexer;

--
-- Name: script_hash; Type: DOMAIN; Schema: plutus; Owner: tx_indexer
--

CREATE DOMAIN plutus.script_hash AS plutus.hash28;


ALTER DOMAIN plutus.script_hash OWNER TO tx_indexer;

--
-- Name: credential; Type: TYPE; Schema: plutus; Owner: tx_indexer
--

CREATE TYPE plutus.credential AS (
	pub_key_hash plutus.ed25519_pub_key_hash,
	script_hash plutus.script_hash
);


ALTER TYPE plutus.credential OWNER TO tx_indexer;

--
-- Name: staking_credential; Type: TYPE; Schema: plutus; Owner: tx_indexer
--

CREATE TYPE plutus.staking_credential AS (
	staking_hash plutus.credential,
	staking_ptr plutus.chain_pointer
);


ALTER TYPE plutus.staking_credential OWNER TO tx_indexer;

--
-- Name: address; Type: TYPE; Schema: plutus; Owner: tx_indexer
--

CREATE TYPE plutus.address AS (
	credential plutus.credential,
	staking_credential plutus.staking_credential
);


ALTER TYPE plutus.address OWNER TO tx_indexer;

--
-- Name: currency_symbol; Type: DOMAIN; Schema: plutus; Owner: tx_indexer
--

CREATE DOMAIN plutus.currency_symbol AS bytea
	CONSTRAINT currency_symbol_check CHECK (((length(VALUE) = 28) OR (length(VALUE) = 0)));


ALTER DOMAIN plutus.currency_symbol OWNER TO tx_indexer;

--
-- Name: token_name; Type: DOMAIN; Schema: plutus; Owner: tx_indexer
--

CREATE DOMAIN plutus.token_name AS bytea
	CONSTRAINT token_name_check CHECK ((length(VALUE) <= 32));


ALTER DOMAIN plutus.token_name OWNER TO tx_indexer;

--
-- Name: asset_quantity; Type: TYPE; Schema: plutus; Owner: tx_indexer
--

CREATE TYPE plutus.asset_quantity AS (
	currency_symbol plutus.currency_symbol,
	token_name plutus.token_name,
	amount bigint
);


ALTER TYPE plutus.asset_quantity OWNER TO tx_indexer;

--
-- Name: hash32; Type: DOMAIN; Schema: plutus; Owner: tx_indexer
--

CREATE DOMAIN plutus.hash32 AS bytea
	CONSTRAINT hash32_check CHECK ((length(VALUE) = 32));


ALTER DOMAIN plutus.hash32 OWNER TO tx_indexer;

--
-- Name: datum_hash; Type: DOMAIN; Schema: plutus; Owner: tx_indexer
--

CREATE DOMAIN plutus.datum_hash AS plutus.hash32;


ALTER DOMAIN plutus.datum_hash OWNER TO tx_indexer;

--
-- Name: output_datum; Type: TYPE; Schema: plutus; Owner: tx_indexer
--

CREATE TYPE plutus.output_datum AS (
	datum_hash plutus.datum_hash,
	inline_datum jsonb
);


ALTER TYPE plutus.output_datum OWNER TO tx_indexer;

--
-- Name: plutus_data; Type: DOMAIN; Schema: plutus; Owner: tx_indexer
--

CREATE DOMAIN plutus.plutus_data AS jsonb;


ALTER DOMAIN plutus.plutus_data OWNER TO tx_indexer;

--
-- Name: slot; Type: DOMAIN; Schema: plutus; Owner: tx_indexer
--

CREATE DOMAIN plutus.slot AS bigint;


ALTER DOMAIN plutus.slot OWNER TO tx_indexer;

--
-- Name: transaction_hash; Type: DOMAIN; Schema: plutus; Owner: tx_indexer
--

CREATE DOMAIN plutus.transaction_hash AS plutus.hash32;


ALTER DOMAIN plutus.transaction_hash OWNER TO tx_indexer;

--
-- Name: transaction_input; Type: TYPE; Schema: plutus; Owner: tx_indexer
--

CREATE TYPE plutus.transaction_input AS (
	tx_id plutus.transaction_hash,
	tx_idx bigint
);


ALTER TYPE plutus.transaction_input OWNER TO tx_indexer;

--
-- Name: transaction_output; Type: TYPE; Schema: plutus; Owner: tx_indexer
--

CREATE TYPE plutus.transaction_output AS (
	address plutus.address,
	assets plutus.asset_quantity[],
	datum plutus.output_datum,
	reference_script plutus.script_hash
);


ALTER TYPE plutus.transaction_output OWNER TO tx_indexer;

--
-- Name: tx_in_info; Type: TYPE; Schema: plutus; Owner: tx_indexer
--

CREATE TYPE plutus.tx_in_info AS (
	reference plutus.transaction_input,
	output plutus.transaction_output
);


ALTER TYPE plutus.tx_in_info OWNER TO tx_indexer;

--
-- Name: value; Type: DOMAIN; Schema: plutus; Owner: tx_indexer
--

CREATE DOMAIN plutus.value AS plutus.asset_quantity[];


ALTER DOMAIN plutus.value OWNER TO tx_indexer;

--
-- Name: diesel_manage_updated_at(regclass); Type: FUNCTION; Schema: public; Owner: tx_indexer
--

CREATE FUNCTION public.diesel_manage_updated_at(_tbl regclass) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    EXECUTE format('CREATE TRIGGER set_updated_at BEFORE UPDATE ON %s
                    FOR EACH ROW EXECUTE PROCEDURE diesel_set_updated_at()', _tbl);
END;
$$;


ALTER FUNCTION public.diesel_manage_updated_at(_tbl regclass) OWNER TO tx_indexer;

--
-- Name: diesel_set_updated_at(); Type: FUNCTION; Schema: public; Owner: tx_indexer
--

CREATE FUNCTION public.diesel_set_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF (
        NEW IS DISTINCT FROM OLD AND
        NEW.updated_at IS NOT DISTINCT FROM OLD.updated_at
    ) THEN
        NEW.updated_at := current_timestamp;
    END IF;
    RETURN NEW;
END;
$$;


ALTER FUNCTION public.diesel_set_updated_at() OWNER TO tx_indexer;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: __diesel_schema_migrations; Type: TABLE; Schema: public; Owner: tx_indexer
--

CREATE TABLE public.__diesel_schema_migrations (
    version character varying(50) NOT NULL,
    run_on timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE public.__diesel_schema_migrations OWNER TO tx_indexer;

--
-- Name: sync_progress; Type: TABLE; Schema: public; Owner: tx_indexer
--

CREATE TABLE public.sync_progress (
    block_slot bigint NOT NULL,
    block_hash bytea NOT NULL,
    processed boolean NOT NULL
);


ALTER TABLE public.sync_progress OWNER TO tx_indexer;

--
-- Name: testdb; Type: TABLE; Schema: public; Owner: tx_indexer
--

CREATE TABLE public.testdb (
    id bigint NOT NULL,
    cur_sym plutus.currency_symbol,
    token_name plutus.token_name,
    tx_hash plutus.transaction_hash,
    pub_key_hash plutus.ed25519_pub_key_hash,
    script_hash plutus.script_hash,
    datum_hash plutus.datum_hash,
    slot plutus.slot,
    plutus_data plutus.plutus_data,
    cred plutus.credential,
    chain_pointer plutus.chain_pointer,
    staking_cred plutus.staking_credential,
    address plutus.address,
    asset_quantity plutus.asset_quantity,
    value plutus.value,
    tx_in plutus.transaction_input,
    datum plutus.output_datum,
    tx_out plutus.transaction_output,
    tx_in_info plutus.tx_in_info
);


ALTER TABLE public.testdb OWNER TO tx_indexer;

--
-- Name: utxos; Type: TABLE; Schema: public; Owner: tx_indexer
--

CREATE TABLE public.utxos (
    utxo_ref plutus.transaction_input NOT NULL,
    value plutus.value NOT NULL,
    address plutus.address NOT NULL,
    datum plutus.output_datum NOT NULL,
    created_at plutus.slot NOT NULL,
    deleted_at plutus.slot
);


ALTER TABLE public.utxos OWNER TO tx_indexer;

--
-- Name: __diesel_schema_migrations __diesel_schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: tx_indexer
--

ALTER TABLE ONLY public.__diesel_schema_migrations
    ADD CONSTRAINT __diesel_schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: sync_progress sync_progress_pkey; Type: CONSTRAINT; Schema: public; Owner: tx_indexer
--

ALTER TABLE ONLY public.sync_progress
    ADD CONSTRAINT sync_progress_pkey PRIMARY KEY (processed);


--
-- Name: testdb testdb_pkey; Type: CONSTRAINT; Schema: public; Owner: tx_indexer
--

ALTER TABLE ONLY public.testdb
    ADD CONSTRAINT testdb_pkey PRIMARY KEY (id);


--
-- Name: utxos utxos_pkey; Type: CONSTRAINT; Schema: public; Owner: tx_indexer
--

ALTER TABLE ONLY public.utxos
    ADD CONSTRAINT utxos_pkey PRIMARY KEY (utxo_ref);


--
-- PostgreSQL database dump complete
--

