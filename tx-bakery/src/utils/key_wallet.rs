use super::csl_to_pla::ToPLA;
use crate::wallet::{Wallet, WalletError};
use anyhow::anyhow;
use cardano_serialization_lib::crypto::PrivateKey;
use cardano_serialization_lib::crypto::Vkeywitnesses;
use cardano_serialization_lib::utils::{hash_transaction, make_vkey_witness};
use cardano_serialization_lib::Transaction;
use data_encoding::HEXLOWER;
use futures::future::OptionFuture;
use plutus_ledger_api::v2::address::{Address, Credential, StakingCredential};
use plutus_ledger_api::v2::crypto::Ed25519PubKeyHash;
use std::io::Cursor;
use std::path::Path;
use thiserror::Error;
use tokio;
use tokio::fs;

#[derive(Error, Debug)]
pub enum KeyWalletError {
    #[error("Failed to read private key: {0}")]
    PrivateKeyReadError(std::io::Error),

    #[error("Failed to parse private key: {0}")]
    PrivateKeyParseError(anyhow::Error),
}

impl From<KeyWalletError> for WalletError {
    fn from(err: KeyWalletError) -> WalletError {
        WalletError(anyhow!(err))
    }
}

#[derive(Debug, serde::Deserialize)]
struct TextEnvelope {
    // TODO: Verify that the TextEnvelope is correct (PaymentSigningKeyShelley_ed25519 or StakeSigningKeyShelley_ed25519)
    // #[serde(rename(deserialize = "type"))]
    // data_type: String,
    // description: String,
    #[serde(rename(deserialize = "cborHex"))]
    cbor_hex: String,
}

pub struct KeyWallet {
    pay_priv_key: PrivateKey,
    pay_pkh: Ed25519PubKeyHash,
    // TODO: Use these to implement staking features
    // stk_priv_key: Option<PrivateKey>,
    // stk_pkh: Option<Ed25519PubKeyHash>,
    address: Address,
}

impl KeyWallet {
    pub async fn new<P: AsRef<Path>>(
        payment_skey: P,
        staking_skey: Option<P>,
    ) -> Result<KeyWallet, KeyWalletError> {
        let pay_priv_key = Self::read_priv_key(payment_skey).await?;
        let pay_pkh: Ed25519PubKeyHash = pay_priv_key.to_public().hash().to_pla();

        let stk_priv_key = OptionFuture::from(staking_skey.map(Self::read_priv_key))
            .await
            .transpose()?;
        let stk_pkh = stk_priv_key
            .as_ref()
            .map(|priv_key| priv_key.to_public().hash().to_pla());

        let address = Address {
            credential: Credential::PubKey(pay_pkh.clone()),
            staking_credential: stk_pkh
                .clone()
                .map(|pkh| StakingCredential::Hash(Credential::PubKey(pkh))),
        };

        Ok(KeyWallet {
            pay_priv_key,
            pay_pkh,
            // stk_priv_key,
            // stk_pkh,
            address,
        })
    }
    /// Get the private key used by Plutip
    async fn read_priv_key(filepath: impl AsRef<Path>) -> Result<PrivateKey, KeyWalletError> {
        let skey_str = fs::read_to_string(&filepath)
            .await
            .map_err(KeyWalletError::PrivateKeyReadError)?;

        let text_envelope: TextEnvelope = serde_json::from_str(&skey_str)
            .map_err(|err| KeyWalletError::PrivateKeyParseError(anyhow!(err)))?;

        let mut raw = cbor_event::de::Deserializer::from(Cursor::new(
            HEXLOWER
                .decode(&text_envelope.cbor_hex.clone().into_bytes())
                .unwrap(),
        ));
        let bytes: Vec<u8> = raw.bytes().unwrap();

        PrivateKey::from_normal_bytes(&bytes)
            .map_err(|err| KeyWalletError::PrivateKeyParseError(anyhow!(err)))
    }
}

impl Wallet for KeyWallet {
    fn sign_transaction(&self, tx: &Transaction) -> Transaction {
        let tx_body = tx.body();
        let mut witness_set = tx.witness_set();
        let aux_data = tx.auxiliary_data();

        let mut vkey_witnesses = witness_set.vkeys().unwrap_or(Vkeywitnesses::new());
        vkey_witnesses.add(&make_vkey_witness(
            &hash_transaction(&tx_body),
            &self.pay_priv_key,
        ));

        witness_set.set_vkeys(&vkey_witnesses);

        Transaction::new(&tx_body, &witness_set, aux_data)
    }

    fn get_change_pkh(&self) -> Ed25519PubKeyHash {
        self.pay_pkh.clone()
    }

    fn get_change_addr(&self) -> Address {
        self.address.clone()
    }
}
