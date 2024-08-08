//! Plutus Script management

use super::csl_to_pla::ToPLA;
use crate::error::{Error, Result};
use anyhow::anyhow;
use cardano_serialization_lib as csl;
use plutus_ledger_api::v2::{
    script::{MintingPolicyHash, ScriptHash, ValidatorHash},
    transaction::TransactionInput,
};

#[derive(Clone, Debug)]
pub enum Script {
    PlutusScript(csl::plutus::PlutusScript),
    NativeScript(csl::NativeScript),
}

/// Plutus Script
#[derive(Clone, Debug)]
pub enum ScriptOrRef {
    /// Script will be used from a reference input
    RefScript(TransactionInput, csl::plutus::PlutusScript),
    /// Script will be added as script witness
    PlutusScript(csl::plutus::PlutusScript),
}

impl ScriptOrRef {
    // TODO: Handle plutus versions
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self> {
        let mut serializer = cbor_event::se::Serializer::new_vec();
        serializer.write_bytes(bytes).unwrap();
        let script_bytes = serializer.finalize();

        let script = csl::plutus::PlutusScript::from_bytes_v2(script_bytes)
            .map_err(|source| Error::ConversionError(anyhow!(source)))?;
        Ok(ScriptOrRef::PlutusScript(script))
    }

    pub fn from_script(script: Script) -> Result<Self> {
        match script {
            Script::PlutusScript(script) => Ok(ScriptOrRef::PlutusScript(script)),
            Script::NativeScript(_) => Err(Error::Unsupported(
                "Native scripts are not supported yet.".to_string(),
            )),
        }
    }

    pub fn into_ref_script(self, tx_in: TransactionInput) -> Self {
        match self {
            ScriptOrRef::RefScript(_, _) => self,
            ScriptOrRef::PlutusScript(script) => ScriptOrRef::RefScript(tx_in, script),
        }
    }

    pub fn get_script(self) -> csl::plutus::PlutusScript {
        match self {
            ScriptOrRef::RefScript(_, script) => script,
            ScriptOrRef::PlutusScript(script) => script,
        }
    }

    pub fn get_script_hash(&self) -> ScriptHash {
        match self {
            ScriptOrRef::RefScript(_, script) => script.hash().to_pla(),
            ScriptOrRef::PlutusScript(script) => script.hash().to_pla(),
        }
    }

    pub fn get_version(self) -> csl::plutus::Language {
        match self {
            ScriptOrRef::RefScript(_, script) => script.language_version(),
            ScriptOrRef::PlutusScript(script) => script.language_version(),
        }
    }

    pub fn as_validator(self) -> (ValidatorHash, ScriptOrRef) {
        let (sh, s) = self.with_script_hash();
        (ValidatorHash(sh), s)
    }

    pub fn as_minting_policy(self) -> (MintingPolicyHash, ScriptOrRef) {
        let (sh, s) = self.with_script_hash();
        (MintingPolicyHash(sh), s)
    }

    pub fn with_script_hash(self) -> (ScriptHash, ScriptOrRef) {
        let script_hash = self.get_script_hash();

        (script_hash, self)
    }
}
