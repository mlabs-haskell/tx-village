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
    PlutusScript(csl::plutus::PlutusScript, PlutusVersion),
    NativeScript(csl::NativeScript),
}

#[derive(Clone, Debug)]
pub enum PlutusVersion {
    V1,
    V2,
}

impl From<&PlutusVersion> for csl::plutus::Language {
    fn from(lang: &PlutusVersion) -> Self {
        match lang {
            crate::utils::script::PlutusVersion::V1 => csl::plutus::Language::new_plutus_v1(),
            crate::utils::script::PlutusVersion::V2 => csl::plutus::Language::new_plutus_v2(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ScriptOrRef {
    RefScript(TransactionInput, csl::plutus::PlutusScript, PlutusVersion),
    PlutusScript(csl::plutus::PlutusScript, PlutusVersion),
}

impl ScriptOrRef {
    // TODO: Handle plutus versions
    pub fn from_bytes(bytes: Vec<u8>, version: PlutusVersion) -> Result<Self> {
        let mut serializer = cbor_event::se::Serializer::new_vec();
        serializer.write_bytes(bytes).unwrap();
        let script_bytes = serializer.finalize();

        let script = csl::plutus::PlutusScript::from_bytes_v2(script_bytes)
            .map_err(|source| Error::ConversionError(anyhow!(source)))?;
        Ok(ScriptOrRef::PlutusScript(script, version))
    }

    pub fn from_script(script: Script) -> Result<Self> {
        match script {
            Script::PlutusScript(script, version) => Ok(ScriptOrRef::PlutusScript(script, version)),
            Script::NativeScript(_) => Err(Error::Unsupported(
                "Native scripts are not supported yet.".to_string(),
            )),
        }
    }

    pub fn into_ref_script(self, tx_in: TransactionInput) -> Self {
        match self {
            ScriptOrRef::RefScript(_, _, _) => self,
            ScriptOrRef::PlutusScript(script, version) => {
                ScriptOrRef::RefScript(tx_in, script, version)
            }
        }
    }

    pub fn get_script(self) -> csl::plutus::PlutusScript {
        match self {
            ScriptOrRef::RefScript(_, script, _) => script,
            ScriptOrRef::PlutusScript(script, _) => script,
        }
    }

    pub fn get_script_hash(self) -> ScriptHash {
        match self {
            ScriptOrRef::RefScript(_, script, _) => script.hash().to_pla(),
            ScriptOrRef::PlutusScript(script, _) => script.hash().to_pla(),
        }
    }

    pub fn get_version(self) -> PlutusVersion {
        match self {
            ScriptOrRef::RefScript(_, _, version) => version,
            ScriptOrRef::PlutusScript(_, version) => version,
        }
    }

    pub fn as_validator(self) -> (ValidatorHash, ScriptOrRef) {
        let script = self.clone().get_script();

        let validator_hash = ValidatorHash(script.hash().to_pla());

        (validator_hash, self)
    }

    pub fn as_minting_policy(self) -> (MintingPolicyHash, ScriptOrRef) {
        let script = self.clone().get_script();

        let minting_policy_hash = MintingPolicyHash(script.hash().to_pla());

        (minting_policy_hash, self)
    }
}
