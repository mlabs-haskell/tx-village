use num_bigint::BigInt;
use plutus_ledger_api::{self as pla};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::types::{v1, v2, v3};

#[derive(Clone, Copy, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum PlutusVersion {
    V1,
    V2,
    V3,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MultiEraTransaction {
    pub inputs: Vec<pla::v3::TransactionInput>,
    pub reference_inputs: Vec<pla::v3::TransactionInput>,
    pub outputs: Vec<MultiEraTransactionOutput>,
    pub fee: pla::v3::Value,
    pub mint: pla::v3::Value,
    pub d_cert: Vec<pla::v3::DCert>,
    pub wdrl: pla::v3::AssocMap<pla::v3::StakingCredential, BigInt>,
    pub valid_range: pla::v3::POSIXTimeRange,
    pub signatories: Vec<pla::v3::PaymentPubKeyHash>,
    pub redeemers: pla::v3::AssocMap<MultiEraScriptPurpose, pla::v3::Redeemer>,
    pub datums: pla::v3::AssocMap<pla::v3::DatumHash, pla::v3::Datum>,
    pub id: pla::v3::TransactionHash,
    pub plutus_version: PlutusVersion,
}

impl From<v1::Transaction> for MultiEraTransaction {
    fn from(value: v1::Transaction) -> Self {
        Self {
            inputs: value
                .inputs
                .into_iter()
                .map(pla::v3::TransactionInput::from)
                .collect(),
            reference_inputs: Vec::new(),
            outputs: value
                .outputs
                .into_iter()
                .map(MultiEraTransactionOutput::from)
                .collect(),
            fee: value.fee,
            mint: value.mint,
            d_cert: value.d_cert,
            wdrl: value.wdrl,
            valid_range: value.valid_range,
            signatories: value.signatories,
            redeemers: value
                .redeemers
                .into_iter()
                .map(|(script_purpose, redeemer)| (script_purpose.into(), redeemer))
                .collect(),
            datums: value.datums,
            id: value.id.into(),
            plutus_version: PlutusVersion::V1,
        }
    }
}

impl From<v2::Transaction> for MultiEraTransaction {
    fn from(value: v2::Transaction) -> Self {
        Self {
            inputs: value
                .inputs
                .into_iter()
                .map(pla::v3::TransactionInput::from)
                .collect(),
            reference_inputs: value
                .reference_inputs
                .into_iter()
                .map(pla::v3::TransactionInput::from)
                .collect(),
            outputs: value
                .outputs
                .into_iter()
                .map(MultiEraTransactionOutput::from)
                .collect(),
            fee: value.fee,
            mint: value.mint,
            d_cert: value.d_cert,
            wdrl: value.wdrl,
            valid_range: value.valid_range,
            signatories: value.signatories,
            redeemers: value
                .redeemers
                .into_iter()
                .map(|(script_purpose, redeemer)| (script_purpose.into(), redeemer))
                .collect(),
            datums: value.datums,
            id: value.id.into(),
            plutus_version: PlutusVersion::V1,
        }
    }
}

impl From<v3::Transaction> for MultiEraTransaction {
    fn from(value: v3::Transaction) -> Self {
        Self {
            inputs: value.inputs,
            reference_inputs: value.reference_inputs,
            outputs: value
                .outputs
                .into_iter()
                .map(MultiEraTransactionOutput::from)
                .collect(),
            fee: value.fee,
            mint: value.mint,
            d_cert: value.d_cert,
            wdrl: value.wdrl,
            valid_range: value.valid_range,
            signatories: value.signatories,
            redeemers: value
                .redeemers
                .into_iter()
                .map(|(script_purpose, redeemer)| (script_purpose.into(), redeemer))
                .collect(),
            datums: value.datums,
            id: value.id,
            plutus_version: PlutusVersion::V1,
        }
    }
}

#[derive(Debug, Error)]
pub enum TransactionConversionError {
    #[error("Transaction content doesn't match Plutus version")]
    PlutusVersionMismatch,
}

impl TryFrom<MultiEraTransaction> for v1::Transaction {
    type Error = TransactionConversionError;

    fn try_from(value: MultiEraTransaction) -> Result<Self, Self::Error> {
        Ok(Self {
            inputs: value
                .inputs
                .into_iter()
                .map(pla::v1::TransactionInput::from)
                .collect(),
            outputs: value
                .outputs
                .into_iter()
                .map(v1::TransactionOutput::try_from)
                .collect::<Result<_, _>>()?,
            fee: value.fee,
            mint: value.mint,
            d_cert: value.d_cert,
            wdrl: value.wdrl,
            valid_range: value.valid_range,
            signatories: value.signatories,
            redeemers: value
                .redeemers
                .into_iter()
                .map(|(script_purpose, redeemer)| {
                    Ok((pla::v1::ScriptPurpose::try_from(script_purpose)?, redeemer))
                })
                .collect::<Result<_, _>>()?,
            datums: value.datums,
            id: value.id.into(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MultiEraScriptPurpose {
    Minting(pla::v3::CurrencySymbol),
    Spending(pla::v3::TransactionInput),
    Rewarding(MultiEraRewardingCredential),
    Certifying(MultiEraTxCert),
    Voting(pla::v3::Voter),
    Proposing(
        /// 0-based index of the given `ProposalProcedure` in `proposal_procedures` field of the `TransactionInfo`
        BigInt,
        pla::v3::ProposalProcedure,
    ),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MultiEraRewardingCredential {
    // V1 and V2 are identical
    V1(pla::v1::StakingCredential),
    V3(pla::v3::Credential),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MultiEraTxCert {
    // V1 and V2 are identical
    V1(pla::v1::DCert),
    V3(BigInt, pla::v3::TxCert),
}

impl From<pla::v1::ScriptPurpose> for MultiEraScriptPurpose {
    fn from(value: pla::v1::ScriptPurpose) -> Self {
        match value {
            pla::v1::ScriptPurpose::Minting(currency_symbol) => {
                MultiEraScriptPurpose::Minting(currency_symbol)
            }
            pla::v1::ScriptPurpose::Spending(transaction_input) => {
                MultiEraScriptPurpose::Spending(transaction_input.into())
            }
            pla::v1::ScriptPurpose::Rewarding(staking_credential) => {
                MultiEraScriptPurpose::Rewarding(MultiEraRewardingCredential::V1(
                    staking_credential,
                ))
            }
            pla::v1::ScriptPurpose::Certifying(dcert) => {
                MultiEraScriptPurpose::Certifying(MultiEraTxCert::V1(dcert))
            }
        }
    }
}

impl From<pla::v3::ScriptPurpose> for MultiEraScriptPurpose {
    fn from(value: pla::v3::ScriptPurpose) -> Self {
        match value {
            pla::v3::ScriptPurpose::Minting(currency_symbol) => {
                MultiEraScriptPurpose::Minting(currency_symbol)
            }
            pla::v3::ScriptPurpose::Spending(transaction_input) => {
                MultiEraScriptPurpose::Spending(transaction_input)
            }
            pla::v3::ScriptPurpose::Rewarding(credential) => {
                MultiEraScriptPurpose::Rewarding(MultiEraRewardingCredential::V3(credential))
            }
            pla::v3::ScriptPurpose::Certifying(cert_idx, tx_cert) => {
                MultiEraScriptPurpose::Certifying(MultiEraTxCert::V3(cert_idx, tx_cert))
            }
            plutus_ledger_api::v3::ScriptPurpose::Voting(voter) => {
                MultiEraScriptPurpose::Voting(voter)
            }
            plutus_ledger_api::v3::ScriptPurpose::Proposing(proposal_idx, proposal_procedure) => {
                MultiEraScriptPurpose::Proposing(proposal_idx, proposal_procedure)
            }
        }
    }
}

impl TryFrom<MultiEraScriptPurpose> for pla::v1::ScriptPurpose {
    type Error = TransactionConversionError;

    fn try_from(value: MultiEraScriptPurpose) -> Result<Self, Self::Error> {
        Ok(match value {
            MultiEraScriptPurpose::Minting(currency_symbol) => {
                pla::v1::ScriptPurpose::Minting(currency_symbol)
            }
            MultiEraScriptPurpose::Spending(transaction_input) => {
                pla::v1::ScriptPurpose::Spending(transaction_input.into())
            }
            MultiEraScriptPurpose::Rewarding(multi_era_rewarding_credential) => {
                match multi_era_rewarding_credential {
                    MultiEraRewardingCredential::V1(staking_credential) => {
                        pla::v1::ScriptPurpose::Rewarding(staking_credential)
                    }
                    MultiEraRewardingCredential::V3(_) => {
                        Err(TransactionConversionError::PlutusVersionMismatch)?
                    }
                }
            }
            MultiEraScriptPurpose::Certifying(multi_era_tx_cert) => match multi_era_tx_cert {
                MultiEraTxCert::V1(dcert) => pla::v1::ScriptPurpose::Certifying(dcert),
                MultiEraTxCert::V3(_, _) => Err(TransactionConversionError::PlutusVersionMismatch)?,
            },
            MultiEraScriptPurpose::Voting(_) => {
                Err(TransactionConversionError::PlutusVersionMismatch)?
            }
            MultiEraScriptPurpose::Proposing(_, _) => {
                Err(TransactionConversionError::PlutusVersionMismatch)?
            }
        })
    }
}

impl TryFrom<MultiEraScriptPurpose> for pla::v3::ScriptPurpose {
    type Error = TransactionConversionError;

    fn try_from(value: MultiEraScriptPurpose) -> Result<Self, Self::Error> {
        Ok(match value {
            MultiEraScriptPurpose::Minting(currency_symbol) => {
                pla::v3::ScriptPurpose::Minting(currency_symbol)
            }
            MultiEraScriptPurpose::Spending(transaction_input) => {
                pla::v3::ScriptPurpose::Spending(transaction_input)
            }
            MultiEraScriptPurpose::Rewarding(multi_era_rewarding_credential) => {
                match multi_era_rewarding_credential {
                    MultiEraRewardingCredential::V1(_) => {
                        Err(TransactionConversionError::PlutusVersionMismatch)?
                    }
                    MultiEraRewardingCredential::V3(credential) => {
                        pla::v3::ScriptPurpose::Rewarding(credential)
                    }
                }
            }
            MultiEraScriptPurpose::Certifying(multi_era_tx_cert) => match multi_era_tx_cert {
                MultiEraTxCert::V1(_) => Err(TransactionConversionError::PlutusVersionMismatch)?,
                MultiEraTxCert::V3(cert_idx, tx_cert) => {
                    pla::v3::ScriptPurpose::Certifying(cert_idx, tx_cert)
                }
            },
            MultiEraScriptPurpose::Voting(voter) => pla::v3::ScriptPurpose::Voting(voter),
            MultiEraScriptPurpose::Proposing(proposal_idx, proposal_procedure) => {
                pla::v3::ScriptPurpose::Proposing(proposal_idx, proposal_procedure)
            }
        })
    }
}

// TransactionOutput

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MultiEraTransactionOutput {
    pub address: v3::MultiEraAddress,
    pub value: pla::v3::Value,
    pub datum: pla::v3::OutputDatum,
    pub reference_script: Option<v3::Script>,
    pub plutus_version: PlutusVersion,
}

impl From<v1::TransactionOutput> for MultiEraTransactionOutput {
    fn from(value: v1::TransactionOutput) -> Self {
        Self {
            address: value.address,
            value: value.value,
            datum: match value.datum_hash {
                None => pla::v3::OutputDatum::None,
                Some(datum_hash) => pla::v3::OutputDatum::DatumHash(datum_hash),
            },
            reference_script: None,
            plutus_version: PlutusVersion::V1,
        }
    }
}

// V2 and V3 are identical
impl From<v2::TransactionOutput> for MultiEraTransactionOutput {
    fn from(value: v2::TransactionOutput) -> Self {
        Self {
            address: value.address,
            value: value.value,
            datum: value.datum,
            reference_script: value.reference_script,
            plutus_version: PlutusVersion::V2,
        }
    }
}

impl TryFrom<MultiEraTransactionOutput> for v1::TransactionOutput {
    type Error = TransactionConversionError;

    fn try_from(value: MultiEraTransactionOutput) -> Result<Self, Self::Error> {
        Ok(Self {
            address: value.address,
            value: value.value,
            datum_hash: match value.datum {
                pla::v3::OutputDatum::None => None,
                pla::v3::OutputDatum::DatumHash(datum_hash) => Some(datum_hash),
                pla::v3::OutputDatum::InlineDatum(_) => {
                    Err(TransactionConversionError::PlutusVersionMismatch)?
                }
            },
        })
    }
}

// V2 and V3 are identical
impl TryFrom<MultiEraTransactionOutput> for v2::TransactionOutput {
    type Error = TransactionConversionError;

    fn try_from(value: MultiEraTransactionOutput) -> Result<Self, Self::Error> {
        Ok(Self {
            address: value.address,
            value: value.value,
            datum: value.datum,
            reference_script: value.reference_script,
        })
    }
}
