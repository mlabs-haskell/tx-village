//! Transaction Metadata

use std::collections::BTreeMap;

use anyhow::anyhow;
use plutus_ledger_api::csl::{lib as csl, pla_to_csl::TryToCSL};

use crate::error::Error;

/// Top level transaction metadata (can only be a Map)
#[derive(Debug, Clone)]
pub struct TransactionMetadata(pub BTreeMap<u64, Metadata>);

impl TryFrom<&TransactionMetadata> for csl::GeneralTransactionMetadata {
    type Error = Error;

    fn try_from(tx_metadata: &TransactionMetadata) -> Result<Self, Self::Error> {
        let mut csl_tx_metadata = csl::GeneralTransactionMetadata::new();

        tx_metadata.0.iter().try_for_each(|(key, value)| {
            let _ = csl_tx_metadata.insert(&csl::BigNum::from(*key), &value.try_into()?);
            Ok::<(), Self::Error>(())
        })?;

        Ok(csl_tx_metadata)
    }
}

impl<const N: usize> From<[(u64, Metadata); N]> for TransactionMetadata {
    fn from(arr: [(u64, Metadata); N]) -> Self {
        TransactionMetadata(BTreeMap::from(arr))
    }
}

/// Transaction Metadata
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Metadata {
    Map(BTreeMap<Metadata, Metadata>),
    List(Vec<Metadata>),
    Int(i64),
    Bytes(Vec<u8>),
    Text(String),
}

impl TryFrom<&Metadata> for csl::TransactionMetadatum {
    type Error = Error;

    fn try_from(metadata: &Metadata) -> Result<Self, Self::Error> {
        match metadata {
            Metadata::Map(metadata_map) => {
                let mut csl_metadata_map = csl::MetadataMap::new();

                metadata_map.iter().try_for_each(|(key, value)| {
                    let _ = csl_metadata_map.insert(&key.try_into()?, &value.try_into()?);
                    Ok::<(), Self::Error>(())
                })?;

                Ok(csl::TransactionMetadatum::new_map(&csl_metadata_map))
            }

            Metadata::List(metadata_list) => {
                let mut csl_metadata_list = csl::MetadataList::new();

                metadata_list.iter().try_for_each(|elem| {
                    csl_metadata_list.add(&elem.try_into()?);
                    Ok::<(), Self::Error>(())
                })?;

                Ok(csl::TransactionMetadatum::new_list(&csl_metadata_list))
            }

            Metadata::Int(int) => Ok(csl::TransactionMetadatum::new_int(&int.try_to_csl()?)),

            Metadata::Bytes(bytes) => csl::TransactionMetadatum::new_bytes(bytes.to_owned())
                .map_err(|source| {
                    Error::ConversionError(anyhow!(
                        "Metadata::Bytes could not be converted: {}",
                        source
                    ))
                }),

            Metadata::Text(str) => {
                csl::TransactionMetadatum::new_text(str.to_owned()).map_err(|source| {
                    Error::ConversionError(anyhow!(
                        "Metadata::Text could not be converted: {}",
                        source
                    ))
                })
            }
        }
    }
}
