use crate::utils::csl_adapter;
use anyhow::anyhow;
use cardano_serialization_lib as csl;
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub struct TransactionMetadata(pub BTreeMap<u64, Metadata>);

impl TryFrom<&TransactionMetadata> for csl::metadata::GeneralTransactionMetadata {
    type Error = csl_adapter::CSLConversionError;

    fn try_from(tx_metadata: &TransactionMetadata) -> Result<Self, Self::Error> {
        let mut csl_tx_metadata = csl::metadata::GeneralTransactionMetadata::new();

        tx_metadata
            .0
            .iter()
            .map(|(key, value)| {
                let _ = csl_tx_metadata.insert(&csl::utils::to_bignum(*key), &value.try_into()?);
                Ok(())
            })
            .collect::<Result<_, Self::Error>>()?;

        Ok(csl_tx_metadata)
    }
}

impl<const N: usize> From<[(u64, Metadata); N]> for TransactionMetadata {
    fn from(arr: [(u64, Metadata); N]) -> Self {
        TransactionMetadata(BTreeMap::from(arr))
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Metadata {
    Map(BTreeMap<Metadata, Metadata>),
    List(Vec<Metadata>),
    Int(i64),
    Bytes(Vec<u8>),
    Text(String),
}

impl TryFrom<&Metadata> for csl::metadata::TransactionMetadatum {
    type Error = csl_adapter::CSLConversionError;

    fn try_from(metadata: &Metadata) -> Result<Self, Self::Error> {
        match metadata {
            Metadata::Map(metadata_map) => {
                let mut csl_metadata_map = csl::metadata::MetadataMap::new();

                metadata_map
                    .iter()
                    .map(|(key, value)| {
                        let _ = csl_metadata_map.insert(&key.try_into()?, &value.try_into()?);
                        Ok(())
                    })
                    .collect::<Result<_, Self::Error>>()?;

                Ok(csl::metadata::TransactionMetadatum::new_map(
                    &csl_metadata_map,
                ))
            }

            Metadata::List(metadata_list) => {
                let mut csl_metadata_list = csl::metadata::MetadataList::new();

                metadata_list
                    .iter()
                    .map(|elem| {
                        let _ = csl_metadata_list.add(&elem.try_into()?);
                        Ok(())
                    })
                    .collect::<Result<_, Self::Error>>()?;

                Ok(csl::metadata::TransactionMetadatum::new_list(
                    &csl_metadata_list,
                ))
            }

            Metadata::Int(int) => Ok(csl::metadata::TransactionMetadatum::new_int(
                &csl_adapter::to_int(*int),
            )),

            Metadata::Bytes(bytes) => {
                csl::metadata::TransactionMetadatum::new_bytes(bytes.to_owned()).map_err(|source| {
                    csl_adapter::CSLConversionError::ToCSLError {
                        label: "Metadata::Bytes".to_string(),
                        source: anyhow!(source),
                    }
                })
            }

            Metadata::Text(str) => csl::metadata::TransactionMetadatum::new_text(str.to_owned())
                .map_err(|source| csl_adapter::CSLConversionError::ToCSLError {
                    label: "Metadata::Text".to_string(),
                    source: anyhow!(source),
                }),
        }
    }
}
