use anyhow::anyhow;
use cardano_serialization_lib as csl;
use cardano_serialization_lib::plutus::{ConstrPlutusData, ExUnits};
use cardano_serialization_lib::utils::{from_bignum, to_bignum, Int};
use num_bigint::BigInt;
use plutus_ledger_api as pla;
use plutus_ledger_api::plutus_data::IsPlutusData;
use std::collections::BTreeMap;
use std::str::FromStr;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, CSLConversionError>;

#[derive(Error, Debug)]
pub enum CSLConversionError {
    #[error("Couldn't convert a {label} from cardano-serialization-lib format: {source}")]
    FromCSLError {
        label: String,
        source: anyhow::Error,
    },
    #[error("Couldn't convert a {label} to cardano-serialization-lib format: {source}")]
    ToCSLError {
        label: String,
        source: anyhow::Error,
    },
    #[error("Unsupported {0}.")]
    Unsupported(String),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum RedeemerTag {
    Spend,
    Certificate,
    Mint,
    Withdrawal,
}

pub fn to_redeemer_tag(str: &str) -> Result<csl::plutus::RedeemerTag> {
    match str {
        "spend" => Ok(csl::plutus::RedeemerTag::new_spend()),
        "certificate" => Ok(csl::plutus::RedeemerTag::new_cert()),
        "mint" => Ok(csl::plutus::RedeemerTag::new_mint()),
        "withdrawal" => Ok(csl::plutus::RedeemerTag::new_reward()),
        _ => Err(CSLConversionError::ToCSLError {
            label: "RedeemerTag".to_string(),
            source: anyhow!("Invalid RedeemerTag"),
        }),
    }
}

pub fn to_address(
    pla_address: &pla::v2::address::Address,
    network_id: u8,
) -> Result<csl::address::Address> {
    match pla_address {
        pla::v2::address::Address {
            credential,
            staking_credential: None,
        } => Ok(csl::address::EnterpriseAddress::new(
            network_id,
            &to_stake_credential(credential)?,
        )
        .to_address()),

        pla::v2::address::Address {
            credential,
            staking_credential: Some(pla::v2::address::StakingCredential::Hash(staking_credential)),
        } => Ok(csl::address::BaseAddress::new(
            network_id,
            &to_stake_credential(credential)?,
            &to_stake_credential(staking_credential)?,
        )
        .to_address()),

        _ => Err(CSLConversionError::Unsupported("address type".to_string())),
    }
}

pub fn from_address(csl_address: &csl::address::Address) -> Result<pla::v2::address::Address> {
    csl::address::EnterpriseAddress::from_address(csl_address)
        .map(|enterprise_addr| pla::v2::address::Address {
            credential: from_stake_credential(&enterprise_addr.payment_cred()),
            staking_credential: None,
        })
        .or_else(|| {
            csl::address::BaseAddress::from_address(csl_address).map(|base_addr| {
                pla::v2::address::Address {
                    credential: from_stake_credential(&base_addr.payment_cred()),
                    staking_credential: Some(pla::v2::address::StakingCredential::Hash(
                        from_stake_credential(&base_addr.stake_cred()),
                    )),
                }
            })
        })
        .ok_or(CSLConversionError::Unsupported("address type".to_string()))
}

pub fn from_stake_credential(
    csl_stake_credential: &csl::address::StakeCredential,
) -> pla::v2::address::Credential {
    match (
        csl_stake_credential.to_keyhash(),
        csl_stake_credential.to_scripthash(),
    ) {
        (Some(key_hash), None) => pla::v2::address::Credential::PubKey(from_ed25519pkh(&key_hash)),
        (None, Some(script_hash)) => pla::v2::address::Credential::Script(
            pla::v2::script::ValidatorHash(from_script_hash(&script_hash)),
        ),
        _ => unreachable!("StakeCredential must be a key hash or script hash"),
    }
}

pub fn to_stake_credential(
    pla_credential: &pla::v2::address::Credential,
) -> Result<csl::address::StakeCredential> {
    match pla_credential {
        pla::v2::address::Credential::PubKey(pkh) => {
            let pkh = to_ed25519pkh(&pkh)?;
            Ok(csl::address::StakeCredential::from_keyhash(&pkh))
        }
        pla::v2::address::Credential::Script(pla::v2::script::ValidatorHash(
            pla::v2::script::ScriptHash(pla::v2::crypto::LedgerBytes(script_hash)),
        )) => {
            let script_hash =
                csl::crypto::ScriptHash::from_bytes(script_hash.to_owned()).map_err(|source| {
                    CSLConversionError::ToCSLError {
                        label: "ScriptHash bytes".to_string(),
                        source: anyhow!(source),
                    }
                })?;
            Ok(csl::address::StakeCredential::from_scripthash(&script_hash))
        }
    }
}

pub fn to_plutus_data(
    pla_plutus_data: &pla::plutus_data::PlutusData,
) -> Result<csl::plutus::PlutusData> {
    match pla_plutus_data {
        pla::plutus_data::PlutusData::Constr(i, d) => Ok(
            csl::plutus::PlutusData::new_constr_plutus_data(&ConstrPlutusData::new(
                &to_bignum(
                    u64::try_from(i).map_err(|source| CSLConversionError::ToCSLError {
                        label: "PlutusData Constr index".to_string(),
                        source: anyhow!(source),
                    })?,
                ),
                &csl::plutus::PlutusList::from(
                    d.into_iter()
                        .map(to_plutus_data)
                        .collect::<Result<Vec<_>>>()?,
                ),
            )),
        ),

        pla::plutus_data::PlutusData::Map(m) => {
            let mut pmap = csl::plutus::PlutusMap::new();

            m.into_iter()
                .map(|(k, v)| {
                    pmap.insert(&to_plutus_data(k)?, &to_plutus_data(v)?);
                    Ok(())
                })
                .collect::<Result<()>>()?;
            Ok(csl::plutus::PlutusData::new_map(&pmap))
        }
        pla::plutus_data::PlutusData::List(l) => Ok(csl::plutus::PlutusData::new_list(
            &csl::plutus::PlutusList::from(
                l.into_iter()
                    .map(to_plutus_data)
                    .collect::<Result<Vec<_>>>()?,
            ),
        )),
        pla::plutus_data::PlutusData::Integer(i) => Ok(csl::plutus::PlutusData::new_integer(
            &cardano_serialization_lib::utils::BigInt::from(i.clone()),
        )),
        pla::plutus_data::PlutusData::Bytes(b) => Ok(csl::plutus::PlutusData::new_bytes(b.clone())),
    }
}

pub fn from_plutus_list(
    csl_plutus_list: &csl::plutus::PlutusList,
) -> Result<Vec<pla::plutus_data::PlutusData>> {
    let mut data_vec = Vec::with_capacity(csl_plutus_list.len());

    for i in 0..csl_plutus_list.len() {
        data_vec.push(from_plutus_data(&csl_plutus_list.get(i))?)
    }

    Ok(data_vec)
}

pub fn from_plutus_map(
    csl_plutus_map: &csl::plutus::PlutusMap,
) -> Result<Vec<(pla::plutus_data::PlutusData, pla::plutus_data::PlutusData)>> {
    let mut data_vec = Vec::with_capacity(csl_plutus_map.len());
    let keys = csl_plutus_map.keys();

    for i in 0..keys.len() {
        let key = keys.get(i);
        data_vec.push((
            from_plutus_data(&key)?,
            from_plutus_data(&csl_plutus_map.get(&key).unwrap(/* unreachable */).clone())?,
        ));
    }

    Ok(data_vec)
}

pub fn from_plutus_data(
    csl_plutus_data: &csl::plutus::PlutusData,
) -> Result<pla::plutus_data::PlutusData> {
    csl_plutus_data
        .as_constr_plutus_data()
        .map(|cpd| {
            Ok(pla::plutus_data::PlutusData::Constr(
                BigInt::from(from_bignum(&cpd.alternative())),
                from_plutus_list(&cpd.data())?,
            ))
        })
        .unwrap_or_else(|| {
            csl_plutus_data
                .as_map()
                .map(|map| Ok(pla::plutus_data::PlutusData::Map(from_plutus_map(&map)?)))
                .unwrap_or_else(|| {
                    csl_plutus_data
                        .as_list()
                        .map(|lst| Ok(pla::plutus_data::PlutusData::List(from_plutus_list(&lst)?)))
                        .unwrap_or_else(|| {
                            csl_plutus_data
                                .as_integer()
                                .map(|int| {
                                    Ok(pla::plutus_data::PlutusData::Integer(
                                        num_bigint::BigInt::from_str(&int.to_str()).map_err(
                                            |source| CSLConversionError::FromCSLError {
                                                label: "PlutusData Integer".to_string(),
                                                source: anyhow!(source),
                                            },
                                        )?,
                                    ))
                                })
                                .unwrap_or_else(|| {
                                    csl_plutus_data
                                        .as_bytes()
                                        .map(|bytes| Ok(pla::plutus_data::PlutusData::Bytes(bytes)))
                                        .unwrap(/* unreachable */)
                                })
                        })
                })
        })
}

pub fn to_transaction_input(
    pla_transaction_input: &pla::v2::transaction::TransactionInput,
) -> Result<csl::TransactionInput> {
    Ok(csl::TransactionInput::new(
        &csl::crypto::TransactionHash::from_bytes(
            pla_transaction_input.transaction_id.0 .0.clone(),
        )
        .map_err(|source| CSLConversionError::ToCSLError {
            label: "TransactionHash".to_string(),
            source: anyhow!(source),
        })?,
        pla_transaction_input.index.clone().try_into().map_err(
            |source: num_bigint::TryFromBigIntError<_>| CSLConversionError::ToCSLError {
                label: "Transaction Index".to_string(),
                source: anyhow!(source),
            },
        )?,
    ))
}

pub fn from_transaction_input(
    csl_transaction_input: &csl::TransactionInput,
) -> pla::v2::transaction::TransactionInput {
    let transaction_id = pla::v2::transaction::TransactionHash(pla::v2::crypto::LedgerBytes(
        csl_transaction_input.transaction_id().to_bytes(),
    ));
    let index = csl_transaction_input.index().into();

    pla::v2::transaction::TransactionInput {
        transaction_id,
        index,
    }
}

pub fn to_output_datum(
    pla_output_datum: &pla::v2::datum::OutputDatum,
) -> Result<Option<csl::OutputDatum>> {
    Ok(match pla_output_datum {
        pla::v2::datum::OutputDatum::None => None,
        pla::v2::datum::OutputDatum::InlineDatum(pla::v2::datum::Datum(d)) => {
            Some(csl::OutputDatum::new_data(&to_plutus_data(d)?))
        }
        pla::v2::datum::OutputDatum::DatumHash(dh) => {
            Some(csl::OutputDatum::new_data_hash(&to_data_hash(dh)?))
        }
    })
}

pub fn to_transaction_output(
    pla_transaction_output: &pla::v2::transaction::TransactionOutput,
    minting_policies: &BTreeMap<
        pla::v2::script::MintingPolicyHash,
        crate::utils::script::ScriptOrRef,
    >,
    validators: &BTreeMap<pla::v2::script::ValidatorHash, crate::utils::script::ScriptOrRef>,
    network_id: u8,
    data_cost: &csl::DataCost,
) -> Result<csl::TransactionOutput> {
    let mut output_builder = csl::output_builder::TransactionOutputBuilder::new()
        .with_address(&to_address(&pla_transaction_output.address, network_id)?);

    output_builder = match &pla_transaction_output.datum {
        pla::v2::datum::OutputDatum::None => output_builder,
        pla::v2::datum::OutputDatum::InlineDatum(pla::v2::datum::Datum(d)) => {
            output_builder.with_plutus_data(&to_plutus_data(&d)?)
        }
        pla::v2::datum::OutputDatum::DatumHash(dh) => {
            output_builder.with_data_hash(&to_data_hash(dh)?)
        }
    };

    let script_ref = pla_transaction_output
        .reference_script
        .clone()
        .map(|script_hash| -> Result<_> {
            let script_or_ref = minting_policies
                .get(&pla::v2::script::MintingPolicyHash(script_hash.clone()))
                .or_else(|| validators.get(&pla::v2::script::ValidatorHash(script_hash.clone())))
                .ok_or(CSLConversionError::ToCSLError {
                    label: "MissingScript".to_string(),
                    source: anyhow!("Missing script (hash: {:?})", script_hash),
                })?;
            Ok(match script_or_ref {
                crate::utils::script::ScriptOrRef::RefScript(_, script, _) => {
                    csl::ScriptRef::new_plutus_script(&script)
                }
                crate::utils::script::ScriptOrRef::PlutusScript(script, _) => {
                    csl::ScriptRef::new_plutus_script(&script)
                }
            })
        })
        .transpose()?;

    if let Some(script_ref) = &script_ref {
        output_builder = output_builder.with_script_ref(&script_ref);
    };

    output_builder
        .next()
        .map_err(|err| CSLConversionError::ToCSLError {
            label: "TransactionOutput".to_string(),
            source: anyhow!(err),
        })?
        .with_value(&to_value_with_min_utxo(
            &pla_transaction_output.value,
            &pla_transaction_output.datum,
            &script_ref,
            &data_cost,
        )?)
        .build()
        .map_err(|err| CSLConversionError::ToCSLError {
            label: "TransactionOutput".to_string(),
            source: anyhow!(err),
        })
}

pub fn to_ed25519pkh(
    pla_pkh: &pla::v2::crypto::Ed25519PubKeyHash,
) -> Result<csl::crypto::Ed25519KeyHash> {
    let pla::v2::crypto::Ed25519PubKeyHash(pla::v2::crypto::LedgerBytes(bytes)) = pla_pkh;
    csl::crypto::Ed25519KeyHash::from_bytes(bytes.clone()).map_err(|source| {
        CSLConversionError::ToCSLError {
            label: "Ed25519KeyHash".to_string(),
            source: anyhow!(source),
        }
    })
}

pub fn from_ed25519pkh(
    csl_pkh: &csl::crypto::Ed25519KeyHash,
) -> pla::v2::crypto::Ed25519PubKeyHash {
    pla::v2::crypto::Ed25519PubKeyHash(pla::v2::crypto::LedgerBytes(csl_pkh.to_bytes()))
}

pub fn to_value_with_min_utxo(
    pla_value: &pla::v2::value::Value,
    datum: &pla::v2::datum::OutputDatum,
    script_ref: &Option<csl::ScriptRef>,
    data_cost: &csl::DataCost,
) -> Result<csl::utils::Value> {
    let value = to_value(&pla_value)?;

    let mut calc = csl::utils::MinOutputAdaCalculator::new_empty(data_cost).map_err(|source| {
        CSLConversionError::ToCSLError {
            label: "Value with minUtxo".to_string(),
            source: anyhow!(source),
        }
    })?;
    calc.set_amount(&value);
    match datum {
        pla::v2::datum::OutputDatum::None => {}
        pla::v2::datum::OutputDatum::InlineDatum(pla::v2::datum::Datum(d)) => {
            calc.set_plutus_data(&to_plutus_data(&d)?);
        }
        pla::v2::datum::OutputDatum::DatumHash(dh) => {
            calc.set_data_hash(&to_data_hash(dh)?);
        }
    };
    if let Some(script_ref) = script_ref {
        calc.set_script_ref(script_ref);
    }

    let required_coin = calc
        .calculate_ada()
        .map_err(|source| CSLConversionError::ToCSLError {
            label: "Value with minUtxo".to_string(),
            source: anyhow!(source),
        })?;
    let coin = std::cmp::max(value.coin(), required_coin);

    Ok(match value.multiasset() {
        Some(multiasset) => csl::utils::Value::new_with_assets(&coin, &multiasset),
        None => csl::utils::Value::new(&coin),
    })
}

pub fn to_coin(pla_value: &pla::v2::value::Value) -> Result<csl::utils::Coin> {
    let pla::v2::value::Value(value) = pla_value;
    value
        .get(&pla::v2::value::CurrencySymbol::Ada)
        .and_then(|assets| {
            assets.get(&pla::v2::value::TokenName(pla::v2::crypto::LedgerBytes(
                Vec::new(),
            )))
        })
        .map_or(Ok(to_bignum(0)), |big_int| {
            Ok(to_bignum(big_int.try_into().map_err(
                |source: num_bigint::TryFromBigIntError<_>| CSLConversionError::ToCSLError {
                    label: "Coin".to_string(),
                    source: anyhow!(source),
                },
            )?))
        })
}

pub fn to_multiasset(pla_value: &pla::v2::value::Value) -> Result<csl::MultiAsset> {
    let pla::v2::value::Value(value) = pla_value;
    let mut multiasset = csl::MultiAsset::new();

    value
        .iter()
        .map(|(cur_sym, pla_assets)| match cur_sym {
            pla::v2::value::CurrencySymbol::Ada => Ok(()),
            pla::v2::value::CurrencySymbol::NativeToken(pla::v2::script::MintingPolicyHash(
                script_hash,
            )) => {
                let policy_id = to_script_hash(script_hash)?;
                let mut assets = csl::Assets::new();
                pla_assets
                    .iter()
                    .map(|(token_name, amount)| {
                        let asset_name = to_asset_name(&token_name)?;
                        let amount = to_bignum(amount.try_into().map_err(
                            |source: num_bigint::TryFromBigIntError<_>| {
                                CSLConversionError::ToCSLError {
                                    label: "MultiAsset amount".to_string(),
                                    source: anyhow!(source),
                                }
                            },
                        )?);

                        assets.insert(&asset_name, &amount);
                        Ok(())
                    })
                    .collect::<Result<()>>()?;

                multiasset.insert(&policy_id, &assets);
                Ok(())
            }
        })
        .collect::<Result<()>>()?;

    Ok(multiasset)
}

pub fn to_value(pla_value: &pla::v2::value::Value) -> Result<csl::utils::Value> {
    let coin = to_coin(&pla_value)?;
    let multiasset = to_multiasset(&pla_value)?;

    Ok(csl::utils::Value::new_with_assets(&coin, &multiasset))
}

pub fn to_policy_id(pla_currency_symbol: &pla::v2::value::CurrencySymbol) -> Result<csl::PolicyID> {
    let empty_bytes = pla::v2::script::ScriptHash(pla::v2::crypto::LedgerBytes(Vec::new()));
    let bytes = match pla_currency_symbol {
        pla::v2::value::CurrencySymbol::Ada => &empty_bytes,
        pla::v2::value::CurrencySymbol::NativeToken(pla::v2::script::MintingPolicyHash(
            script_hash,
        )) => script_hash,
    };

    to_script_hash(&bytes)
}

pub fn to_asset_name(pla_token_name: &pla::v2::value::TokenName) -> Result<csl::AssetName> {
    let pla::v2::value::TokenName(pla::v2::crypto::LedgerBytes(bytes)) = pla_token_name;
    let plutus_data = to_plutus_data(&bytes.to_plutus_data())?;
    Ok(
        csl::AssetName::from_bytes(plutus_data.to_bytes()).map_err(|source| {
            CSLConversionError::ToCSLError {
                label: "AssetName".to_string(),
                source: anyhow!(source),
            }
        })?,
    )
}

pub fn from_script_hash(csl_script_hash: &csl::crypto::ScriptHash) -> pla::v2::script::ScriptHash {
    pla::v2::script::ScriptHash(pla::v2::crypto::LedgerBytes(csl_script_hash.to_bytes()))
}

pub fn to_script_hash(
    pla_script_hash: &pla::v2::script::ScriptHash,
) -> Result<csl::crypto::ScriptHash> {
    let pla::v2::script::ScriptHash(pla::v2::crypto::LedgerBytes(bytes)) = pla_script_hash;
    csl::crypto::ScriptHash::from_bytes(bytes.to_owned()).map_err(|source| {
        CSLConversionError::ToCSLError {
            label: "ScriptHash".to_string(),
            source: anyhow!(source),
        }
    })
}

pub fn to_redeemer(
    pla_redeemer: &pla::v2::redeemer::Redeemer,
    red_tag: &csl::plutus::RedeemerTag,
    red_idx: u64,
) -> Result<csl::plutus::Redeemer> {
    let pla::v2::redeemer::Redeemer(plutus_data) = pla_redeemer;
    Ok(csl::plutus::Redeemer::new(
        &red_tag,
        &to_bignum(red_idx),
        &to_plutus_data(plutus_data)?,
        &ExUnits::new(&to_bignum(0), &to_bignum(0)),
    ))
}

pub fn to_data_hash(pla_datum_hash: &pla::v2::datum::DatumHash) -> Result<csl::crypto::DataHash> {
    let pla::v2::datum::DatumHash(pla::v2::crypto::LedgerBytes(bytes)) = pla_datum_hash;
    csl::crypto::DataHash::from_bytes(bytes.clone()).map_err(|source| {
        CSLConversionError::ToCSLError {
            label: "DataHash".to_string(),
            source: anyhow!(source),
        }
    })
}

pub fn from_data_hash(csl_datum_hash: &csl::crypto::DataHash) -> pla::v2::datum::DatumHash {
    let bytes = csl_datum_hash.to_bytes();
    pla::v2::datum::DatumHash(pla::v2::crypto::LedgerBytes(bytes))
}

pub fn to_int(int: i64) -> Int {
    if int >= 0 {
        Int::new(&to_bignum(int as u64))
    } else {
        Int::new_negative(&to_bignum((int * -1) as u64))
    }
}

pub fn to_language(pla_lang: &crate::utils::script::PlutusVersion) -> csl::plutus::Language {
    match pla_lang {
        crate::utils::script::PlutusVersion::V1 => csl::plutus::Language::new_plutus_v1(),
        crate::utils::script::PlutusVersion::V2 => csl::plutus::Language::new_plutus_v2(),
    }
}

#[cfg(test)]
mod roundtrip_tests {
    use super::*;
    use plutus_ledger_api::generators::correct::v1::*;
    use proptest::prelude::*;

    proptest! {
            // TODO: BigInt breaks CSL
            // #[test]
            // fn test_plutus_data(val in arb_plutus_data()) {
            //     assert_eq!(val, from_plutus_data(&to_plutus_data(&val)));
            // }


            // TODO: Implement all address types
            // #[test]
            // fn test_address(val in arb_address()) {
            //     assert_eq!(val, from_address(&to_address(&val, 1).unwrap()));
            // }
            //

             // #[test]
             // fn test_transaction_input(val in arb_transaction_input()) {
             //     assert_eq!(val, from_transaction_input(&to_transaction_input(&val).unwrap()));
             // }

             #[test]
             fn test_ed25519_pub_key_hash(val in arb_ed25519_pub_key_hash()) {
                 assert_eq!(val, from_ed25519pkh(&to_ed25519pkh(&val)?));
             }

             #[test]
             fn test_script_hash(val in arb_script_hash()) {
                 assert_eq!(val, from_script_hash(&to_script_hash(&val)?));
             }

             #[test]
             fn test_data_hash(val in arb_datum_hash()) {
                 assert_eq!(val, from_data_hash(&to_data_hash(&val)?));
             }
    }
}
