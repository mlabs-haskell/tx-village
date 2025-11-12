use itertools::Itertools;
use num_bigint::BigInt;
use plutus_ledger_api::v3::{Credential, TransactionInfo, TxInInfo};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::error::Error;

/// Options to deal with collateral selection
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum CollateralStrategy {
    /// Automatically pick a suitable UTxO from the transaction inputs
    Automatic {
        min_amount: u64,
        max_utxo_count: usize,
    },
    /// Explicitly set a UTxO (doesn't have to be an input UTxO)
    Explicit {
        utxos: Vec<TxInInfo>,
        min_amount: u64,
    },
    /// No collateral (for transaction without scripts)
    None,
}

impl CollateralStrategy {
    pub fn find_collaterals(
        &self,
        tx_info: &TransactionInfo,
    ) -> crate::error::Result<Vec<TxInInfo>> {
        match self {
            CollateralStrategy::Automatic { max_utxo_count, .. } => {
                self.find_from_inputs(*max_utxo_count, &tx_info.inputs)
            }
            CollateralStrategy::Explicit { utxos, .. } => Ok(utxos.to_owned()),
            CollateralStrategy::None => Ok(Vec::new()),
        }
    }

    pub fn min_collateral_amount(&self) -> u64 {
        match self {
            CollateralStrategy::Automatic { min_amount, .. } => *min_amount,
            CollateralStrategy::Explicit { min_amount, .. } => *min_amount,
            CollateralStrategy::None => 0,
        }
    }

    /// Find suitable UTxOs to be used as a collateral. Each UTxO has to be at a pub key address,
    /// and the total Ada amount of must be at least the configured collateral amount
    // TODO(szg251): we could calculate the exact minimum collateral amount using protocol params
    fn find_from_inputs(
        &self,
        max_utxo_count: usize,
        tx_inputs: &[TxInInfo],
    ) -> crate::error::Result<Vec<TxInInfo>> {
        let min_collateral_amount = BigInt::from(self.min_collateral_amount());
        let (amount, collaterals) = tx_inputs
            .iter()
            .sorted_by_key(|input| -input.output.value.get_ada_amount())
            .take(max_utxo_count)
            .fold(
                (BigInt::ZERO, Vec::new()),
                |(mut acc_amount, mut collaterals), tx_in_info| {
                    if acc_amount < min_collateral_amount {
                        if let Credential::PubKey(_) = tx_in_info.output.address.credential {
                            let ada_amount = tx_in_info.output.value.get_ada_amount();
                            acc_amount += ada_amount;
                            collaterals.push(tx_in_info.clone());
                        }
                    };

                    (acc_amount, collaterals)
                },
            );

        if amount >= min_collateral_amount {
            Ok(collaterals)
        } else {
            Err(Error::NotEnoughCollaterals {
                amount,
                required: min_collateral_amount,
                utxos: collaterals,
            })
        }
    }
}
