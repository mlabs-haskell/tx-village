use plutus_ledger_api::v3::{Address, OutputDatum, TransactionInfo, TransactionOutput};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use crate::{error::Error, TxWithCtx};

/// Options to deal with change outputs and collateral returns
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum ChangeStrategy {
    /// Send all change to an address
    Address(Address),
    /// Use the last output of the TransactionInfo as change output (modify it's value)
    /// Collateral returns are following the address of the last output
    LastOutput,
}

impl ChangeStrategy {
    pub fn get_change_address<'a>(
        &'a self,
        tx_info: &'a TransactionInfo,
    ) -> crate::error::Result<&'a Address> {
        Ok(match self {
            ChangeStrategy::Address(addr) => addr,
            ChangeStrategy::LastOutput => {
                &tx_info
                    .outputs
                    .last()
                    .ok_or(Error::MissingChangeOutput)?
                    .address
            }
        })
    }

    /// Filters out normal outputs, leaving the
    pub fn filter_outputs<'a>(
        &'a self,
        outputs: &'a [TransactionOutput],
    ) -> &'a [TransactionOutput] {
        match self {
            ChangeStrategy::Address(_) => outputs,
            ChangeStrategy::LastOutput => &outputs[..(outputs.len() - 1)],
        }
    }

    /// Get change address and datum (amount will be defined calculated by the TxBuilder)
    pub fn get_change<'a>(
        &'a self,
        tx: &'a TxWithCtx,
    ) -> crate::error::Result<(&'a Address, &'a OutputDatum)> {
        Ok(match self {
            ChangeStrategy::Address(address) => (address, &OutputDatum::None),
            ChangeStrategy::LastOutput => {
                let last_output = tx
                    .tx_info
                    .outputs
                    .last()
                    .ok_or(Error::MissingChangeOutput)?;

                (&last_output.address, &last_output.datum)
            }
        })
    }
}
