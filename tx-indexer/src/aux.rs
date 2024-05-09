use data_encoding::HEXLOWER;
use plutus_ledger_api::v2::{
    crypto::LedgerBytes,
    script::{MintingPolicyHash, ScriptHash},
    value::CurrencySymbol,
};
use std::str::FromStr;

#[derive(Debug, Clone)]
pub struct ParseCurrencySymbol(pub CurrencySymbol);

impl FromStr for ParseCurrencySymbol {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Ok(ParseCurrencySymbol(CurrencySymbol::NativeToken(
            MintingPolicyHash(ScriptHash(LedgerBytes(
                HEXLOWER.decode(&s.to_owned().into_bytes()).unwrap(),
            ))),
        )))
    }
}
