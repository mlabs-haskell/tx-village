use cardano_serialization_lib as csl;
use oura::filters::selection::{Config, Predicate};
use plutus_ledger_api::v2::{script::MintingPolicyHash, value::CurrencySymbol};
use tx_bakery::utils::pla_to_csl::TryFromPLAWithDef;

/// Interesting transaction components to look for when filtering transactions relevant to the protocol.
pub struct Filter {
    pub curr_symbols: Vec<CurrencySymbol>,
}

impl Filter {
    pub fn to_selection_config(self) -> Config {
        Config {
            check: Predicate::AnyOf(
                self.curr_symbols
                    .into_iter()
                    .map(|cur_sym| match cur_sym {
                        CurrencySymbol::Ada => Predicate::PolicyEquals(String::new()),
                        CurrencySymbol::NativeToken(MintingPolicyHash(script_hash)) => {
                            Predicate::PolicyEquals(
                                csl::crypto::ScriptHash::try_from_pla(&script_hash)
                                    .unwrap()
                                    .to_hex(),
                            )
                        }
                    })
                    .collect(),
            ),
        }
    }
}
