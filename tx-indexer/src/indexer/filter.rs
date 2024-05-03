use oura::filters::selection::{Config, Predicate};

/// Interesting transaction components to look for when filtering transactions relevant to the protocol.
pub struct Filter {
    pub curr_symbols: Vec<String>,
}

// We only obtain Transaction events that contain the policy in the output
// NOTE: Must enable 'include_transaction_details' in oura Mapper config.
impl Filter {
    pub fn to_selection_config(self) -> Config {
        Config {
            check: Predicate::AllOf(vec![
                Predicate::VariantIn(vec!["Transaction".to_string()]),
                Predicate::AnyOf(
                    self.curr_symbols
                        .into_iter()
                        .map(Predicate::PolicyEquals)
                        .collect(),
                ),
            ]),
        }
    }
}
