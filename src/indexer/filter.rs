use oura::filters::selection::{Config, Predicate};

/// Interesting transaction components to look for when filtering transactions relevant to the protocol.
pub struct Filter {
  curr_symbols: Vec<String>,
}

impl Filter {
  pub fn to_selection_config(self) -> Config {
    Config {
      check: Predicate::AnyOf(
        self
          .curr_symbols
          .into_iter()
          .map(|x| Predicate::PolicyEquals(x))
          .collect(),
      ),
    }
  }
}
