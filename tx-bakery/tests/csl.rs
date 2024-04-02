#[cfg(test)]
mod csl_pla_roundtrip_tests {
    use cardano_serialization_lib as csl;
    use num_bigint::BigInt;
    use plutus_ledger_api::{
        generators::correct::v1::{
            arb_address, arb_certificate_index, arb_chain_pointer, arb_credential, arb_datum_hash,
            arb_ed25519_pub_key_hash, arb_minting_policy_hash, arb_plutus_data, arb_script_hash,
            arb_slot, arb_token_name, arb_transaction_hash, arb_transaction_index,
            arb_transaction_input, arb_value,
        },
        v2::{address::StakingCredential, value::Value},
    };
    use proptest::{prop_assert_eq, proptest, strategy::Strategy, test_runner::TestCaseError};
    use tx_bakery::utils::from_csl::FromCSL;
    use tx_bakery::utils::to_csl::{ToCSL, ToCSLWithDef};

    fn round_trip_prop<B, A: ToCSLWithDef<B> + FromCSL<B> + PartialEq + std::fmt::Debug>(
        v: A,
    ) -> Result<(), TestCaseError> {
        Ok(prop_assert_eq!(
            A::from_csl(&<A as ToCSLWithDef<B>>::to_csl(&v)?)?,
            v
        ))
    }

    fn round_trip_prop_with<B, A: ToCSL<B> + FromCSL<B> + PartialEq + std::fmt::Debug>(
        v: A,
        info: A::ExtraInfo,
    ) -> Result<(), TestCaseError> {
        Ok(prop_assert_eq!(
            A::from_csl(&<A as ToCSL<B>>::to_csl_with(&v, info)?)?,
            v
        ))
    }

    proptest! {
      #[test]
      fn test_token_name(val in arb_token_name()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_minting_policy_hash(val in arb_minting_policy_hash()) {
          round_trip_prop(val)?
      }

      // This is special because the CSL machinery always puts in at least a zero Ada in the value
      // But the arbitrary generated value by PLA does not.
      #[test]
      fn test_value(val in arb_value()) {
        prop_assert_eq!(
          Value::from_csl(&val.to_csl()?)?,
          // Add a zero ada value.
          Value::ada_value(&BigInt::from(0)) + val
        )
      }

      #[test]
      fn test_transaction_hash(val in arb_transaction_hash()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_transaction_input(val in arb_transaction_input()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_ed25519_pub_key_hash(val in arb_ed25519_pub_key_hash()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_script_hash(val in arb_script_hash()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_staking_credential(val in arb_credential().prop_map(StakingCredential::Hash)) {
          round_trip_prop::<csl::address::StakeCredential, StakingCredential>(val)?
      }

      #[test]
      fn test_credential(val in arb_credential()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_slot(val in arb_slot()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_transaction_index(val in arb_transaction_index()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_certificate_index(val in arb_certificate_index()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_chain_pointer(val in arb_chain_pointer()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_plutus_data(val in arb_plutus_data()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_datum_hash(val in arb_datum_hash()) {
          round_trip_prop(val)?
      }

      #[test]
      fn test_address(val in arb_address()) {
          round_trip_prop_with(val, 1)?
      }
    }
}
