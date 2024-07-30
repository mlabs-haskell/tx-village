# Transaction Bakery

This is a light-weight Cardano transaction builder written in Rust.

Our main motivation was to replace the monolithic off-chain tools that are
common in our stacks with modular components, that can be mixed and matched for
your projects specific needs.

## How to use

Transaction building can be separated into 3 stages:

1. TransactionInfo building (kneading)
2. Transaction building (baking)
3. Submitting (deliver)

The bakery has tools to do all the 3, but you can also easily delegate some
stages to other components: for example you could build your TransactionInfo
in Haskell, and let the last 2 stages be handled by the bakery.

### 1. TransactionInfo building

In this step we compose a transaction without having to deal with network
related data, costs and fees, etc. The `TxScaffold` will provide a way to
build a simplified model of a transaction.

For example, a claim transaction from a validator address would look like
the following:

```rust
use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer};
use num_bigint::BigInt;
use plutus_ledger_api::{
    plutus_data::IsPlutusData,
    v2::{
        datum::{Datum, OutputDatum},
        redeemer::Redeemer,
        transaction::{TransactionInfo, TransactionInput},
    },
};
use std::collections::BTreeMap;
use tx_bakery::{
    chain_query::{ChainQuery, FullTransactionOutput},
    error::Result,
    submitter::Submitter,
    tx_info_builder::TxScaffold,
    utils::script::ScriptOrRef,
    wallet::Wallet,
};

pub fn mk_tx_info(
    own_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
    eq_validator_utxos: &BTreeMap<TransactionInput, FullTransactionOutput>,
    eq_redeemer: &EqRedeemer,
    eq_datum: &EqDatum,
) -> TransactionInfo {
    // Find fee input UTxO: in this case, pick a random UTxO with more than 5 Ada
    let fee_input = own_utxos
        .iter()
        .find(|(_tx_in, tx_out)| tx_out.value.get_ada_amount() >= BigInt::from(5_000_000))
        .expect("Cannot find spendable input UTxO.");

    // Finding the locked UTxO with the correct inline datum
    let tx_input = eq_validator_utxos
        .iter()
        .find(|(_, tx_out)| {
            if let OutputDatum::InlineDatum(Datum(inline_datum)) = &tx_out.datum
            {
                EqDatum::from_plutus_data(&inline_datum).unwrap() == *eq_datum
            } else {
                false
            }
        })
        .expect("UTxO with inline datum not found");

    // Converting redeemer to PlutusData
    let redeemer = Redeemer(eq_redeemer.to_plutus_data());

    TxScaffold::new()
        // Adding fee input from our pub key address
        .add_pub_key_input(fee_input.0.clone(), fee_input.1.into())
        // Input from the validator
        .add_script_input(tx_input.0.clone(), tx_input.1.into(), None, redeemer)
        // Build a TransactionInfo
        .build()
}
```

Note that we didn't even add an output to our transaction. This is OK, as in the
next step, we're going to use `ChangeStrategy::Address` which instructs the
bakery to send the change to a specified address.

We also don't have to worry about the fees, or the minimum UTxO deposits on
outputs. Those will also be sorted out in the baking stage.

Also note that this function is pure, all side-effects were pushed out from the
core component.

### 2. Transaction building, signing and submitting

Transaction building will convert our TransactionInfo into a valid, CBOR
serialized unsigned transaction.

```rust
use lbf_demo_plutus_api::demo::plutus::{EqDatum, EqRedeemer};
use plutus_ledger_api::{
v2::{
address::{Address, Credential},
        script::ValidatorHash,
        transaction::{TransactionHash},
    },
};
use std::collections::BTreeMap;
use tx_bakery::{
    chain_query::{ChainQuery, FullTransactionOutput},
    error::Result,
    submitter::Submitter,
    utils::script::ScriptOrRef,
    wallet::Wallet,
    ChangeStrategy, CollateralStrategy, TxBakery, TxWithCtx,
};

pub async fn build_and_submit(
    wallet: &impl Wallet,
    chain_query: &impl ChainQuery,
    submitter: &impl Submitter,
    eq_validator: (ValidatorHash, ScriptOrRef),
    eq_redeemer: &EqRedeemer,
    eq_datum: &EqDatum,
) -> Result<TransactionHash> {
    // Converting the validator hash into an address
    let eq_validator_addr = Address {
        credential: Credential::Script(eq_validator.0.clone()),
        staking_credential: None,
    };

    // Fetching the UTxOs from the chain query client at our address
    let own_utxos = chain_query
        .query_utxos_by_addr(&wallet.get_change_addr())
        .await?;

    // Fetching the UTxOs from the chain query client at the validator address
    let eq_validator_utxos = chain_query.query_utxos_by_addr(&eq_validator_addr).await?;

    // Calling our previously implemented TxInfo builder
    // Alternatively we could call out to an external service to get the TxInfo
    let tx_info = mk_tx_info(&own_utxos, &eq_validator_utxos, eq_redeemer, eq_datum);

    // Creating a map of all the scripts used in the transaction (unused scripts
    // won't be attached)
    let scripts = BTreeMap::from([eq_validator.1.with_script_hash()]);

    // Define the strategy to find a suitable collateral
    let collateral = CollateralStrategy::Automatic;

    // Initialise TxBakery by fetching protocol parameters from the ChainQuery
    let tx_bakery = TxBakery::init(chain_query).await?;

    // Define the strategy to handle change outpu
    let change_strategy = ChangeStrategy::Address(wallet.get_change_addr());

    // Transaction with context will attach required scripts, collateral, etc.
    let tx = TxWithCtx::new(&tx_info, &scripts, &collateral, &change_strategy);

    // Bake, sign and submit the transaction
    //
    // You can also get the raw Cardano Serialization Lib transaction by using
    // `bake_balanced_tx`, which returns a `Result<csl::Transaction>`
    // This also allows to multi-sign a transaction with a Wallet 
    // `sign_transaction(&self, tx: &csl::Transaction) -> csl::Transaction`
    tx_bakery.bake_and_deliver(submitter, wallet, tx).await
}
```

The rest of the implementation is about wiring up the actual services,
for example Ogmios as the ChainQuery and Submitter, and a KeyWallet.

To see a full example implementation, please visit
[lambda-buffers-for-cardano](https://github.com/mlabs-haskell/lambda-buffers-for-cardano/tree/main/transactions/demo-rust)
