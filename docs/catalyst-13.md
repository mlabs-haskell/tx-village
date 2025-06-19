# Catalyst F13 report - MLabs: Tooling upgrade for Conway compatibility

## Milestone 4 - Updating TX Village to be Compatible with the Conway Era

Catalyst URL:
[https://milestones.projectcatalyst.io/projects/1300144/milestones/4](https://milestones.projectcatalyst.io/projects/1300144/milestones/4)

## Evidence of milestone completion

1. Proof of Achievement 1 : Updated tx-builder code is available in the
   tx-village repository
   [https://github.com/mlabs-haskell/tx-village](https://github.com/mlabs-haskell/tx-village)
   and a git tag is provided. In addition, we will provide test logs that
   document the execution ouf the nine test cases on a local Cardano dev
   network. This will demonstrate that Tx-bakery successfully builds and submits
   transactions via V3 TxInfo.

   - tagged version of tx-bakery v2 (compatible with V3 TransactionInfo):
     [mlabs-haskell/tx-village/tree/v2.0.0/tx-bakery](https://github.com/mlabs-haskell/tx-village/tree/v2.0.0/tx-bakery)
   - Documentation with the relevant parts:
     - `TxWithCtx` takes a `TransactionInfo` from `plutus-ledger-api::v3`:
       [https://docs.rs/tx-bakery/latest/tx_bakery/struct.TxWithCtx.html](https://docs.rs/tx-bakery/latest/tx_bakery/struct.TxWithCtx.html)
     - `TxInfoBuilder` builds V3 `TransactionInfo`:
       [https://docs.rs/tx-bakery/latest/tx_bakery/tx_info_builder/struct.TxScaffold.html#method.build](https://docs.rs/tx-bakery/latest/tx_bakery/tx_info_builder/struct.TxScaffold.html#method.build)
   - test case using reference scripts (fee calculation for transactions using
     reference scripts have changed in Conway, this required changed in the
     codebase):
     [mlabs-haskell/tx-village/blob/v2.0.0/extras/tx-bakery-testsuite/tests/tests/main.rs#L1079-L1101](https://github.com/mlabs-haskell/tx-village/blob/v2.0.0/extras/tx-bakery-testsuite/tests/tests/main.rs#L1079-L1101)
   - tx-bakery test logs (running devnet with
     [cardano-node 10.1.4](https://github.com/mlabs-haskell/tx-village/blob/v2.0.0/flake.nix#L31)):
     [https://hercules-ci.com/accounts/github/mlabs-haskell/derivations/%2Fnix%2Fstore%2Fpfbz1s4h8v6w4aa35qpxphkmnl4gv0d8-tx-bakery-testsuite-check.drv/log?via-job=87e12c47-dc76-4220-b088-811b00db084e](https://hercules-ci.com/accounts/github/mlabs-haskell/derivations/%2Fnix%2Fstore%2Fpfbz1s4h8v6w4aa35qpxphkmnl4gv0d8-tx-bakery-testsuite-check.drv/log?via-job=87e12c47-dc76-4220-b088-811b00db084e)

2. Proof of Achievement 2: Updated tx-builder code is available in the
   tx-village repository
   [https://github.com/mlabs-haskell/tx-village](https://github.com/mlabs-haskell/tx-village)
   and a git tag is provided. Test logs will prove that transactions can be
   built and submitted that use Plutus V2 and V3 scripts.

   - tagged version of tx-bakery v2 (compatible with Plutus v2 and V3 scripts):
     [mlabs-haskell/tx-village/tree/v2.0.0/tx-bakery](https://github.com/mlabs-haskell/tx-village/tree/v2.0.0/tx-bakery)
   - Test case using a V2 script:
     - Plutarch script:
       [mlabs-haskell/tx-village/blob/v2.0.0/extras/tx-bakery-testsuite/validation/src/Demo/ValidationV2.hs](https://github.com/mlabs-haskell/tx-village/blob/v2.0.0/extras/tx-bakery-testsuite/validation/src/Demo/ValidationV2.hs)
     - Tx-bakery test case (see line 928 for the script import):
       [mlabs-haskell/tx-village/blob/v2.0.0/extras/tx-bakery-testsuite/tests/tests/main.rs#L925-L985](https://github.com/mlabs-haskell/tx-village/blob/v2.0.0/extras/tx-bakery-testsuite/tests/tests/main.rs#L925-L985)
   - Test case using a V3 script:
     - Plutarch script:
       [mlabs-haskell/tx-village/blob/v2.0.0/extras/tx-bakery-testsuite/validation/src/Demo/Validation.hs](https://github.com/mlabs-haskell/tx-village/blob/v2.0.0/extras/tx-bakery-testsuite/validation/src/Demo/Validation.hs)
     - Tx-bakery test case (see line 865 for the script import):
       [mlabs-haskell/tx-village/blob/v2.0.0/extras/tx-bakery-testsuite/tests/tests/main.rs#L863-L922](https://github.com/mlabs-haskell/tx-village/blob/v2.0.0/extras/tx-bakery-testsuite/tests/tests/main.rs#L863-L922)

3. Proof of Achievement 3: Updated ledger simulator and flake files are
   available in
   [https://github.com/mlabs-haskell/tx-village](https://github.com/mlabs-haskell/tx-village)
   and a git tag is provided.

   - Ledger simulator source code:
     [mlabs-haskell/tx-village/tree/v2.0.0/ledger-sim](https://github.com/mlabs-haskell/tx-village/tree/v2.0.0/ledger-sim)
   - Test logs:
     [https://hercules-ci.com/accounts/github/mlabs-haskell/derivations/%2Fnix%2Fstore%2F0jilp5xps1f2q8mc25dldk43klnji9pw-ledger-sim-test-ledger-sim-test-0.1.0.0-check.drv/log?via-job=87e12c47-dc76-4220-b088-811b00db084e](https://hercules-ci.com/accounts/github/mlabs-haskell/derivations/%2Fnix%2Fstore%2F0jilp5xps1f2q8mc25dldk43klnji9pw-ledger-sim-test-ledger-sim-test-0.1.0.0-check.drv/log?via-job=87e12c47-dc76-4220-b088-811b00db084e)

4. Proof of Achievement 4: An automated CI pipeline will execute a test suite
   designed to verify the parsing and processing of V3 transactions. We will
   provide the logs from the tests to confirm that all tests have passed and
   thereby the Tx-indexer performs properly parsing V3 transactions per
   Acceptance criteria #4.

   - tx-indexer testsuite logs (running devnet with
     [cardano-node 10.1.4](https://github.com/mlabs-haskell/tx-village/blob/v2.0.0/flake.nix#L31)):
     [https://hercules-ci.com/accounts/github/mlabs-haskell/derivations/%2Fnix%2Fstore%2F5ib266gcddfkwwapqvk3ryw78gapblky-tx-indexer-testsuite.drv/log?via-job=87e12c47-dc76-4220-b088-811b00db084e](https://hercules-ci.com/accounts/github/mlabs-haskell/derivations/%2Fnix%2Fstore%2F5ib266gcddfkwwapqvk3ryw78gapblky-tx-indexer-testsuite.drv/log?via-job=87e12c47-dc76-4220-b088-811b00db084e)
