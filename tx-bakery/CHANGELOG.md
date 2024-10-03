# Changelog

This changelog is based on [Keep A
Changelog](https://keepachangelog.com/en/1.1.0).

## v2.0.0

This version brings partial Conway support: all previously supported features
work with a Conway node, but no new Conway feature is added. These are planned
to be rolled out in a later minor version bump.

### Added

### Changed

- Updated cardano-serialization-lib to Conway compatible 12.0.0
- Added reference script fee calculation
- Updated plutus-ledger-api to 1.0.0
- Breaking change: allowing multiple collaterals
  - allowing multiple UTxO in `CollateralStrategy::Explicit`
  - `CollateralStrategy::Automatic` will pick multiple UTxOs if necessary, `max_utxo_count`
    parameter added
  - `amount` renamed to `min_amount`

### Removed

- Plutip is discontinued and not compatible with Conway, we removed the client library

## v1.0.0

Babbage compatible first release
