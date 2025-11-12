# Transaction Indexer

Cardano chain indexer builder framework

This is framework to build domain-specific indexing solutions, handling
Cardano transactions as events and persisting the to a PostgreSQL database.

Warning: tx-indexer is still under development and not production ready.

## Domain-specific

Although it's possible to implement general-use indexer, we have seen that
the performance of such solutions is not desirable for most cases. Our solution
allows to build a database schema with the domain requirements in the focus:

- only store what you need in the format you need
- index, structure however you want

## Event-based

We capture blocks convert them into plutus-ledger-api types so it's convenient
to work with, and emit them as events.

## Features

- Plutus ledger types for PostgreSQL
- sqlx bindings for the above types
- retry strategies based on error type
- sync progress tracking
- transaction events as plutus-ledger-api types
- rollback handling
