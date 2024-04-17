# Transaction Bakery

This is a light-weight Cardano transaction builder written in Rust.

Our main motivation was to replace the monolithic off-chain tools that are
common in our stacks with modular components, that can be mixed and matched for
your projects specific needs.

The sole purpose of this component is to convert a **TransactionInfo** into a
serializable **Cardano transaction**. We removed all tight couplings to indexing,
logging, orchestrating etc., and we don't even care how you build your
TransactionInfo.
