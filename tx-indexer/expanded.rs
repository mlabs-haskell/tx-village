#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
pub mod aux {
    use anyhow::anyhow;
    use data_encoding::HEXLOWER;
    use plutus_ledger_api::v2::{
        address::Address, crypto::LedgerBytes, script::{MintingPolicyHash, ScriptHash},
        value::CurrencySymbol,
    };
    use std::str::FromStr;
    use tx_bakery::csl;
    use tx_bakery::utils::csl_to_pla::TryToPLA;
    pub struct ParseCurrencySymbol(pub CurrencySymbol);
    #[automatically_derived]
    impl ::core::fmt::Debug for ParseCurrencySymbol {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "ParseCurrencySymbol",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ParseCurrencySymbol {
        #[inline]
        fn clone(&self) -> ParseCurrencySymbol {
            ParseCurrencySymbol(::core::clone::Clone::clone(&self.0))
        }
    }
    impl FromStr for ParseCurrencySymbol {
        type Err = &'static str;
        fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
            Ok(
                ParseCurrencySymbol(
                    CurrencySymbol::NativeToken(
                        MintingPolicyHash(
                            ScriptHash(
                                LedgerBytes(
                                    HEXLOWER.decode(&s.to_owned().into_bytes()).unwrap(),
                                ),
                            ),
                        ),
                    ),
                ),
            )
        }
    }
    pub struct ParseAddress(pub Address);
    #[automatically_derived]
    impl ::core::fmt::Debug for ParseAddress {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "ParseAddress",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ParseAddress {
        #[inline]
        fn clone(&self) -> ParseAddress {
            ParseAddress(::core::clone::Clone::clone(&self.0))
        }
    }
    impl FromStr for ParseAddress {
        type Err = anyhow::Error;
        fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
            Ok(
                ParseAddress(
                    csl::Address::from_bech32(s)
                        .map_err(|err| ::anyhow::Error::msg({
                            let res = ::alloc::fmt::format(
                                format_args!("Couldn\'t parse bech32 address: {0}", err),
                            );
                            res
                        }))?
                        .try_to_pla()
                        .map_err(|err| ::anyhow::Error::msg({
                            let res = ::alloc::fmt::format(
                                format_args!("Couldn\'t convert address: {0}", err),
                            );
                            res
                        }))?,
                ),
            )
        }
    }
}
pub mod config {
    use crate::{filter::Filter, handler::{callback::EventHandler, retry::RetryPolicy}};
    use anyhow::anyhow;
    use core::str::FromStr;
    use oura::{sources::MagicArg, utils::ChainWellKnownInfo};
    use std::error::Error;
    use std::fmt;
    use std::fs::File;
    use std::io::BufReader;
    use strum_macros::Display;
    pub struct TxIndexerConfig<H: EventHandler> {
        pub handler: H,
        pub node_address: NodeAddress,
        pub network: NetworkConfig,
        /// Slot number and hash as hex string (optional).
        /// If not provided, sync will begin from the tip of the chain.
        pub since_slot: Option<(u64, String)>,
        /// Minimum depth a block has to be from the tip for it to be considered "confirmed"
        /// See: https://oura.txpipe.io/v1/advanced/rollback_buffer
        pub safe_block_depth: usize,
        pub event_filter: Filter,
        /// Retry policy - how much to retry for each event callback failure
        /// This only takes effect on ErrorPolicy for a particular error is `Retry`.
        /// Once retries are exhausted, the handler will error (same treatment as ErrorPolicy::Exit)
        pub retry_policy: RetryPolicy,
    }
    impl<H: EventHandler> TxIndexerConfig<H> {
        #[allow(clippy::too_many_arguments)]
        pub fn new(
            handler: H,
            node_address: NodeAddress,
            network: NetworkConfig,
            since_slot: Option<(u64, String)>,
            safe_block_depth: usize,
            event_filter: Filter,
            retry_policy: RetryPolicy,
        ) -> Self {
            Self {
                handler,
                node_address,
                network,
                since_slot,
                safe_block_depth,
                event_filter,
                retry_policy,
            }
        }
    }
    /// Simple description on how to connect to a local or remote node.
    /// Used to build Oura source config.
    pub enum NodeAddress {
        /// Path to Unix node.socket
        UnixSocket(String),
        /// Hostname and port number for TCP connection to remote node
        TcpAddress(String, u16),
    }
    /// Typed network magic restricted to specific networks fully supported by Oura.
    pub enum NetworkName {
        PREPROD,
        PREVIEW,
        MAINNET,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for NetworkName {
        #[inline]
        fn clone(&self) -> NetworkName {
            match self {
                NetworkName::PREPROD => NetworkName::PREPROD,
                NetworkName::PREVIEW => NetworkName::PREVIEW,
                NetworkName::MAINNET => NetworkName::MAINNET,
            }
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for NetworkName {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    NetworkName::PREPROD => "PREPROD",
                    NetworkName::PREVIEW => "PREVIEW",
                    NetworkName::MAINNET => "MAINNET",
                },
            )
        }
    }
    impl ::core::fmt::Display for NetworkName {
        fn fmt(
            &self,
            f: &mut ::core::fmt::Formatter,
        ) -> ::core::result::Result<(), ::core::fmt::Error> {
            match *self {
                NetworkName::PREPROD => f.pad("PREPROD"),
                NetworkName::PREVIEW => f.pad("PREVIEW"),
                NetworkName::MAINNET => f.pad("MAINNET"),
            }
        }
    }
    pub enum NetworkConfig {
        ConfigPath { node_config_path: String, magic: u64 },
        WellKnown(NetworkName),
    }
    #[automatically_derived]
    impl ::core::clone::Clone for NetworkConfig {
        #[inline]
        fn clone(&self) -> NetworkConfig {
            match self {
                NetworkConfig::ConfigPath {
                    node_config_path: __self_0,
                    magic: __self_1,
                } => {
                    NetworkConfig::ConfigPath {
                        node_config_path: ::core::clone::Clone::clone(__self_0),
                        magic: ::core::clone::Clone::clone(__self_1),
                    }
                }
                NetworkConfig::WellKnown(__self_0) => {
                    NetworkConfig::WellKnown(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for NetworkConfig {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                NetworkConfig::ConfigPath {
                    node_config_path: __self_0,
                    magic: __self_1,
                } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "ConfigPath",
                        "node_config_path",
                        __self_0,
                        "magic",
                        &__self_1,
                    )
                }
                NetworkConfig::WellKnown(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "WellKnown",
                        &__self_0,
                    )
                }
            }
        }
    }
    pub struct NetworkNameParseErr;
    #[automatically_derived]
    impl ::core::clone::Clone for NetworkNameParseErr {
        #[inline]
        fn clone(&self) -> NetworkNameParseErr {
            NetworkNameParseErr
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for NetworkNameParseErr {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "NetworkNameParseErr")
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for NetworkNameParseErr {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for NetworkNameParseErr {
        #[inline]
        fn eq(&self, other: &NetworkNameParseErr) -> bool {
            true
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for NetworkNameParseErr {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    impl fmt::Display for NetworkNameParseErr {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            "provided string was not `preprod` or `preview` or `mainnet`".fmt(f)
        }
    }
    impl Error for NetworkNameParseErr {}
    impl FromStr for NetworkName {
        type Err = NetworkNameParseErr;
        fn from_str(s: &str) -> Result<NetworkName, Self::Err> {
            match &s.to_lowercase()[..] {
                "preprod" => Ok(NetworkName::PREPROD),
                "preview" => Ok(NetworkName::PREVIEW),
                "mainnet" => Ok(NetworkName::MAINNET),
                _ => Err(NetworkNameParseErr),
            }
        }
    }
    impl NetworkConfig {
        pub fn to_magic_arg(&self) -> MagicArg {
            MagicArg(
                match self {
                    NetworkConfig::WellKnown(network_name) => {
                        match network_name {
                            NetworkName::PREPROD => {
                                pallas::network::miniprotocols::PRE_PRODUCTION_MAGIC
                            }
                            NetworkName::PREVIEW => {
                                pallas::network::miniprotocols::PREVIEW_MAGIC
                            }
                            NetworkName::MAINNET => {
                                pallas::network::miniprotocols::MAINNET_MAGIC
                            }
                        }
                    }
                    NetworkConfig::ConfigPath { magic, .. } => *magic,
                },
            )
        }
        pub fn to_chain_info(&self) -> Result<ChainWellKnownInfo, anyhow::Error> {
            Ok(
                match self {
                    NetworkConfig::WellKnown(network_name) => {
                        match network_name {
                            NetworkName::PREPROD => ChainWellKnownInfo::preprod(),
                            NetworkName::PREVIEW => ChainWellKnownInfo::preview(),
                            NetworkName::MAINNET => ChainWellKnownInfo::mainnet(),
                        }
                    }
                    NetworkConfig::ConfigPath { node_config_path, .. } => {
                        let file = File::open(node_config_path.clone())
                            .map_err(|err| ::anyhow::Error::msg({
                                let res = ::alloc::fmt::format(
                                    format_args!("Chain Info not found at given path: {0}", err),
                                );
                                res
                            }))?;
                        let reader = BufReader::new(file);
                        serde_json::from_reader(reader)
                            .expect("Invalid JSON format for ChainWellKnownInfo")
                    }
                },
            )
        }
    }
    pub mod deprecation_usage {
        #![allow(deprecated)]
        use oura::mapper::Config as MapperConfig;
        use oura::sources::n2c::Config as N2CConfig;
        use oura::sources::n2n::Config as N2NConfig;
        use oura::sources::{AddressArg, IntersectArg, MagicArg, PointArg};
        pub fn n2c_config(
            addr: AddressArg,
            magic: MagicArg,
            since_slot: Option<(u64, String)>,
            safe_block_depth: usize,
        ) -> N2CConfig {
            N2CConfig {
                address: addr,
                magic: Some(magic),
                intersect: since_slot
                    .map(|since_slot| IntersectArg::Point(
                        PointArg(since_slot.0, since_slot.1),
                    )),
                mapper: MapperConfig {
                    include_transaction_details: true,
                    ..Default::default()
                },
                min_depth: safe_block_depth,
                retry_policy: None,
                finalize: None,
                since: None,
                well_known: None,
            }
        }
        pub fn n2n_config(
            addr: AddressArg,
            magic: MagicArg,
            since_slot: Option<(u64, String)>,
            safe_block_depth: usize,
        ) -> N2NConfig {
            N2NConfig {
                address: addr,
                magic: Some(magic),
                intersect: since_slot
                    .map(|since_slot| IntersectArg::Point(
                        PointArg(since_slot.0, since_slot.1),
                    )),
                mapper: MapperConfig {
                    include_transaction_details: true,
                    ..Default::default()
                },
                min_depth: safe_block_depth,
                retry_policy: None,
                finalize: None,
                since: None,
                well_known: None,
            }
        }
    }
    pub use self::deprecation_usage::*;
}
pub mod database {
    #[cfg(feature = "diesel")]
    pub mod diesel {
        pub mod sync_progress {
            use data_encoding::HEXLOWER;
            use diesel::{prelude::*, Connection};
            use tracing::{debug, info_span, span, Level};
            #[diesel(table_name = crate::schema::sync_progress)]
            pub struct SyncProgressTable {
                pub block_slot: i64,
                pub block_hash: Vec<u8>,
                pub processed: bool,
            }
            #[automatically_derived]
            impl ::core::clone::Clone for SyncProgressTable {
                #[inline]
                fn clone(&self) -> SyncProgressTable {
                    SyncProgressTable {
                        block_slot: ::core::clone::Clone::clone(&self.block_slot),
                        block_hash: ::core::clone::Clone::clone(&self.block_hash),
                        processed: ::core::clone::Clone::clone(&self.processed),
                    }
                }
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for SyncProgressTable {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::debug_struct_field3_finish(
                        f,
                        "SyncProgressTable",
                        "block_slot",
                        &self.block_slot,
                        "block_hash",
                        &self.block_hash,
                        "processed",
                        &&self.processed,
                    )
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Eq for SyncProgressTable {
                #[inline]
                #[doc(hidden)]
                #[coverage(off)]
                fn assert_receiver_is_total_eq(&self) -> () {
                    let _: ::core::cmp::AssertParamIsEq<i64>;
                    let _: ::core::cmp::AssertParamIsEq<Vec<u8>>;
                    let _: ::core::cmp::AssertParamIsEq<bool>;
                }
            }
            #[automatically_derived]
            impl ::core::marker::StructuralPartialEq for SyncProgressTable {}
            #[automatically_derived]
            impl ::core::cmp::PartialEq for SyncProgressTable {
                #[inline]
                fn eq(&self, other: &SyncProgressTable) -> bool {
                    self.block_slot == other.block_slot
                        && self.block_hash == other.block_hash
                        && self.processed == other.processed
                }
            }
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::deserialize::{self, FromStaticSqlRow, Queryable};
                use diesel::row::{Row as _, Field as _};
                use std::convert::TryInto;
                impl<
                    __DB: diesel::backend::Backend,
                    __ST0,
                    __ST1,
                    __ST2,
                > Queryable<(__ST0, __ST1, __ST2), __DB> for SyncProgressTable
                where
                    (i64, Vec<u8>, bool): FromStaticSqlRow<(__ST0, __ST1, __ST2), __DB>,
                {
                    type Row = (i64, Vec<u8>, bool);
                    fn build(row: Self::Row) -> deserialize::Result<Self> {
                        Ok(Self {
                            block_slot: row.0.try_into()?,
                            block_hash: row.1.try_into()?,
                            processed: row.2.try_into()?,
                        })
                    }
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::expression::Selectable;
                impl<__DB: diesel::backend::Backend> Selectable<__DB>
                for SyncProgressTable {
                    type SelectExpression = (
                        crate::schema::sync_progress::block_slot,
                        crate::schema::sync_progress::block_hash,
                        crate::schema::sync_progress::processed,
                    );
                    fn construct_selection() -> Self::SelectExpression {
                        (
                            crate::schema::sync_progress::block_slot,
                            crate::schema::sync_progress::block_hash,
                            crate::schema::sync_progress::processed,
                        )
                    }
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::insertable::Insertable;
                use diesel::internal::derives::insertable::UndecoratedInsertRecord;
                use diesel::prelude::*;
                #[allow(unused_qualifications)]
                impl Insertable<crate::schema::sync_progress::table>
                for SyncProgressTable {
                    type Values = <(
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::block_slot,
                                i64,
                            >,
                        >,
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::block_hash,
                                Vec<u8>,
                            >,
                        >,
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::processed,
                                bool,
                            >,
                        >,
                    ) as Insertable<crate::schema::sync_progress::table>>::Values;
                    fn values(
                        self,
                    ) -> <(
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::block_slot,
                                i64,
                            >,
                        >,
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::block_hash,
                                Vec<u8>,
                            >,
                        >,
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::processed,
                                bool,
                            >,
                        >,
                    ) as Insertable<crate::schema::sync_progress::table>>::Values {
                        (
                            std::option::Option::Some(
                                crate::schema::sync_progress::block_slot.eq(self.block_slot),
                            ),
                            std::option::Option::Some(
                                crate::schema::sync_progress::block_hash.eq(self.block_hash),
                            ),
                            std::option::Option::Some(
                                crate::schema::sync_progress::processed.eq(self.processed),
                            ),
                        )
                            .values()
                    }
                }
                #[allow(unused_qualifications)]
                impl<'insert> Insertable<crate::schema::sync_progress::table>
                for &'insert SyncProgressTable {
                    type Values = <(
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::block_slot,
                                &'insert i64,
                            >,
                        >,
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::block_hash,
                                &'insert Vec<u8>,
                            >,
                        >,
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::processed,
                                &'insert bool,
                            >,
                        >,
                    ) as Insertable<crate::schema::sync_progress::table>>::Values;
                    fn values(
                        self,
                    ) -> <(
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::block_slot,
                                &'insert i64,
                            >,
                        >,
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::block_hash,
                                &'insert Vec<u8>,
                            >,
                        >,
                        std::option::Option<
                            diesel::dsl::Eq<
                                crate::schema::sync_progress::processed,
                                &'insert bool,
                            >,
                        >,
                    ) as Insertable<crate::schema::sync_progress::table>>::Values {
                        (
                            std::option::Option::Some(
                                crate::schema::sync_progress::block_slot
                                    .eq(&self.block_slot),
                            ),
                            std::option::Option::Some(
                                crate::schema::sync_progress::block_hash
                                    .eq(&self.block_hash),
                            ),
                            std::option::Option::Some(
                                crate::schema::sync_progress::processed.eq(&self.processed),
                            ),
                        )
                            .values()
                    }
                }
                impl UndecoratedInsertRecord<crate::schema::sync_progress::table>
                for SyncProgressTable {}
            };
            impl SyncProgressTable {
                pub fn new(
                    block_slot: u64,
                    block_hash: String,
                ) -> Result<SyncProgressTable, anyhow::Error> {
                    Ok(SyncProgressTable {
                        block_slot: block_slot as i64,
                        block_hash: HEXLOWER.decode(block_hash.as_bytes())?,
                        processed: false,
                    })
                }
                /// Obtain the sync status of the DB
                pub fn get(
                    conn: &mut diesel::PgConnection,
                ) -> Result<Option<Self>, diesel::result::Error> {
                    use crate::schema::sync_progress::dsl::*;
                    let span = {
                        use ::tracing::__macro_support::Callsite as _;
                        static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                            static META: ::tracing::Metadata<'static> = {
                                ::tracing_core::metadata::Metadata::new(
                                    "Get SyncProgress",
                                    "tx_indexer::database::diesel::sync_progress",
                                    ::tracing::Level::INFO,
                                    ::core::option::Option::Some(
                                        "src/database/diesel/sync_progress.rs",
                                    ),
                                    ::core::option::Option::Some(28u32),
                                    ::core::option::Option::Some(
                                        "tx_indexer::database::diesel::sync_progress",
                                    ),
                                    ::tracing_core::field::FieldSet::new(
                                        &[],
                                        ::tracing_core::callsite::Identifier(&__CALLSITE),
                                    ),
                                    ::tracing::metadata::Kind::SPAN,
                                )
                            };
                            ::tracing::callsite::DefaultCallsite::new(&META)
                        };
                        let mut interest = ::tracing::subscriber::Interest::never();
                        if ::tracing::Level::INFO
                            <= ::tracing::level_filters::STATIC_MAX_LEVEL
                            && ::tracing::Level::INFO
                                <= ::tracing::level_filters::LevelFilter::current()
                            && {
                                interest = __CALLSITE.interest();
                                !interest.is_never()
                            }
                            && ::tracing::__macro_support::__is_enabled(
                                __CALLSITE.metadata(),
                                interest,
                            )
                        {
                            let meta = __CALLSITE.metadata();
                            ::tracing::Span::new(meta, &{ meta.fields().value_set(&[]) })
                        } else {
                            let span = ::tracing::__macro_support::__disabled_span(
                                __CALLSITE.metadata(),
                            );
                            {};
                            span
                        }
                    };
                    let _entered = span.enter();
                    sync_progress
                        .filter(processed.eq(true))
                        .select(SyncProgressTable::as_select())
                        .first(conn)
                        .optional()
                }
                /// Save a new entity to the database.
                pub fn store(
                    &self,
                    conn: &mut diesel::PgConnection,
                ) -> Result<(), diesel::result::Error> {
                    use crate::schema::sync_progress::{self, dsl::*};
                    let span = {
                        use ::tracing::__macro_support::Callsite as _;
                        static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                            static META: ::tracing::Metadata<'static> = {
                                ::tracing_core::metadata::Metadata::new(
                                    "Store SyncProgress",
                                    "tx_indexer::database::diesel::sync_progress",
                                    Level::INFO,
                                    ::core::option::Option::Some(
                                        "src/database/diesel/sync_progress.rs",
                                    ),
                                    ::core::option::Option::Some(43u32),
                                    ::core::option::Option::Some(
                                        "tx_indexer::database::diesel::sync_progress",
                                    ),
                                    ::tracing_core::field::FieldSet::new(
                                        &["self.block_slot"],
                                        ::tracing_core::callsite::Identifier(&__CALLSITE),
                                    ),
                                    ::tracing::metadata::Kind::SPAN,
                                )
                            };
                            ::tracing::callsite::DefaultCallsite::new(&META)
                        };
                        let mut interest = ::tracing::subscriber::Interest::never();
                        if Level::INFO <= ::tracing::level_filters::STATIC_MAX_LEVEL
                            && Level::INFO
                                <= ::tracing::level_filters::LevelFilter::current()
                            && {
                                interest = __CALLSITE.interest();
                                !interest.is_never()
                            }
                            && ::tracing::__macro_support::__is_enabled(
                                __CALLSITE.metadata(),
                                interest,
                            )
                        {
                            let meta = __CALLSITE.metadata();
                            ::tracing::Span::new(
                                meta,
                                &{
                                    #[allow(unused_imports)]
                                    use ::tracing::field::{debug, display, Value};
                                    let mut iter = meta.fields().iter();
                                    meta.fields()
                                        .value_set(
                                            &[
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(
                                                        &debug(&self.block_slot) as &dyn Value,
                                                    ),
                                                ),
                                            ],
                                        )
                                },
                            )
                        } else {
                            let span = ::tracing::__macro_support::__disabled_span(
                                __CALLSITE.metadata(),
                            );
                            {};
                            span
                        }
                    };
                    let _ = span.enter();
                    let already_stored = sync_progress
                        .filter(processed.eq(false))
                        .filter(block_slot.eq(self.block_slot))
                        .select(SyncProgressTable::as_select())
                        .first(conn)
                        .optional()?;
                    if already_stored.is_none() {
                        conn.transaction::<
                                _,
                                diesel::result::Error,
                                _,
                            >(|conn| {
                            diesel::delete(sync_progress.filter(processed.eq(true)))
                                .execute(conn)?;
                            diesel::update(sync_progress.filter(processed.eq(false)))
                                .set(processed.eq(true))
                                .execute(conn)?;
                            diesel::insert_into(sync_progress::table)
                                .values(self)
                                .execute(conn)?;
                            Ok(())
                        })?;
                        {
                            use ::tracing::__macro_support::Callsite as _;
                            static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                static META: ::tracing::Metadata<'static> = {
                                    ::tracing_core::metadata::Metadata::new(
                                        "event src/database/diesel/sync_progress.rs:69",
                                        "tx_indexer::database::diesel::sync_progress",
                                        ::tracing::Level::DEBUG,
                                        ::core::option::Option::Some(
                                            "src/database/diesel/sync_progress.rs",
                                        ),
                                        ::core::option::Option::Some(69u32),
                                        ::core::option::Option::Some(
                                            "tx_indexer::database::diesel::sync_progress",
                                        ),
                                        ::tracing_core::field::FieldSet::new(
                                            &["message"],
                                            ::tracing_core::callsite::Identifier(&__CALLSITE),
                                        ),
                                        ::tracing::metadata::Kind::EVENT,
                                    )
                                };
                                ::tracing::callsite::DefaultCallsite::new(&META)
                            };
                            let enabled = ::tracing::Level::DEBUG
                                <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                && ::tracing::Level::DEBUG
                                    <= ::tracing::level_filters::LevelFilter::current()
                                && {
                                    let interest = __CALLSITE.interest();
                                    !interest.is_never()
                                        && ::tracing::__macro_support::__is_enabled(
                                            __CALLSITE.metadata(),
                                            interest,
                                        )
                                };
                            if enabled {
                                (|value_set: ::tracing::field::ValueSet| {
                                    let meta = __CALLSITE.metadata();
                                    ::tracing::Event::dispatch(meta, &value_set);
                                })({
                                    #[allow(unused_imports)]
                                    use ::tracing::field::{debug, display, Value};
                                    let mut iter = __CALLSITE.metadata().fields().iter();
                                    __CALLSITE
                                        .metadata()
                                        .fields()
                                        .value_set(
                                            &[
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(
                                                        &format_args!("Stored Sync Progress") as &dyn Value,
                                                    ),
                                                ),
                                            ],
                                        )
                                });
                            } else {
                            }
                        }
                    } else {
                        {
                            use ::tracing::__macro_support::Callsite as _;
                            static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                static META: ::tracing::Metadata<'static> = {
                                    ::tracing_core::metadata::Metadata::new(
                                        "event src/database/diesel/sync_progress.rs:71",
                                        "tx_indexer::database::diesel::sync_progress",
                                        ::tracing::Level::DEBUG,
                                        ::core::option::Option::Some(
                                            "src/database/diesel/sync_progress.rs",
                                        ),
                                        ::core::option::Option::Some(71u32),
                                        ::core::option::Option::Some(
                                            "tx_indexer::database::diesel::sync_progress",
                                        ),
                                        ::tracing_core::field::FieldSet::new(
                                            &["message"],
                                            ::tracing_core::callsite::Identifier(&__CALLSITE),
                                        ),
                                        ::tracing::metadata::Kind::EVENT,
                                    )
                                };
                                ::tracing::callsite::DefaultCallsite::new(&META)
                            };
                            let enabled = ::tracing::Level::DEBUG
                                <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                && ::tracing::Level::DEBUG
                                    <= ::tracing::level_filters::LevelFilter::current()
                                && {
                                    let interest = __CALLSITE.interest();
                                    !interest.is_never()
                                        && ::tracing::__macro_support::__is_enabled(
                                            __CALLSITE.metadata(),
                                            interest,
                                        )
                                };
                            if enabled {
                                (|value_set: ::tracing::field::ValueSet| {
                                    let meta = __CALLSITE.metadata();
                                    ::tracing::Event::dispatch(meta, &value_set);
                                })({
                                    #[allow(unused_imports)]
                                    use ::tracing::field::{debug, display, Value};
                                    let mut iter = __CALLSITE.metadata().fields().iter();
                                    __CALLSITE
                                        .metadata()
                                        .fields()
                                        .value_set(
                                            &[
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(
                                                        &format_args!("Duplicate Sync Progress event") as &dyn Value,
                                                    ),
                                                ),
                                            ],
                                        )
                                });
                            } else {
                            }
                        }
                    }
                    Ok(())
                }
                pub fn get_or(
                    conn: &mut diesel::PgConnection,
                    since_slot: Option<u64>,
                    since_block: Option<String>,
                ) -> Result<Option<(u64, String)>, anyhow::Error> {
                    let sync_status = Self::get(conn)?;
                    Ok(
                        sync_status
                            .map(|Self { block_slot, block_hash, .. }| (
                                block_slot as u64,
                                HEXLOWER.encode(&block_hash),
                            ))
                            .or(since_slot.zip(since_block)),
                    )
                }
            }
        }
    }
    pub mod plutus {
        #[cfg(feature = "diesel")]
        use diesel::sql_types::{Array, Bytea, Jsonb, Nullable};
        use num_bigint::BigInt;
        use plutus_ledger_api as pla;
        use thiserror::Error;
        use tx_bakery::csl;
        use tx_bakery::utils::{
            csl_to_pla::{TryFromCSLError, TryToPLA},
            pla_to_csl::{TryFromPLAError, TryToCSLWithDef},
        };
        pub enum DBTypeConversionError {
            #[error(
                "Couldn't parse DB type, because some invariants weren't valid: {0}"
            )]
            InvariantBroken(String),
            #[error("Cannot represent BigInt as PostgreSQL BIGINT type: {0}")]
            BigIntConversion(num_bigint::TryFromBigIntError<BigInt>),
            #[error(transparent)]
            PlutusDataEncodingError(#[from] PlutusDataEncodingError),
        }
        #[allow(unused_qualifications)]
        impl std::error::Error for DBTypeConversionError {
            fn source(
                &self,
            ) -> ::core::option::Option<&(dyn std::error::Error + 'static)> {
                use thiserror::__private::AsDynError as _;
                #[allow(deprecated)]
                match self {
                    DBTypeConversionError::InvariantBroken { .. } => {
                        ::core::option::Option::None
                    }
                    DBTypeConversionError::BigIntConversion { .. } => {
                        ::core::option::Option::None
                    }
                    DBTypeConversionError::PlutusDataEncodingError { 0: transparent } => {
                        std::error::Error::source(transparent.as_dyn_error())
                    }
                }
            }
        }
        #[allow(unused_qualifications)]
        impl ::core::fmt::Display for DBTypeConversionError {
            fn fmt(
                &self,
                __formatter: &mut ::core::fmt::Formatter,
            ) -> ::core::fmt::Result {
                use thiserror::__private::AsDisplay as _;
                #[allow(unused_variables, deprecated, clippy::used_underscore_binding)]
                match self {
                    DBTypeConversionError::InvariantBroken(_0) => {
                        __formatter
                            .write_fmt(
                                format_args!(
                                    "Couldn\'t parse DB type, because some invariants weren\'t valid: {0}",
                                    _0.as_display(),
                                ),
                            )
                    }
                    DBTypeConversionError::BigIntConversion(_0) => {
                        __formatter
                            .write_fmt(
                                format_args!(
                                    "Cannot represent BigInt as PostgreSQL BIGINT type: {0}",
                                    _0.as_display(),
                                ),
                            )
                    }
                    DBTypeConversionError::PlutusDataEncodingError(_0) => {
                        ::core::fmt::Display::fmt(_0, __formatter)
                    }
                }
            }
        }
        #[allow(unused_qualifications)]
        impl ::core::convert::From<PlutusDataEncodingError> for DBTypeConversionError {
            #[allow(deprecated)]
            fn from(source: PlutusDataEncodingError) -> Self {
                DBTypeConversionError::PlutusDataEncodingError {
                    0: source,
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for DBTypeConversionError {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                match self {
                    DBTypeConversionError::InvariantBroken(__self_0) => {
                        ::core::fmt::Formatter::debug_tuple_field1_finish(
                            f,
                            "InvariantBroken",
                            &__self_0,
                        )
                    }
                    DBTypeConversionError::BigIntConversion(__self_0) => {
                        ::core::fmt::Formatter::debug_tuple_field1_finish(
                            f,
                            "BigIntConversion",
                            &__self_0,
                        )
                    }
                    DBTypeConversionError::PlutusDataEncodingError(__self_0) => {
                        ::core::fmt::Formatter::debug_tuple_field1_finish(
                            f,
                            "PlutusDataEncodingError",
                            &__self_0,
                        )
                    }
                }
            }
        }
        #[cfg(feature = "diesel")]
        pub mod sql_types {
            #[diesel(postgres_type(name = "address", schema = "plutus"))]
            pub struct Address;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for Address {
                    type QueryId = Address;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for Address {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for Address {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<Address> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("address", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "asset_quantity", schema = "plutus"))]
            pub struct AssetQuantity;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for AssetQuantity {
                    type QueryId = AssetQuantity;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for AssetQuantity {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for AssetQuantity {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<AssetQuantity> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("asset_quantity", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "chain_pointer", schema = "plutus"))]
            pub struct ChainPointer;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for ChainPointer {
                    type QueryId = ChainPointer;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for ChainPointer {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for ChainPointer {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<ChainPointer> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("chain_pointer", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "credential", schema = "plutus"))]
            pub struct Credential;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for Credential {
                    type QueryId = Credential;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for Credential {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for Credential {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<Credential> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("credential", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "currency_symbol", schema = "plutus"))]
            pub struct CurrencySymbol;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for CurrencySymbol {
                    type QueryId = CurrencySymbol;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for CurrencySymbol {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for CurrencySymbol {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<CurrencySymbol> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("currency_symbol", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "datum_hash", schema = "plutus"))]
            pub struct DatumHash;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for DatumHash {
                    type QueryId = DatumHash;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for DatumHash {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for DatumHash {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<DatumHash> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("datum_hash", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "ed25519_pub_key_hash", schema = "plutus"))]
            pub struct Ed25519PubKeyHash;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for Ed25519PubKeyHash {
                    type QueryId = Ed25519PubKeyHash;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for Ed25519PubKeyHash {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for Ed25519PubKeyHash {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<Ed25519PubKeyHash>
                for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("ed25519_pub_key_hash", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "hash28", schema = "plutus"))]
            pub struct Hash28;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for Hash28 {
                    type QueryId = Hash28;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for Hash28 {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for Hash28 {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<Hash28> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("hash28", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "hash32", schema = "plutus"))]
            pub struct Hash32;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for Hash32 {
                    type QueryId = Hash32;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for Hash32 {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for Hash32 {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<Hash32> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("hash32", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "output_datum", schema = "plutus"))]
            pub struct OutputDatum;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for OutputDatum {
                    type QueryId = OutputDatum;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for OutputDatum {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for OutputDatum {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<OutputDatum> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("output_datum", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "plutus_data", schema = "plutus"))]
            pub struct PlutusData;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for PlutusData {
                    type QueryId = PlutusData;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for PlutusData {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for PlutusData {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<PlutusData> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("plutus_data", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "script_hash", schema = "plutus"))]
            pub struct ScriptHash;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for ScriptHash {
                    type QueryId = ScriptHash;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for ScriptHash {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for ScriptHash {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<ScriptHash> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("script_hash", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "slot", schema = "plutus"))]
            pub struct Slot;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for Slot {
                    type QueryId = Slot;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for Slot {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for Slot {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<Slot> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("slot", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "staking_credential", schema = "plutus"))]
            pub struct StakingCredential;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for StakingCredential {
                    type QueryId = StakingCredential;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for StakingCredential {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for StakingCredential {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<StakingCredential>
                for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("staking_credential", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "token_name", schema = "plutus"))]
            pub struct TokenName;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for TokenName {
                    type QueryId = TokenName;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for TokenName {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for TokenName {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<TokenName> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("token_name", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "transaction_hash", schema = "plutus"))]
            pub struct TransactionHash;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for TransactionHash {
                    type QueryId = TransactionHash;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for TransactionHash {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for TransactionHash {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<TransactionHash> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("transaction_hash", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "transaction_input", schema = "plutus"))]
            pub struct TransactionInput;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for TransactionInput {
                    type QueryId = TransactionInput;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for TransactionInput {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for TransactionInput {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<TransactionInput> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("transaction_input", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "transaction_output", schema = "plutus"))]
            pub struct TransactionOutput;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for TransactionOutput {
                    type QueryId = TransactionOutput;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for TransactionOutput {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for TransactionOutput {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<TransactionOutput>
                for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("transaction_output", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "tx_in_info", schema = "plutus"))]
            pub struct TxInInfo;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for TxInInfo {
                    type QueryId = TxInInfo;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for TxInInfo {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for TxInInfo {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<TxInInfo> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("tx_in_info", Some("plutus"))
                    }
                }
            };
            #[diesel(postgres_type(name = "value", schema = "plutus"))]
            pub struct Value;
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for Value {
                    type QueryId = Value;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                impl diesel::sql_types::SqlType for Value {
                    type IsNull = diesel::sql_types::is_nullable::NotNull;
                }
                impl diesel::sql_types::SingleValue for Value {}
                use diesel::pg::{PgMetadataLookup, PgTypeMetadata};
                impl diesel::sql_types::HasSqlType<Value> for diesel::pg::Pg {
                    fn metadata(lookup: &mut Self::MetadataLookup) -> PgTypeMetadata {
                        lookup.lookup_type("value", Some("plutus"))
                    }
                }
            };
        }
        /// Hash28
        #[diesel(sql_type = sql_types::Hash28)]
        #[diesel_derive(sql_type = sql_types::Hash28, sql_inner_type = Bytea)]
        pub struct Hash28(pub Vec<u8>);
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::Hash28> for &'__expr Hash28 {
                type Expression = Bound<sql_types::Hash28, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::Hash28>> for &'__expr Hash28 {
                type Expression = Bound<Nullable<sql_types::Hash28>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::Hash28>
            for &'__expr2 &'__expr Hash28 {
                type Expression = Bound<sql_types::Hash28, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::Hash28>>
            for &'__expr2 &'__expr Hash28 {
                type Expression = Bound<Nullable<sql_types::Hash28>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::Hash28>, __DB>
            for Hash28
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::Hash28, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::Hash28, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::Hash28> for Hash28 {
                type Expression = Bound<sql_types::Hash28, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::Hash28>> for Hash28 {
                type Expression = Bound<Nullable<sql_types::Hash28>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for Hash28
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        impl diesel::deserialize::FromSql<sql_types::Hash28, diesel::pg::Pg> for Hash28 {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let inner = diesel::deserialize::FromSql::<
                    Bytea,
                    diesel::pg::Pg,
                >::from_sql(bytes)?;
                Ok(Self(inner))
            }
        }
        impl diesel::serialize::ToSql<sql_types::Hash28, diesel::pg::Pg> for Hash28 {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                <Vec<
                    u8,
                > as diesel::serialize::ToSql<
                    Bytea,
                    diesel::pg::Pg,
                >>::to_sql(&self.0, &mut out.reborrow())
            }
        }
        #[automatically_derived]
        impl ::core::clone::Clone for Hash28 {
            #[inline]
            fn clone(&self) -> Hash28 {
                Hash28(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for Hash28 {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Hash28", &&self.0)
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for Hash28 {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for Hash28 {
            #[inline]
            fn eq(&self, other: &Hash28) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for Hash28 {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Vec<u8>>;
            }
        }
        impl From<pla::v2::crypto::LedgerBytes> for Hash28 {
            fn from(item: pla::v2::crypto::LedgerBytes) -> Self {
                Hash28(item.0)
            }
        }
        impl From<Hash28> for pla::v2::crypto::LedgerBytes {
            fn from(item: Hash28) -> Self {
                pla::v2::crypto::LedgerBytes(item.0)
            }
        }
        /// Hash32
        #[diesel(sql_type = sql_types::Hash32)]
        #[diesel_derive(sql_type = sql_types::Hash32, sql_inner_type = Bytea)]
        pub struct Hash32(pub Vec<u8>);
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::Hash32> for &'__expr Hash32 {
                type Expression = Bound<sql_types::Hash32, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::Hash32>> for &'__expr Hash32 {
                type Expression = Bound<Nullable<sql_types::Hash32>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::Hash32>
            for &'__expr2 &'__expr Hash32 {
                type Expression = Bound<sql_types::Hash32, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::Hash32>>
            for &'__expr2 &'__expr Hash32 {
                type Expression = Bound<Nullable<sql_types::Hash32>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::Hash32>, __DB>
            for Hash32
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::Hash32, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::Hash32, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::Hash32> for Hash32 {
                type Expression = Bound<sql_types::Hash32, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::Hash32>> for Hash32 {
                type Expression = Bound<Nullable<sql_types::Hash32>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for Hash32
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        impl diesel::deserialize::FromSql<sql_types::Hash32, diesel::pg::Pg> for Hash32 {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let inner = diesel::deserialize::FromSql::<
                    Bytea,
                    diesel::pg::Pg,
                >::from_sql(bytes)?;
                Ok(Self(inner))
            }
        }
        impl diesel::serialize::ToSql<sql_types::Hash32, diesel::pg::Pg> for Hash32 {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                <Vec<
                    u8,
                > as diesel::serialize::ToSql<
                    Bytea,
                    diesel::pg::Pg,
                >>::to_sql(&self.0, &mut out.reborrow())
            }
        }
        #[automatically_derived]
        impl ::core::clone::Clone for Hash32 {
            #[inline]
            fn clone(&self) -> Hash32 {
                Hash32(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for Hash32 {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Hash32", &&self.0)
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for Hash32 {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for Hash32 {
            #[inline]
            fn eq(&self, other: &Hash32) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for Hash32 {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Vec<u8>>;
            }
        }
        impl From<pla::v2::crypto::LedgerBytes> for Hash32 {
            fn from(item: pla::v2::crypto::LedgerBytes) -> Self {
                Hash32(item.0)
            }
        }
        impl From<Hash32> for pla::v2::crypto::LedgerBytes {
            fn from(item: Hash32) -> Self {
                pla::v2::crypto::LedgerBytes(item.0)
            }
        }
        /// CurrencySymbol
        #[diesel(sql_type = sql_types::CurrencySymbol)]
        #[diesel_derive(sql_type = sql_types::Hash32, sql_inner_type = Bytea)]
        pub struct CurrencySymbol(pub Vec<u8>);
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::CurrencySymbol>
            for &'__expr CurrencySymbol {
                type Expression = Bound<sql_types::CurrencySymbol, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::CurrencySymbol>>
            for &'__expr CurrencySymbol {
                type Expression = Bound<Nullable<sql_types::CurrencySymbol>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::CurrencySymbol>
            for &'__expr2 &'__expr CurrencySymbol {
                type Expression = Bound<sql_types::CurrencySymbol, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::CurrencySymbol>>
            for &'__expr2 &'__expr CurrencySymbol {
                type Expression = Bound<Nullable<sql_types::CurrencySymbol>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<
                __DB,
            > diesel::serialize::ToSql<Nullable<sql_types::CurrencySymbol>, __DB>
            for CurrencySymbol
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::CurrencySymbol, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::CurrencySymbol, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::CurrencySymbol> for CurrencySymbol {
                type Expression = Bound<sql_types::CurrencySymbol, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::CurrencySymbol>> for CurrencySymbol {
                type Expression = Bound<Nullable<sql_types::CurrencySymbol>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for CurrencySymbol
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        impl diesel::deserialize::FromSql<sql_types::Hash32, diesel::pg::Pg>
        for CurrencySymbol {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let inner = diesel::deserialize::FromSql::<
                    Bytea,
                    diesel::pg::Pg,
                >::from_sql(bytes)?;
                Ok(Self(inner))
            }
        }
        impl diesel::serialize::ToSql<sql_types::Hash32, diesel::pg::Pg>
        for CurrencySymbol {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                <Vec<
                    u8,
                > as diesel::serialize::ToSql<
                    Bytea,
                    diesel::pg::Pg,
                >>::to_sql(&self.0, &mut out.reborrow())
            }
        }
        #[automatically_derived]
        impl ::core::clone::Clone for CurrencySymbol {
            #[inline]
            fn clone(&self) -> CurrencySymbol {
                CurrencySymbol(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for CurrencySymbol {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "CurrencySymbol",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for CurrencySymbol {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for CurrencySymbol {
            #[inline]
            fn eq(&self, other: &CurrencySymbol) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for CurrencySymbol {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Vec<u8>>;
            }
        }
        impl From<pla::v2::value::CurrencySymbol> for CurrencySymbol {
            fn from(item: pla::v2::value::CurrencySymbol) -> Self {
                match item {
                    pla::v2::value::CurrencySymbol::Ada => {
                        CurrencySymbol(Vec::with_capacity(0))
                    }
                    pla::v2::value::CurrencySymbol::NativeToken(
                        pla::v2::script::MintingPolicyHash(
                            pla::v2::script::ScriptHash(
                                pla::v2::crypto::LedgerBytes(bytes),
                            ),
                        ),
                    ) => CurrencySymbol(bytes),
                }
            }
        }
        impl From<CurrencySymbol> for pla::v2::value::CurrencySymbol {
            fn from(item: CurrencySymbol) -> Self {
                let CurrencySymbol(bytes) = item;
                if bytes.is_empty() {
                    pla::v2::value::CurrencySymbol::Ada
                } else {
                    pla::v2::value::CurrencySymbol::NativeToken(
                        pla::v2::script::MintingPolicyHash(
                            pla::v2::script::ScriptHash(
                                pla::v2::crypto::LedgerBytes(bytes),
                            ),
                        ),
                    )
                }
            }
        }
        /// TokenName
        #[diesel(sql_type = sql_types::TokenName)]
        #[diesel_derive(sql_type = sql_types::Hash32, sql_inner_type = Bytea)]
        pub struct TokenName(pub Vec<u8>);
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::TokenName> for &'__expr TokenName {
                type Expression = Bound<sql_types::TokenName, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::TokenName>>
            for &'__expr TokenName {
                type Expression = Bound<Nullable<sql_types::TokenName>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::TokenName>
            for &'__expr2 &'__expr TokenName {
                type Expression = Bound<sql_types::TokenName, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::TokenName>>
            for &'__expr2 &'__expr TokenName {
                type Expression = Bound<Nullable<sql_types::TokenName>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::TokenName>, __DB>
            for TokenName
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::TokenName, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::TokenName, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::TokenName> for TokenName {
                type Expression = Bound<sql_types::TokenName, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::TokenName>> for TokenName {
                type Expression = Bound<Nullable<sql_types::TokenName>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for TokenName
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        impl diesel::deserialize::FromSql<sql_types::Hash32, diesel::pg::Pg>
        for TokenName {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let inner = diesel::deserialize::FromSql::<
                    Bytea,
                    diesel::pg::Pg,
                >::from_sql(bytes)?;
                Ok(Self(inner))
            }
        }
        impl diesel::serialize::ToSql<sql_types::Hash32, diesel::pg::Pg> for TokenName {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                <Vec<
                    u8,
                > as diesel::serialize::ToSql<
                    Bytea,
                    diesel::pg::Pg,
                >>::to_sql(&self.0, &mut out.reborrow())
            }
        }
        #[automatically_derived]
        impl ::core::clone::Clone for TokenName {
            #[inline]
            fn clone(&self) -> TokenName {
                TokenName(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for TokenName {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "TokenName",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for TokenName {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for TokenName {
            #[inline]
            fn eq(&self, other: &TokenName) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for TokenName {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Vec<u8>>;
            }
        }
        impl From<pla::v2::value::TokenName> for TokenName {
            fn from(item: pla::v2::value::TokenName) -> Self {
                TokenName(item.0.0)
            }
        }
        impl From<TokenName> for pla::v2::value::TokenName {
            fn from(item: TokenName) -> Self {
                pla::v2::value::TokenName(pla::v2::crypto::LedgerBytes(item.0))
            }
        }
        /// TransactionHash
        #[diesel(sql_type = sql_types::TransactionHash)]
        #[diesel_derive(
            sql_type = sql_types::Hash32,
            sql_inner_type = sql_types::Hash32
        )]
        pub struct TransactionHash(pub Hash32);
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::TransactionHash>
            for &'__expr TransactionHash {
                type Expression = Bound<sql_types::TransactionHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::TransactionHash>>
            for &'__expr TransactionHash {
                type Expression = Bound<Nullable<sql_types::TransactionHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::TransactionHash>
            for &'__expr2 &'__expr TransactionHash {
                type Expression = Bound<sql_types::TransactionHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::TransactionHash>>
            for &'__expr2 &'__expr TransactionHash {
                type Expression = Bound<Nullable<sql_types::TransactionHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<
                __DB,
            > diesel::serialize::ToSql<Nullable<sql_types::TransactionHash>, __DB>
            for TransactionHash
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::TransactionHash, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::TransactionHash, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::TransactionHash> for TransactionHash {
                type Expression = Bound<sql_types::TransactionHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::TransactionHash>> for TransactionHash {
                type Expression = Bound<Nullable<sql_types::TransactionHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for TransactionHash
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        impl diesel::deserialize::FromSql<sql_types::Hash32, diesel::pg::Pg>
        for TransactionHash {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let inner = diesel::deserialize::FromSql::<
                    sql_types::Hash32,
                    diesel::pg::Pg,
                >::from_sql(bytes)?;
                Ok(Self(inner))
            }
        }
        impl diesel::serialize::ToSql<sql_types::Hash32, diesel::pg::Pg>
        for TransactionHash {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                <Hash32 as diesel::serialize::ToSql<
                    sql_types::Hash32,
                    diesel::pg::Pg,
                >>::to_sql(&self.0, &mut out.reborrow())
            }
        }
        #[automatically_derived]
        impl ::core::clone::Clone for TransactionHash {
            #[inline]
            fn clone(&self) -> TransactionHash {
                TransactionHash(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for TransactionHash {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "TransactionHash",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for TransactionHash {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for TransactionHash {
            #[inline]
            fn eq(&self, other: &TransactionHash) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for TransactionHash {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Hash32>;
            }
        }
        impl From<pla::v2::transaction::TransactionHash> for TransactionHash {
            fn from(item: pla::v2::transaction::TransactionHash) -> Self {
                TransactionHash(item.0.into())
            }
        }
        impl From<TransactionHash> for pla::v2::transaction::TransactionHash {
            fn from(item: TransactionHash) -> Self {
                pla::v2::transaction::TransactionHash(item.0.into())
            }
        }
        /// Ed25519PubKeyHash
        #[diesel(sql_type = sql_types::Ed25519PubKeyHash)]
        #[diesel_derive(
            sql_type = sql_types::Hash28,
            sql_inner_type = sql_types::Hash28
        )]
        pub struct Ed25519PubKeyHash(pub Hash28);
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::Ed25519PubKeyHash>
            for &'__expr Ed25519PubKeyHash {
                type Expression = Bound<sql_types::Ed25519PubKeyHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::Ed25519PubKeyHash>>
            for &'__expr Ed25519PubKeyHash {
                type Expression = Bound<Nullable<sql_types::Ed25519PubKeyHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::Ed25519PubKeyHash>
            for &'__expr2 &'__expr Ed25519PubKeyHash {
                type Expression = Bound<sql_types::Ed25519PubKeyHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::Ed25519PubKeyHash>>
            for &'__expr2 &'__expr Ed25519PubKeyHash {
                type Expression = Bound<Nullable<sql_types::Ed25519PubKeyHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<
                __DB,
            > diesel::serialize::ToSql<Nullable<sql_types::Ed25519PubKeyHash>, __DB>
            for Ed25519PubKeyHash
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::Ed25519PubKeyHash, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::Ed25519PubKeyHash, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::Ed25519PubKeyHash> for Ed25519PubKeyHash {
                type Expression = Bound<sql_types::Ed25519PubKeyHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::Ed25519PubKeyHash>>
            for Ed25519PubKeyHash {
                type Expression = Bound<Nullable<sql_types::Ed25519PubKeyHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for Ed25519PubKeyHash
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        impl diesel::deserialize::FromSql<sql_types::Hash28, diesel::pg::Pg>
        for Ed25519PubKeyHash {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let inner = diesel::deserialize::FromSql::<
                    sql_types::Hash28,
                    diesel::pg::Pg,
                >::from_sql(bytes)?;
                Ok(Self(inner))
            }
        }
        impl diesel::serialize::ToSql<sql_types::Hash28, diesel::pg::Pg>
        for Ed25519PubKeyHash {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                <Hash28 as diesel::serialize::ToSql<
                    sql_types::Hash28,
                    diesel::pg::Pg,
                >>::to_sql(&self.0, &mut out.reborrow())
            }
        }
        #[automatically_derived]
        impl ::core::clone::Clone for Ed25519PubKeyHash {
            #[inline]
            fn clone(&self) -> Ed25519PubKeyHash {
                Ed25519PubKeyHash(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for Ed25519PubKeyHash {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "Ed25519PubKeyHash",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for Ed25519PubKeyHash {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for Ed25519PubKeyHash {
            #[inline]
            fn eq(&self, other: &Ed25519PubKeyHash) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for Ed25519PubKeyHash {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Hash28>;
            }
        }
        impl From<pla::v2::crypto::Ed25519PubKeyHash> for Ed25519PubKeyHash {
            fn from(item: pla::v2::crypto::Ed25519PubKeyHash) -> Self {
                Ed25519PubKeyHash(item.0.into())
            }
        }
        impl From<Ed25519PubKeyHash> for pla::v2::crypto::Ed25519PubKeyHash {
            fn from(item: Ed25519PubKeyHash) -> Self {
                pla::v2::crypto::Ed25519PubKeyHash(item.0.into())
            }
        }
        /// ScriptHash
        #[diesel(sql_type = sql_types::ScriptHash)]
        #[diesel_derive(
            sql_type = sql_types::Hash32,
            sql_inner_type = sql_types::Hash28
        )]
        pub struct ScriptHash(pub Hash28);
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::ScriptHash> for &'__expr ScriptHash {
                type Expression = Bound<sql_types::ScriptHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::ScriptHash>>
            for &'__expr ScriptHash {
                type Expression = Bound<Nullable<sql_types::ScriptHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::ScriptHash>
            for &'__expr2 &'__expr ScriptHash {
                type Expression = Bound<sql_types::ScriptHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::ScriptHash>>
            for &'__expr2 &'__expr ScriptHash {
                type Expression = Bound<Nullable<sql_types::ScriptHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::ScriptHash>, __DB>
            for ScriptHash
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::ScriptHash, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::ScriptHash, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::ScriptHash> for ScriptHash {
                type Expression = Bound<sql_types::ScriptHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::ScriptHash>> for ScriptHash {
                type Expression = Bound<Nullable<sql_types::ScriptHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for ScriptHash
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        impl diesel::deserialize::FromSql<sql_types::Hash32, diesel::pg::Pg>
        for ScriptHash {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let inner = diesel::deserialize::FromSql::<
                    sql_types::Hash28,
                    diesel::pg::Pg,
                >::from_sql(bytes)?;
                Ok(Self(inner))
            }
        }
        impl diesel::serialize::ToSql<sql_types::Hash32, diesel::pg::Pg> for ScriptHash {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                <Hash28 as diesel::serialize::ToSql<
                    sql_types::Hash28,
                    diesel::pg::Pg,
                >>::to_sql(&self.0, &mut out.reborrow())
            }
        }
        #[automatically_derived]
        impl ::core::clone::Clone for ScriptHash {
            #[inline]
            fn clone(&self) -> ScriptHash {
                ScriptHash(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for ScriptHash {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "ScriptHash",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ScriptHash {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ScriptHash {
            #[inline]
            fn eq(&self, other: &ScriptHash) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for ScriptHash {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Hash28>;
            }
        }
        impl From<pla::v2::script::ScriptHash> for ScriptHash {
            fn from(item: pla::v2::script::ScriptHash) -> Self {
                ScriptHash(item.0.into())
            }
        }
        impl From<ScriptHash> for pla::v2::script::ScriptHash {
            fn from(item: ScriptHash) -> Self {
                pla::v2::script::ScriptHash(item.0.into())
            }
        }
        /// DatumHash
        #[diesel(sql_type = sql_types::DatumHash)]
        pub struct DatumHash(pub Hash32);
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::DatumHash> for &'__expr DatumHash {
                type Expression = Bound<sql_types::DatumHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::DatumHash>>
            for &'__expr DatumHash {
                type Expression = Bound<Nullable<sql_types::DatumHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::DatumHash>
            for &'__expr2 &'__expr DatumHash {
                type Expression = Bound<sql_types::DatumHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::DatumHash>>
            for &'__expr2 &'__expr DatumHash {
                type Expression = Bound<Nullable<sql_types::DatumHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::DatumHash>, __DB>
            for DatumHash
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::DatumHash, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::DatumHash, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::DatumHash> for DatumHash {
                type Expression = Bound<sql_types::DatumHash, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::DatumHash>> for DatumHash {
                type Expression = Bound<Nullable<sql_types::DatumHash>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for DatumHash
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for DatumHash {
            #[inline]
            fn clone(&self) -> DatumHash {
                DatumHash(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for DatumHash {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "DatumHash",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for DatumHash {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for DatumHash {
            #[inline]
            fn eq(&self, other: &DatumHash) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for DatumHash {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Hash32>;
            }
        }
        impl From<pla::v2::datum::DatumHash> for DatumHash {
            fn from(item: pla::v2::datum::DatumHash) -> Self {
                DatumHash(item.0.into())
            }
        }
        impl From<DatumHash> for pla::v2::datum::DatumHash {
            fn from(item: DatumHash) -> Self {
                pla::v2::datum::DatumHash(item.0.into())
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::DatumHash, diesel::pg::Pg>
        for DatumHash {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let inner = diesel::deserialize::FromSql::from_sql(bytes)?;
                Ok(Self(inner))
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::DatumHash, diesel::pg::Pg>
        for DatumHash {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                <Hash32 as diesel::serialize::ToSql<
                    sql_types::Hash32,
                    diesel::pg::Pg,
                >>::to_sql(&self.0, &mut out.reborrow())
            }
        }
        /// Slot
        #[diesel(sql_type = sql_types::Slot)]
        pub struct Slot(pub i64);
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::Slot> for &'__expr Slot {
                type Expression = Bound<sql_types::Slot, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::Slot>> for &'__expr Slot {
                type Expression = Bound<Nullable<sql_types::Slot>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::Slot>
            for &'__expr2 &'__expr Slot {
                type Expression = Bound<sql_types::Slot, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::Slot>>
            for &'__expr2 &'__expr Slot {
                type Expression = Bound<Nullable<sql_types::Slot>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::Slot>, __DB> for Slot
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::Slot, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::Slot, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::Slot> for Slot {
                type Expression = Bound<sql_types::Slot, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::Slot>> for Slot {
                type Expression = Bound<Nullable<sql_types::Slot>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for Slot
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for Slot {
            #[inline]
            fn clone(&self) -> Slot {
                Slot(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for Slot {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Slot", &&self.0)
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for Slot {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for Slot {
            #[inline]
            fn eq(&self, other: &Slot) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for Slot {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<i64>;
            }
        }
        impl From<u64> for Slot {
            fn from(item: u64) -> Self {
                Slot(item as i64)
            }
        }
        impl From<&Slot> for u64 {
            fn from(item: &Slot) -> Self {
                item.0 as u64
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::Slot, diesel::pg::Pg> for Slot {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let slot = diesel::deserialize::FromSql::from_sql(bytes)?;
                Ok(Self(slot))
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::Slot, diesel::pg::Pg> for Slot {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                <i64 as diesel::serialize::ToSql<
                    diesel::sql_types::BigInt,
                    diesel::pg::Pg,
                >>::to_sql(&self.0, &mut out.reborrow())
            }
        }
        /// PlutusData
        pub enum PlutusDataEncodingError {
            #[error(transparent)]
            CSLConversionError(#[from] csl::JsError),
            #[error(transparent)]
            TryFromPLAError(#[from] TryFromPLAError),
            #[error(transparent)]
            TryFromCSLError(#[from] TryFromCSLError),
        }
        #[allow(unused_qualifications)]
        impl std::error::Error for PlutusDataEncodingError {
            fn source(
                &self,
            ) -> ::core::option::Option<&(dyn std::error::Error + 'static)> {
                use thiserror::__private::AsDynError as _;
                #[allow(deprecated)]
                match self {
                    PlutusDataEncodingError::CSLConversionError { 0: transparent } => {
                        std::error::Error::source(transparent.as_dyn_error())
                    }
                    PlutusDataEncodingError::TryFromPLAError { 0: transparent } => {
                        std::error::Error::source(transparent.as_dyn_error())
                    }
                    PlutusDataEncodingError::TryFromCSLError { 0: transparent } => {
                        std::error::Error::source(transparent.as_dyn_error())
                    }
                }
            }
        }
        #[allow(unused_qualifications)]
        impl ::core::fmt::Display for PlutusDataEncodingError {
            fn fmt(
                &self,
                __formatter: &mut ::core::fmt::Formatter,
            ) -> ::core::fmt::Result {
                #[allow(unused_variables, deprecated, clippy::used_underscore_binding)]
                match self {
                    PlutusDataEncodingError::CSLConversionError(_0) => {
                        ::core::fmt::Display::fmt(_0, __formatter)
                    }
                    PlutusDataEncodingError::TryFromPLAError(_0) => {
                        ::core::fmt::Display::fmt(_0, __formatter)
                    }
                    PlutusDataEncodingError::TryFromCSLError(_0) => {
                        ::core::fmt::Display::fmt(_0, __formatter)
                    }
                }
            }
        }
        #[allow(unused_qualifications)]
        impl ::core::convert::From<csl::JsError> for PlutusDataEncodingError {
            #[allow(deprecated)]
            fn from(source: csl::JsError) -> Self {
                PlutusDataEncodingError::CSLConversionError {
                    0: source,
                }
            }
        }
        #[allow(unused_qualifications)]
        impl ::core::convert::From<TryFromPLAError> for PlutusDataEncodingError {
            #[allow(deprecated)]
            fn from(source: TryFromPLAError) -> Self {
                PlutusDataEncodingError::TryFromPLAError {
                    0: source,
                }
            }
        }
        #[allow(unused_qualifications)]
        impl ::core::convert::From<TryFromCSLError> for PlutusDataEncodingError {
            #[allow(deprecated)]
            fn from(source: TryFromCSLError) -> Self {
                PlutusDataEncodingError::TryFromCSLError {
                    0: source,
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for PlutusDataEncodingError {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                match self {
                    PlutusDataEncodingError::CSLConversionError(__self_0) => {
                        ::core::fmt::Formatter::debug_tuple_field1_finish(
                            f,
                            "CSLConversionError",
                            &__self_0,
                        )
                    }
                    PlutusDataEncodingError::TryFromPLAError(__self_0) => {
                        ::core::fmt::Formatter::debug_tuple_field1_finish(
                            f,
                            "TryFromPLAError",
                            &__self_0,
                        )
                    }
                    PlutusDataEncodingError::TryFromCSLError(__self_0) => {
                        ::core::fmt::Formatter::debug_tuple_field1_finish(
                            f,
                            "TryFromCSLError",
                            &__self_0,
                        )
                    }
                }
            }
        }
        #[diesel(sql_type = sql_types::PlutusData)]
        pub struct PlutusData(pub serde_json::Value);
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::PlutusData> for &'__expr PlutusData {
                type Expression = Bound<sql_types::PlutusData, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::PlutusData>>
            for &'__expr PlutusData {
                type Expression = Bound<Nullable<sql_types::PlutusData>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::PlutusData>
            for &'__expr2 &'__expr PlutusData {
                type Expression = Bound<sql_types::PlutusData, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::PlutusData>>
            for &'__expr2 &'__expr PlutusData {
                type Expression = Bound<Nullable<sql_types::PlutusData>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::PlutusData>, __DB>
            for PlutusData
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::PlutusData, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::PlutusData, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::PlutusData> for PlutusData {
                type Expression = Bound<sql_types::PlutusData, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::PlutusData>> for PlutusData {
                type Expression = Bound<Nullable<sql_types::PlutusData>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for PlutusData
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for PlutusData {
            #[inline]
            fn clone(&self) -> PlutusData {
                PlutusData(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for PlutusData {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "PlutusData",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for PlutusData {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for PlutusData {
            #[inline]
            fn eq(&self, other: &PlutusData) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for PlutusData {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<serde_json::Value>;
            }
        }
        impl TryFrom<pla::plutus_data::PlutusData> for PlutusData {
            type Error = DBTypeConversionError;
            fn try_from(
                item: pla::plutus_data::PlutusData,
            ) -> Result<Self, Self::Error> {
                Ok(
                    PlutusData(
                        csl::decode_plutus_datum_to_json_value(
                                &item
                                    .try_to_csl()
                                    .map_err(PlutusDataEncodingError::TryFromPLAError)?,
                                csl::PlutusDatumSchema::DetailedSchema,
                            )
                            .map_err(PlutusDataEncodingError::CSLConversionError)?,
                    ),
                )
            }
        }
        impl TryFrom<PlutusData> for pla::plutus_data::PlutusData {
            type Error = DBTypeConversionError;
            fn try_from(item: PlutusData) -> Result<Self, Self::Error> {
                Ok(
                    csl::encode_json_value_to_plutus_datum(
                            item.0,
                            csl::PlutusDatumSchema::DetailedSchema,
                        )
                        .map_err(PlutusDataEncodingError::CSLConversionError)?
                        .try_to_pla()
                        .map_err(PlutusDataEncodingError::TryFromCSLError)?,
                )
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::PlutusData, diesel::pg::Pg>
        for PlutusData {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let inner = diesel::deserialize::FromSql::<
                    Jsonb,
                    diesel::pg::Pg,
                >::from_sql(bytes)?;
                Ok(Self(inner))
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::PlutusData, diesel::pg::Pg>
        for PlutusData {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                <serde_json::Value as diesel::serialize::ToSql<
                    diesel::pg::sql_types::Jsonb,
                    diesel::pg::Pg,
                >>::to_sql(&self.0, &mut out.reborrow())
            }
        }
        /// Credential
        #[diesel(sql_type = sql_types::Credential)]
        pub struct Credential {
            pub_key_hash: Option<Ed25519PubKeyHash>,
            script_hash: Option<ScriptHash>,
        }
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::Credential> for &'__expr Credential {
                type Expression = Bound<sql_types::Credential, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::Credential>>
            for &'__expr Credential {
                type Expression = Bound<Nullable<sql_types::Credential>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::Credential>
            for &'__expr2 &'__expr Credential {
                type Expression = Bound<sql_types::Credential, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::Credential>>
            for &'__expr2 &'__expr Credential {
                type Expression = Bound<Nullable<sql_types::Credential>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::Credential>, __DB>
            for Credential
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::Credential, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::Credential, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::Credential> for Credential {
                type Expression = Bound<sql_types::Credential, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::Credential>> for Credential {
                type Expression = Bound<Nullable<sql_types::Credential>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for Credential
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for Credential {
            #[inline]
            fn clone(&self) -> Credential {
                Credential {
                    pub_key_hash: ::core::clone::Clone::clone(&self.pub_key_hash),
                    script_hash: ::core::clone::Clone::clone(&self.script_hash),
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for Credential {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field2_finish(
                    f,
                    "Credential",
                    "pub_key_hash",
                    &self.pub_key_hash,
                    "script_hash",
                    &&self.script_hash,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for Credential {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for Credential {
            #[inline]
            fn eq(&self, other: &Credential) -> bool {
                self.pub_key_hash == other.pub_key_hash
                    && self.script_hash == other.script_hash
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for Credential {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Option<Ed25519PubKeyHash>>;
                let _: ::core::cmp::AssertParamIsEq<Option<ScriptHash>>;
            }
        }
        impl From<pla::v2::address::Credential> for Credential {
            fn from(item: pla::v2::address::Credential) -> Self {
                match item {
                    pla::v2::address::Credential::PubKey(pkh) => {
                        Credential {
                            pub_key_hash: Some(pkh.into()),
                            script_hash: None,
                        }
                    }
                    pla::v2::address::Credential::Script(
                        pla::v2::script::ValidatorHash(sh),
                    ) => {
                        Credential {
                            pub_key_hash: None,
                            script_hash: Some(sh.into()),
                        }
                    }
                }
            }
        }
        impl TryFrom<Credential> for pla::v2::address::Credential {
            type Error = DBTypeConversionError;
            fn try_from(item: Credential) -> Result<Self, Self::Error> {
                Ok(
                    match item {
                        Credential { pub_key_hash: Some(pkh_db), script_hash: None } => {
                            pla::v2::address::Credential::PubKey(pkh_db.into())
                        }
                        Credential { pub_key_hash: None, script_hash: Some(sh_db) } => {
                            pla::v2::address::Credential::Script(
                                pla::v2::script::ValidatorHash(sh_db.into()),
                            )
                        }
                        _ => {
                            Err(
                                DBTypeConversionError::InvariantBroken(
                                    "DB Credential must have either 'pub_key_hash' or 'script_hash'"
                                        .to_string(),
                                ),
                            )?
                        }
                    },
                )
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::Credential, diesel::pg::Pg>
        for Credential {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let (pub_key_hash, script_hash) = diesel::deserialize::FromSql::from_sql(
                    bytes,
                )?;
                Ok(Self { pub_key_hash, script_hash })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::Credential, diesel::pg::Pg>
        for Credential {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                diesel::serialize::WriteTuple::<
                    (
                        Nullable<sql_types::Ed25519PubKeyHash>,
                        Nullable<sql_types::ScriptHash>,
                    ),
                >::write_tuple(
                    &(self.pub_key_hash.clone(), self.script_hash.clone()),
                    &mut out.reborrow(),
                )
            }
        }
        /// ChainPointer
        #[diesel(sql_type = sql_types::ChainPointer)]
        pub struct ChainPointer {
            slot_num: i64,
            tx_idx: i64,
            cert_idx: i64,
        }
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::ChainPointer>
            for &'__expr ChainPointer {
                type Expression = Bound<sql_types::ChainPointer, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::ChainPointer>>
            for &'__expr ChainPointer {
                type Expression = Bound<Nullable<sql_types::ChainPointer>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::ChainPointer>
            for &'__expr2 &'__expr ChainPointer {
                type Expression = Bound<sql_types::ChainPointer, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::ChainPointer>>
            for &'__expr2 &'__expr ChainPointer {
                type Expression = Bound<Nullable<sql_types::ChainPointer>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::ChainPointer>, __DB>
            for ChainPointer
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::ChainPointer, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::ChainPointer, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::ChainPointer> for ChainPointer {
                type Expression = Bound<sql_types::ChainPointer, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::ChainPointer>> for ChainPointer {
                type Expression = Bound<Nullable<sql_types::ChainPointer>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for ChainPointer
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for ChainPointer {
            #[inline]
            fn clone(&self) -> ChainPointer {
                ChainPointer {
                    slot_num: ::core::clone::Clone::clone(&self.slot_num),
                    tx_idx: ::core::clone::Clone::clone(&self.tx_idx),
                    cert_idx: ::core::clone::Clone::clone(&self.cert_idx),
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for ChainPointer {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field3_finish(
                    f,
                    "ChainPointer",
                    "slot_num",
                    &self.slot_num,
                    "tx_idx",
                    &self.tx_idx,
                    "cert_idx",
                    &&self.cert_idx,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ChainPointer {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ChainPointer {
            #[inline]
            fn eq(&self, other: &ChainPointer) -> bool {
                self.slot_num == other.slot_num && self.tx_idx == other.tx_idx
                    && self.cert_idx == other.cert_idx
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for ChainPointer {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<i64>;
            }
        }
        impl TryFrom<pla::v2::address::ChainPointer> for ChainPointer {
            type Error = DBTypeConversionError;
            fn try_from(
                item: pla::v2::address::ChainPointer,
            ) -> Result<Self, Self::Error> {
                Ok(ChainPointer {
                    slot_num: item
                        .slot_number
                        .0
                        .try_into()
                        .map_err(DBTypeConversionError::BigIntConversion)?,
                    tx_idx: item
                        .transaction_index
                        .0
                        .try_into()
                        .map_err(DBTypeConversionError::BigIntConversion)?,
                    cert_idx: item
                        .certificate_index
                        .0
                        .try_into()
                        .map_err(DBTypeConversionError::BigIntConversion)?,
                })
            }
        }
        impl From<ChainPointer> for pla::v2::address::ChainPointer {
            fn from(item: ChainPointer) -> Self {
                pla::v2::address::ChainPointer {
                    slot_number: pla::v2::address::Slot(BigInt::from(item.slot_num)),
                    transaction_index: pla::v2::address::TransactionIndex(
                        BigInt::from(item.tx_idx),
                    ),
                    certificate_index: pla::v2::address::CertificateIndex(
                        BigInt::from(item.cert_idx),
                    ),
                }
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::ChainPointer, diesel::pg::Pg>
        for ChainPointer {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let (slot_num, tx_idx, cert_idx) = diesel::deserialize::FromSql::from_sql(
                    bytes,
                )?;
                Ok(Self { slot_num, tx_idx, cert_idx })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::ChainPointer, diesel::pg::Pg>
        for ChainPointer {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                diesel::serialize::WriteTuple::<
                    (
                        diesel::sql_types::BigInt,
                        diesel::sql_types::BigInt,
                        diesel::sql_types::BigInt,
                    ),
                >::write_tuple(
                    &(self.slot_num, self.tx_idx, self.cert_idx),
                    &mut out.reborrow(),
                )
            }
        }
        /// StakingCredential
        #[diesel(sql_type = sql_types::StakingCredential)]
        pub struct StakingCredential {
            staking_hash: Option<Credential>,
            staking_ptr: Option<ChainPointer>,
        }
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::StakingCredential>
            for &'__expr StakingCredential {
                type Expression = Bound<sql_types::StakingCredential, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::StakingCredential>>
            for &'__expr StakingCredential {
                type Expression = Bound<Nullable<sql_types::StakingCredential>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::StakingCredential>
            for &'__expr2 &'__expr StakingCredential {
                type Expression = Bound<sql_types::StakingCredential, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::StakingCredential>>
            for &'__expr2 &'__expr StakingCredential {
                type Expression = Bound<Nullable<sql_types::StakingCredential>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<
                __DB,
            > diesel::serialize::ToSql<Nullable<sql_types::StakingCredential>, __DB>
            for StakingCredential
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::StakingCredential, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::StakingCredential, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::StakingCredential> for StakingCredential {
                type Expression = Bound<sql_types::StakingCredential, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::StakingCredential>>
            for StakingCredential {
                type Expression = Bound<Nullable<sql_types::StakingCredential>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for StakingCredential
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for StakingCredential {
            #[inline]
            fn clone(&self) -> StakingCredential {
                StakingCredential {
                    staking_hash: ::core::clone::Clone::clone(&self.staking_hash),
                    staking_ptr: ::core::clone::Clone::clone(&self.staking_ptr),
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for StakingCredential {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field2_finish(
                    f,
                    "StakingCredential",
                    "staking_hash",
                    &self.staking_hash,
                    "staking_ptr",
                    &&self.staking_ptr,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for StakingCredential {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for StakingCredential {
            #[inline]
            fn eq(&self, other: &StakingCredential) -> bool {
                self.staking_hash == other.staking_hash
                    && self.staking_ptr == other.staking_ptr
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for StakingCredential {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Option<Credential>>;
                let _: ::core::cmp::AssertParamIsEq<Option<ChainPointer>>;
            }
        }
        impl TryFrom<pla::v2::address::StakingCredential> for StakingCredential {
            type Error = DBTypeConversionError;
            fn try_from(
                item: pla::v2::address::StakingCredential,
            ) -> Result<Self, Self::Error> {
                Ok(
                    match item {
                        pla::v2::address::StakingCredential::Hash(cred) => {
                            StakingCredential {
                                staking_hash: Some(cred.into()),
                                staking_ptr: None,
                            }
                        }
                        pla::v2::address::StakingCredential::Pointer(ptr) => {
                            StakingCredential {
                                staking_hash: None,
                                staking_ptr: Some(ptr.try_into()?),
                            }
                        }
                    },
                )
            }
        }
        impl TryFrom<StakingCredential> for pla::v2::address::StakingCredential {
            type Error = DBTypeConversionError;
            fn try_from(item: StakingCredential) -> Result<Self, Self::Error> {
                Ok(
                    match item {
                        StakingCredential {
                            staking_hash: Some(cred),
                            staking_ptr: None,
                        } => pla::v2::address::StakingCredential::Hash(cred.try_into()?),
                        StakingCredential {
                            staking_hash: None,
                            staking_ptr: Some(ptr),
                        } => pla::v2::address::StakingCredential::Pointer(ptr.into()),
                        _ => {
                            Err(
                                DBTypeConversionError::InvariantBroken(
                                    "DB StakingCredential must have either 'staking_hash' or 'staking_ptr'"
                                        .to_string(),
                                ),
                            )?
                        }
                    },
                )
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::StakingCredential, diesel::pg::Pg>
        for StakingCredential {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let (staking_hash, staking_ptr) = diesel::deserialize::FromSql::from_sql(
                    bytes,
                )?;
                Ok(Self { staking_hash, staking_ptr })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::StakingCredential, diesel::pg::Pg>
        for StakingCredential {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                diesel::serialize::WriteTuple::<
                    (Nullable<sql_types::Credential>, Nullable<sql_types::ChainPointer>),
                >::write_tuple(
                    &(self.staking_hash.clone(), self.staking_ptr.clone()),
                    &mut out.reborrow(),
                )
            }
        }
        /// Address
        #[diesel(sql_type = sql_types::Address)]
        pub struct Address {
            credential: Credential,
            staking_credential: Option<StakingCredential>,
        }
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::Address> for &'__expr Address {
                type Expression = Bound<sql_types::Address, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::Address>>
            for &'__expr Address {
                type Expression = Bound<Nullable<sql_types::Address>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::Address>
            for &'__expr2 &'__expr Address {
                type Expression = Bound<sql_types::Address, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::Address>>
            for &'__expr2 &'__expr Address {
                type Expression = Bound<Nullable<sql_types::Address>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::Address>, __DB>
            for Address
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::Address, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::Address, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::Address> for Address {
                type Expression = Bound<sql_types::Address, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::Address>> for Address {
                type Expression = Bound<Nullable<sql_types::Address>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for Address
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for Address {
            #[inline]
            fn clone(&self) -> Address {
                Address {
                    credential: ::core::clone::Clone::clone(&self.credential),
                    staking_credential: ::core::clone::Clone::clone(
                        &self.staking_credential,
                    ),
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for Address {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field2_finish(
                    f,
                    "Address",
                    "credential",
                    &self.credential,
                    "staking_credential",
                    &&self.staking_credential,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for Address {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for Address {
            #[inline]
            fn eq(&self, other: &Address) -> bool {
                self.credential == other.credential
                    && self.staking_credential == other.staking_credential
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for Address {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Credential>;
                let _: ::core::cmp::AssertParamIsEq<Option<StakingCredential>>;
            }
        }
        impl TryFrom<pla::v2::address::Address> for Address {
            type Error = DBTypeConversionError;
            fn try_from(item: pla::v2::address::Address) -> Result<Self, Self::Error> {
                Ok(Address {
                    credential: item.credential.into(),
                    staking_credential: item
                        .staking_credential
                        .map(StakingCredential::try_from)
                        .transpose()?,
                })
            }
        }
        impl TryFrom<Address> for pla::v2::address::Address {
            type Error = DBTypeConversionError;
            fn try_from(item: Address) -> Result<Self, Self::Error> {
                Ok(pla::v2::address::Address {
                    credential: item.credential.try_into()?,
                    staking_credential: item
                        .staking_credential
                        .map(pla::v2::address::StakingCredential::try_from)
                        .transpose()?,
                })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::Address, diesel::pg::Pg>
        for Address {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let (credential, staking_credential) = diesel::deserialize::FromSql::from_sql(
                    bytes,
                )?;
                Ok(Self {
                    credential,
                    staking_credential,
                })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::Address, diesel::pg::Pg> for Address {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                diesel::serialize::WriteTuple::<
                    (sql_types::Credential, Nullable<sql_types::StakingCredential>),
                >::write_tuple(
                    &(self.credential.clone(), self.staking_credential.clone()),
                    &mut out.reborrow(),
                )
            }
        }
        /// AssetQuantity
        #[diesel(sql_type = sql_types::AssetQuantity)]
        pub struct AssetQuantity {
            currency_symbol: CurrencySymbol,
            token_name: TokenName,
            amount: i64,
        }
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::AssetQuantity>
            for &'__expr AssetQuantity {
                type Expression = Bound<sql_types::AssetQuantity, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::AssetQuantity>>
            for &'__expr AssetQuantity {
                type Expression = Bound<Nullable<sql_types::AssetQuantity>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::AssetQuantity>
            for &'__expr2 &'__expr AssetQuantity {
                type Expression = Bound<sql_types::AssetQuantity, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::AssetQuantity>>
            for &'__expr2 &'__expr AssetQuantity {
                type Expression = Bound<Nullable<sql_types::AssetQuantity>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::AssetQuantity>, __DB>
            for AssetQuantity
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::AssetQuantity, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::AssetQuantity, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::AssetQuantity> for AssetQuantity {
                type Expression = Bound<sql_types::AssetQuantity, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::AssetQuantity>> for AssetQuantity {
                type Expression = Bound<Nullable<sql_types::AssetQuantity>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for AssetQuantity
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for AssetQuantity {
            #[inline]
            fn clone(&self) -> AssetQuantity {
                AssetQuantity {
                    currency_symbol: ::core::clone::Clone::clone(&self.currency_symbol),
                    token_name: ::core::clone::Clone::clone(&self.token_name),
                    amount: ::core::clone::Clone::clone(&self.amount),
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for AssetQuantity {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field3_finish(
                    f,
                    "AssetQuantity",
                    "currency_symbol",
                    &self.currency_symbol,
                    "token_name",
                    &self.token_name,
                    "amount",
                    &&self.amount,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for AssetQuantity {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for AssetQuantity {
            #[inline]
            fn eq(&self, other: &AssetQuantity) -> bool {
                self.currency_symbol == other.currency_symbol
                    && self.token_name == other.token_name && self.amount == other.amount
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for AssetQuantity {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<CurrencySymbol>;
                let _: ::core::cmp::AssertParamIsEq<TokenName>;
                let _: ::core::cmp::AssertParamIsEq<i64>;
            }
        }
        impl TryFrom<(pla::v2::value::CurrencySymbol, pla::v2::value::TokenName, BigInt)>
        for AssetQuantity {
            type Error = DBTypeConversionError;
            fn try_from(
                item: (pla::v2::value::CurrencySymbol, pla::v2::value::TokenName, BigInt),
            ) -> Result<Self, Self::Error> {
                Ok(AssetQuantity {
                    currency_symbol: item.0.into(),
                    token_name: item.1.into(),
                    amount: item
                        .2
                        .try_into()
                        .map_err(DBTypeConversionError::BigIntConversion)?,
                })
            }
        }
        impl From<AssetQuantity>
        for (pla::v2::value::CurrencySymbol, pla::v2::value::TokenName, BigInt) {
            fn from(item: AssetQuantity) -> Self {
                (item.currency_symbol.into(), item.token_name.into(), item.amount.into())
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::AssetQuantity, diesel::pg::Pg>
        for AssetQuantity {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let (currency_symbol, token_name, amount) = diesel::deserialize::FromSql::from_sql(
                    bytes,
                )?;
                Ok(Self {
                    currency_symbol,
                    token_name,
                    amount,
                })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::AssetQuantity, diesel::pg::Pg>
        for AssetQuantity {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                diesel::serialize::WriteTuple::<
                    (
                        sql_types::CurrencySymbol,
                        sql_types::TokenName,
                        diesel::sql_types::BigInt,
                    ),
                >::write_tuple(
                    &(
                        self.currency_symbol.clone(),
                        self.token_name.clone(),
                        self.amount,
                    ),
                    &mut out.reborrow(),
                )
            }
        }
        /// Value
        #[diesel(sql_type = sql_types::Value)]
        pub struct Value(pub Vec<AssetQuantity>);
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::Value> for &'__expr Value {
                type Expression = Bound<sql_types::Value, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::Value>> for &'__expr Value {
                type Expression = Bound<Nullable<sql_types::Value>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::Value>
            for &'__expr2 &'__expr Value {
                type Expression = Bound<sql_types::Value, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::Value>>
            for &'__expr2 &'__expr Value {
                type Expression = Bound<Nullable<sql_types::Value>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::Value>, __DB>
            for Value
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::Value, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::Value, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::Value> for Value {
                type Expression = Bound<sql_types::Value, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::Value>> for Value {
                type Expression = Bound<Nullable<sql_types::Value>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for Value
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for Value {
            #[inline]
            fn clone(&self) -> Value {
                Value(::core::clone::Clone::clone(&self.0))
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for Value {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Value", &&self.0)
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for Value {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for Value {
            #[inline]
            fn eq(&self, other: &Value) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for Value {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Vec<AssetQuantity>>;
            }
        }
        impl TryFrom<pla::v2::value::Value> for Value {
            type Error = DBTypeConversionError;
            fn try_from(item: pla::v2::value::Value) -> Result<Self, Self::Error> {
                let assets = item
                    .0
                    .iter()
                    .flat_map(|(cs, assets)| {
                        assets
                            .iter()
                            .map(|(tn, amount)| {
                                AssetQuantity::try_from((
                                    cs.to_owned(),
                                    tn.to_owned(),
                                    amount.to_owned(),
                                ))
                            })
                            .collect::<Vec<_>>()
                    })
                    .collect::<Result<Vec<AssetQuantity>, DBTypeConversionError>>()?;
                Ok(Value(assets))
            }
        }
        impl From<Value> for pla::v2::value::Value {
            fn from(item: Value) -> Self {
                item.0
                    .into_iter()
                    .fold(
                        pla::v2::value::Value::new(),
                        |value, AssetQuantity { currency_symbol, token_name, amount }| {
                            value
                                .insert_token(
                                    &currency_symbol.into(),
                                    &token_name.into(),
                                    &amount.into(),
                                )
                        },
                    )
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::Value, diesel::pg::Pg> for Value {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let inner = diesel::deserialize::FromSql::from_sql(bytes)?;
                Ok(Self(inner))
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::Value, diesel::pg::Pg> for Value {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                <Vec<
                    AssetQuantity,
                > as diesel::serialize::ToSql<
                    Array<sql_types::AssetQuantity>,
                    diesel::pg::Pg,
                >>::to_sql(&self.0, &mut out.reborrow())
            }
        }
        /// TransactionInput
        #[diesel(sql_type = sql_types::TransactionInput)]
        pub struct TransactionInput {
            tx_id: TransactionHash,
            tx_idx: i64,
        }
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::TransactionInput>
            for &'__expr TransactionInput {
                type Expression = Bound<sql_types::TransactionInput, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::TransactionInput>>
            for &'__expr TransactionInput {
                type Expression = Bound<Nullable<sql_types::TransactionInput>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::TransactionInput>
            for &'__expr2 &'__expr TransactionInput {
                type Expression = Bound<sql_types::TransactionInput, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::TransactionInput>>
            for &'__expr2 &'__expr TransactionInput {
                type Expression = Bound<Nullable<sql_types::TransactionInput>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<
                __DB,
            > diesel::serialize::ToSql<Nullable<sql_types::TransactionInput>, __DB>
            for TransactionInput
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::TransactionInput, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::TransactionInput, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::TransactionInput> for TransactionInput {
                type Expression = Bound<sql_types::TransactionInput, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::TransactionInput>>
            for TransactionInput {
                type Expression = Bound<Nullable<sql_types::TransactionInput>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for TransactionInput
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for TransactionInput {
            #[inline]
            fn clone(&self) -> TransactionInput {
                TransactionInput {
                    tx_id: ::core::clone::Clone::clone(&self.tx_id),
                    tx_idx: ::core::clone::Clone::clone(&self.tx_idx),
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for TransactionInput {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field2_finish(
                    f,
                    "TransactionInput",
                    "tx_id",
                    &self.tx_id,
                    "tx_idx",
                    &&self.tx_idx,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for TransactionInput {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for TransactionInput {
            #[inline]
            fn eq(&self, other: &TransactionInput) -> bool {
                self.tx_id == other.tx_id && self.tx_idx == other.tx_idx
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for TransactionInput {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<TransactionHash>;
                let _: ::core::cmp::AssertParamIsEq<i64>;
            }
        }
        impl TryFrom<pla::v2::transaction::TransactionInput> for TransactionInput {
            type Error = DBTypeConversionError;
            fn try_from(
                item: pla::v2::transaction::TransactionInput,
            ) -> Result<Self, Self::Error> {
                Ok(TransactionInput {
                    tx_id: item.transaction_id.into(),
                    tx_idx: item
                        .index
                        .try_into()
                        .map_err(DBTypeConversionError::BigIntConversion)?,
                })
            }
        }
        impl From<TransactionInput> for pla::v2::transaction::TransactionInput {
            fn from(item: TransactionInput) -> Self {
                pla::v2::transaction::TransactionInput {
                    transaction_id: item.tx_id.into(),
                    index: item.tx_idx.into(),
                }
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::TransactionInput, diesel::pg::Pg>
        for TransactionInput {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let (tx_id, tx_idx) = diesel::deserialize::FromSql::from_sql(bytes)?;
                Ok(Self { tx_id, tx_idx })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::TransactionInput, diesel::pg::Pg>
        for TransactionInput {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                diesel::serialize::WriteTuple::<
                    (sql_types::TransactionHash, diesel::sql_types::BigInt),
                >::write_tuple(&(self.tx_id.clone(), self.tx_idx), &mut out.reborrow())
            }
        }
        /// OutputDatum
        #[diesel(sql_type = sql_types::OutputDatum)]
        pub struct OutputDatum {
            datum_hash: Option<DatumHash>,
            inline_datum: Option<PlutusData>,
        }
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::OutputDatum> for &'__expr OutputDatum {
                type Expression = Bound<sql_types::OutputDatum, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::OutputDatum>>
            for &'__expr OutputDatum {
                type Expression = Bound<Nullable<sql_types::OutputDatum>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::OutputDatum>
            for &'__expr2 &'__expr OutputDatum {
                type Expression = Bound<sql_types::OutputDatum, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::OutputDatum>>
            for &'__expr2 &'__expr OutputDatum {
                type Expression = Bound<Nullable<sql_types::OutputDatum>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::OutputDatum>, __DB>
            for OutputDatum
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::OutputDatum, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::OutputDatum, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::OutputDatum> for OutputDatum {
                type Expression = Bound<sql_types::OutputDatum, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::OutputDatum>> for OutputDatum {
                type Expression = Bound<Nullable<sql_types::OutputDatum>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for OutputDatum
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for OutputDatum {
            #[inline]
            fn clone(&self) -> OutputDatum {
                OutputDatum {
                    datum_hash: ::core::clone::Clone::clone(&self.datum_hash),
                    inline_datum: ::core::clone::Clone::clone(&self.inline_datum),
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for OutputDatum {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field2_finish(
                    f,
                    "OutputDatum",
                    "datum_hash",
                    &self.datum_hash,
                    "inline_datum",
                    &&self.inline_datum,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for OutputDatum {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for OutputDatum {
            #[inline]
            fn eq(&self, other: &OutputDatum) -> bool {
                self.datum_hash == other.datum_hash
                    && self.inline_datum == other.inline_datum
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for OutputDatum {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Option<DatumHash>>;
                let _: ::core::cmp::AssertParamIsEq<Option<PlutusData>>;
            }
        }
        impl TryFrom<pla::v2::datum::OutputDatum> for OutputDatum {
            type Error = DBTypeConversionError;
            fn try_from(item: pla::v2::datum::OutputDatum) -> Result<Self, Self::Error> {
                Ok(
                    match item {
                        pla::v2::datum::OutputDatum::DatumHash(dh) => {
                            OutputDatum {
                                datum_hash: Some(dh.into()),
                                inline_datum: None,
                            }
                        }
                        pla::v2::datum::OutputDatum::InlineDatum(
                            pla::v2::datum::Datum(datum),
                        ) => {
                            OutputDatum {
                                datum_hash: None,
                                inline_datum: Some(datum.try_into()?),
                            }
                        }
                        pla::v2::datum::OutputDatum::None => {
                            OutputDatum {
                                datum_hash: None,
                                inline_datum: None,
                            }
                        }
                    },
                )
            }
        }
        impl TryFrom<OutputDatum> for pla::v2::datum::OutputDatum {
            type Error = DBTypeConversionError;
            fn try_from(item: OutputDatum) -> Result<Self, Self::Error> {
                Ok(
                    match item {
                        OutputDatum { datum_hash: Some(dh_db), .. } => {
                            pla::v2::datum::OutputDatum::DatumHash(dh_db.into())
                        }
                        OutputDatum { inline_datum: Some(datum_db), .. } => {
                            pla::v2::datum::OutputDatum::InlineDatum(
                                pla::v2::datum::Datum(datum_db.try_into()?),
                            )
                        }
                        _ => pla::v2::datum::OutputDatum::None,
                    },
                )
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::OutputDatum, diesel::pg::Pg>
        for OutputDatum {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let (datum_hash, inline_datum) = diesel::deserialize::FromSql::from_sql(
                    bytes,
                )?;
                Ok(Self { datum_hash, inline_datum })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::OutputDatum, diesel::pg::Pg>
        for OutputDatum {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                diesel::serialize::WriteTuple::<
                    (Nullable<sql_types::DatumHash>, Nullable<sql_types::PlutusData>),
                >::write_tuple(
                    &(self.datum_hash.clone(), self.inline_datum.clone()),
                    &mut out.reborrow(),
                )
            }
        }
        /// TransactionOutput
        #[diesel(sql_type = sql_types::TransactionOutput)]
        pub struct TransactionOutput {
            address: Address,
            assets: Value,
            datum: OutputDatum,
            reference_script: Option<ScriptHash>,
        }
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::TransactionOutput>
            for &'__expr TransactionOutput {
                type Expression = Bound<sql_types::TransactionOutput, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::TransactionOutput>>
            for &'__expr TransactionOutput {
                type Expression = Bound<Nullable<sql_types::TransactionOutput>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::TransactionOutput>
            for &'__expr2 &'__expr TransactionOutput {
                type Expression = Bound<sql_types::TransactionOutput, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::TransactionOutput>>
            for &'__expr2 &'__expr TransactionOutput {
                type Expression = Bound<Nullable<sql_types::TransactionOutput>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<
                __DB,
            > diesel::serialize::ToSql<Nullable<sql_types::TransactionOutput>, __DB>
            for TransactionOutput
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::TransactionOutput, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::TransactionOutput, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::TransactionOutput> for TransactionOutput {
                type Expression = Bound<sql_types::TransactionOutput, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::TransactionOutput>>
            for TransactionOutput {
                type Expression = Bound<Nullable<sql_types::TransactionOutput>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for TransactionOutput
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for TransactionOutput {
            #[inline]
            fn clone(&self) -> TransactionOutput {
                TransactionOutput {
                    address: ::core::clone::Clone::clone(&self.address),
                    assets: ::core::clone::Clone::clone(&self.assets),
                    datum: ::core::clone::Clone::clone(&self.datum),
                    reference_script: ::core::clone::Clone::clone(&self.reference_script),
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for TransactionOutput {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field4_finish(
                    f,
                    "TransactionOutput",
                    "address",
                    &self.address,
                    "assets",
                    &self.assets,
                    "datum",
                    &self.datum,
                    "reference_script",
                    &&self.reference_script,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for TransactionOutput {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for TransactionOutput {
            #[inline]
            fn eq(&self, other: &TransactionOutput) -> bool {
                self.address == other.address && self.assets == other.assets
                    && self.datum == other.datum
                    && self.reference_script == other.reference_script
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for TransactionOutput {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<Address>;
                let _: ::core::cmp::AssertParamIsEq<Value>;
                let _: ::core::cmp::AssertParamIsEq<OutputDatum>;
                let _: ::core::cmp::AssertParamIsEq<Option<ScriptHash>>;
            }
        }
        impl TryFrom<pla::v2::transaction::TransactionOutput> for TransactionOutput {
            type Error = DBTypeConversionError;
            fn try_from(
                item: pla::v2::transaction::TransactionOutput,
            ) -> Result<Self, Self::Error> {
                Ok(TransactionOutput {
                    address: item.address.try_into()?,
                    assets: item.value.try_into()?,
                    datum: item.datum.try_into()?,
                    reference_script: item.reference_script.map(ScriptHash::from),
                })
            }
        }
        impl TryFrom<TransactionOutput> for pla::v2::transaction::TransactionOutput {
            type Error = DBTypeConversionError;
            fn try_from(item: TransactionOutput) -> Result<Self, Self::Error> {
                Ok(pla::v2::transaction::TransactionOutput {
                    address: item.address.try_into()?,
                    value: item.assets.into(),
                    datum: item.datum.try_into()?,
                    reference_script: item
                        .reference_script
                        .map(pla::v2::script::ScriptHash::from),
                })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::TransactionOutput, diesel::pg::Pg>
        for TransactionOutput {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let (address, assets, datum, reference_script) = diesel::deserialize::FromSql::from_sql(
                    bytes,
                )?;
                Ok(Self {
                    address,
                    assets,
                    datum,
                    reference_script,
                })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::TransactionOutput, diesel::pg::Pg>
        for TransactionOutput {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                diesel::serialize::WriteTuple::<
                    (
                        sql_types::Address,
                        sql_types::Value,
                        sql_types::OutputDatum,
                        Nullable<sql_types::ScriptHash>,
                    ),
                >::write_tuple(
                    &(
                        self.address.clone(),
                        self.assets.clone(),
                        self.datum.clone(),
                        self.reference_script.clone(),
                    ),
                    &mut out.reborrow(),
                )
            }
        }
        /// TxInInfo
        #[diesel(sql_type = sql_types::TxInInfo)]
        pub struct TxInInfo {
            reference: TransactionInput,
            output: TransactionOutput,
        }
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::expression::AsExpression;
            use diesel::internal::derives::as_expression::Bound;
            use diesel::sql_types::Nullable;
            use diesel::serialize::{self, ToSql, Output};
            impl<'__expr> AsExpression<sql_types::TxInInfo> for &'__expr TxInInfo {
                type Expression = Bound<sql_types::TxInInfo, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr> AsExpression<Nullable<sql_types::TxInInfo>>
            for &'__expr TxInInfo {
                type Expression = Bound<Nullable<sql_types::TxInInfo>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<sql_types::TxInInfo>
            for &'__expr2 &'__expr TxInInfo {
                type Expression = Bound<sql_types::TxInInfo, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<'__expr, '__expr2> AsExpression<Nullable<sql_types::TxInInfo>>
            for &'__expr2 &'__expr TxInInfo {
                type Expression = Bound<Nullable<sql_types::TxInInfo>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl<__DB> diesel::serialize::ToSql<Nullable<sql_types::TxInInfo>, __DB>
            for TxInInfo
            where
                __DB: diesel::backend::Backend,
                Self: ToSql<sql_types::TxInInfo, __DB>,
            {
                fn to_sql<'__b>(
                    &'__b self,
                    out: &mut Output<'__b, '_, __DB>,
                ) -> serialize::Result {
                    ToSql::<sql_types::TxInInfo, __DB>::to_sql(self, out)
                }
            }
            impl AsExpression<sql_types::TxInInfo> for TxInInfo {
                type Expression = Bound<sql_types::TxInInfo, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
            impl AsExpression<Nullable<sql_types::TxInInfo>> for TxInInfo {
                type Expression = Bound<Nullable<sql_types::TxInInfo>, Self>;
                fn as_expression(self) -> Self::Expression {
                    Bound::new(self)
                }
            }
        };
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::deserialize::{self, FromSql, Queryable};
            impl<__DB, __ST> Queryable<__ST, __DB> for TxInInfo
            where
                __DB: diesel::backend::Backend,
                __ST: diesel::sql_types::SingleValue,
                Self: FromSql<__ST, __DB>,
            {
                type Row = Self;
                fn build(row: Self::Row) -> deserialize::Result<Self> {
                    Ok(row)
                }
            }
        };
        #[automatically_derived]
        impl ::core::clone::Clone for TxInInfo {
            #[inline]
            fn clone(&self) -> TxInInfo {
                TxInInfo {
                    reference: ::core::clone::Clone::clone(&self.reference),
                    output: ::core::clone::Clone::clone(&self.output),
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for TxInInfo {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field2_finish(
                    f,
                    "TxInInfo",
                    "reference",
                    &self.reference,
                    "output",
                    &&self.output,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for TxInInfo {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for TxInInfo {
            #[inline]
            fn eq(&self, other: &TxInInfo) -> bool {
                self.reference == other.reference && self.output == other.output
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for TxInInfo {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<TransactionInput>;
                let _: ::core::cmp::AssertParamIsEq<TransactionOutput>;
            }
        }
        impl TryFrom<pla::v2::transaction::TxInInfo> for TxInInfo {
            type Error = DBTypeConversionError;
            fn try_from(
                item: pla::v2::transaction::TxInInfo,
            ) -> Result<Self, Self::Error> {
                Ok(TxInInfo {
                    reference: item.reference.try_into()?,
                    output: item.output.try_into()?,
                })
            }
        }
        impl TryFrom<TxInInfo> for pla::v2::transaction::TxInInfo {
            type Error = DBTypeConversionError;
            fn try_from(item: TxInInfo) -> Result<Self, Self::Error> {
                Ok(pla::v2::transaction::TxInInfo {
                    reference: item.reference.into(),
                    output: item.output.try_into()?,
                })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::deserialize::FromSql<sql_types::TxInInfo, diesel::pg::Pg>
        for TxInInfo {
            fn from_sql(
                bytes: diesel::pg::PgValue,
            ) -> diesel::deserialize::Result<Self> {
                let (reference, output) = diesel::deserialize::FromSql::from_sql(bytes)?;
                Ok(Self { reference, output })
            }
        }
        #[cfg(feature = "diesel")]
        impl diesel::serialize::ToSql<sql_types::TxInInfo, diesel::pg::Pg> for TxInInfo {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                diesel::serialize::WriteTuple::<
                    (sql_types::TransactionInput, sql_types::TransactionOutput),
                >::write_tuple(
                    &(self.reference.clone(), self.output.clone()),
                    &mut out.reborrow(),
                )
            }
        }
    }
}
pub mod error {
    /// Specify what the indexer event handler should do for specific errors. See: `ErrorPolicyProvider`.
    /// The idea is that an error type, `E`, implements `ErrorPolicyProvider`.
    /// Based on the different variants of `E`, different `ErrorPolicy` can be returned, which influences
    /// the behavior of the event handler.
    pub enum ErrorPolicy<E> {
        /// Indicate the callback operation should be retried. Also see: `RetryPolicy`.
        Retry,
        /// Indicate that the error should be ignored, go to next event.
        Skip,
        /// Indicate that the event handler should exit with error.
        Exit,
        /// Indicate that the event handler should call given error handling function with the error.
        Call(fn(E) -> ()),
    }
    /// Trait that can be implemented for custom error types.
    /// Different variants in said error types can then be given different `ErrorPolicy` assignments.
    pub trait ErrorPolicyProvider
    where
        Self: Sized,
    {
        fn get_error_policy(&self) -> ErrorPolicy<Self>;
    }
}
pub mod filter {
    use oura::filters::selection::{Config, Predicate};
    use plutus_ledger_api::v2::{script::MintingPolicyHash, value::CurrencySymbol};
    use tx_bakery::csl;
    use tx_bakery::utils::pla_to_csl::TryFromPLAWithDef;
    /// Interesting transaction components to look for when filtering transactions
    /// relevant to the protocol.
    /// Set curr_symbols to empty vectors to handle any transaction event indiscriminately.
    pub struct Filter {
        pub curr_symbols: Vec<CurrencySymbol>,
    }
    impl Filter {
        pub fn to_selection_config(self) -> Config {
            Config {
                check: Predicate::AnyOf(
                    <[_]>::into_vec(
                        #[rustc_box]
                        ::alloc::boxed::Box::new([
                            Predicate::VariantIn(
                                <[_]>::into_vec(
                                    #[rustc_box]
                                    ::alloc::boxed::Box::new([
                                        "RollBack".to_string(),
                                        "Block".to_string(),
                                    ]),
                                ),
                            ),
                            Predicate::AllOf(
                                <[_]>::into_vec(
                                    #[rustc_box]
                                    ::alloc::boxed::Box::new([
                                        Predicate::VariantIn(
                                            <[_]>::into_vec(
                                                #[rustc_box]
                                                ::alloc::boxed::Box::new(["Transaction".to_string()]),
                                            ),
                                        ),
                                        if self.curr_symbols.is_empty() {
                                            ALWAYS_TRUE
                                        } else {
                                            Predicate::AnyOf(
                                                self
                                                    .curr_symbols
                                                    .into_iter()
                                                    .map(serialize_cur_sym)
                                                    .map(Predicate::PolicyEquals)
                                                    .collect(),
                                            )
                                        },
                                    ]),
                                ),
                            ),
                        ]),
                    ),
                ),
            }
        }
    }
    const ALWAYS_TRUE: Predicate = Predicate::AllOf(::alloc::vec::Vec::new());
    fn serialize_cur_sym(cur_sym: CurrencySymbol) -> String {
        match cur_sym {
            CurrencySymbol::Ada => String::new(),
            CurrencySymbol::NativeToken(MintingPolicyHash(script_hash)) => {
                csl::ScriptHash::try_from_pla(&script_hash).unwrap().to_hex()
            }
        }
    }
}
pub(crate) mod from_oura {
    use ::oura::model::{MintRecord, OutputAssetRecord};
    use anyhow::Context;
    use data_encoding::HEXLOWER;
    use num_bigint::BigInt;
    use plutus_ledger_api::v2::{
        address::Address, crypto::LedgerBytes, datum::{Datum, DatumHash},
        script::{MintingPolicyHash, ScriptHash},
        transaction::TransactionHash, value::{CurrencySymbol, TokenName, Value},
    };
    use std::fmt::Debug;
    use tx_bakery::csl;
    use tx_bakery::utils::csl_to_pla::TryToPLA;
    pub enum OuraParseError {
        #[error(transparent)]
        ParseError(#[from] anyhow::Error),
        #[error("Unable to convert current time: {0}")]
        TimeConversionError(tx_bakery::error::Error),
    }
    #[allow(unused_qualifications)]
    impl std::error::Error for OuraParseError {
        fn source(&self) -> ::core::option::Option<&(dyn std::error::Error + 'static)> {
            use thiserror::__private::AsDynError as _;
            #[allow(deprecated)]
            match self {
                OuraParseError::ParseError { 0: transparent } => {
                    std::error::Error::source(transparent.as_dyn_error())
                }
                OuraParseError::TimeConversionError { .. } => {
                    ::core::option::Option::None
                }
            }
        }
    }
    #[allow(unused_qualifications)]
    impl ::core::fmt::Display for OuraParseError {
        fn fmt(&self, __formatter: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            use thiserror::__private::AsDisplay as _;
            #[allow(unused_variables, deprecated, clippy::used_underscore_binding)]
            match self {
                OuraParseError::ParseError(_0) => {
                    ::core::fmt::Display::fmt(_0, __formatter)
                }
                OuraParseError::TimeConversionError(_0) => {
                    __formatter
                        .write_fmt(
                            format_args!(
                                "Unable to convert current time: {0}",
                                _0.as_display(),
                            ),
                        )
                }
            }
        }
    }
    #[allow(unused_qualifications)]
    impl ::core::convert::From<anyhow::Error> for OuraParseError {
        #[allow(deprecated)]
        fn from(source: anyhow::Error) -> Self {
            OuraParseError::ParseError {
                0: source,
            }
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for OuraParseError {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                OuraParseError::ParseError(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "ParseError",
                        &__self_0,
                    )
                }
                OuraParseError::TimeConversionError(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TimeConversionError",
                        &__self_0,
                    )
                }
            }
        }
    }
    /// Convert an Oura transaction record type to its plutus-ledger-api counterpart
    pub trait FromOura<T> {
        fn from_oura(value: T) -> Result<Self, OuraParseError>
        where
            Self: Sized;
    }
    impl FromOura<String> for LedgerBytes {
        fn from_oura(value: String) -> Result<Self, OuraParseError> {
            Ok(
                LedgerBytes(
                    HEXLOWER
                        .decode(&value.clone().into_bytes()[..])
                        .with_context(|| "Parsing LedgerBytes from Oura")?,
                ),
            )
        }
    }
    impl FromOura<String> for TransactionHash {
        fn from_oura(value: String) -> Result<Self, OuraParseError> {
            Ok(
                TransactionHash(
                    LedgerBytes::from_oura(value)
                        .with_context(|| "Parsing TransactionHash from Oura")?,
                ),
            )
        }
    }
    impl FromOura<String> for DatumHash {
        fn from_oura(value: String) -> Result<Self, OuraParseError> {
            Ok(DatumHash(LedgerBytes::from_oura(value)?))
        }
    }
    impl FromOura<String> for CurrencySymbol {
        fn from_oura(value: String) -> Result<Self, OuraParseError> {
            Ok(
                if value.is_empty() {
                    CurrencySymbol::Ada
                } else {
                    CurrencySymbol::NativeToken(
                        MintingPolicyHash(ScriptHash(LedgerBytes::from_oura(value)?)),
                    )
                },
            )
        }
    }
    impl FromOura<String> for TokenName {
        fn from_oura(value: String) -> Result<Self, OuraParseError> {
            Ok(
                if value.is_empty() {
                    TokenName::ada()
                } else {
                    TokenName(LedgerBytes::from_oura(value)?)
                },
            )
        }
    }
    impl FromOura<serde_json::Value> for Datum {
        fn from_oura(value: serde_json::Value) -> Result<Self, OuraParseError> {
            let csl_plutus_data = csl::encode_json_value_to_plutus_datum(
                    value,
                    csl::PlutusDatumSchema::DetailedSchema,
                )
                .with_context(|| "Parsing Datum from Oura")?;
            Ok(
                Datum(
                    csl_plutus_data
                        .try_to_pla()
                        .with_context(|| "Parsing Datum from Oura")?,
                ),
            )
        }
    }
    impl FromOura<String> for Address {
        fn from_oura(value: String) -> Result<Self, OuraParseError> {
            let csl_addr = csl::Address::from_bech32(&value)
                .or_else(|_| {
                    csl::ByronAddress::from_base58(&value)
                        .map(|byron_addr| byron_addr.to_address())
                })
                .with_context(|| "Parsing Address from Oura")?;
            Ok(csl_addr.try_to_pla().with_context(|| "Parsing Address from Oura")?)
        }
    }
    impl FromOura<Vec<OutputAssetRecord>> for Value {
        fn from_oura(value: Vec<OutputAssetRecord>) -> Result<Self, OuraParseError> {
            value
                .iter()
                .try_fold(
                    Value::new(),
                    |acc, x| {
                        let amt = BigInt::from(x.amount);
                        Ok(
                            acc
                                .insert_token(
                                    &CurrencySymbol::from_oura(x.policy.clone())?,
                                    &TokenName::from_oura(x.asset.clone())?,
                                    &amt,
                                ),
                        )
                    },
                )
        }
    }
    impl FromOura<Vec<MintRecord>> for Value {
        fn from_oura(value: Vec<MintRecord>) -> Result<Self, OuraParseError> {
            value
                .iter()
                .try_fold(
                    Value::new(),
                    |acc, x| {
                        let amt = BigInt::from(x.quantity);
                        Ok(
                            acc
                                .insert_token(
                                    &CurrencySymbol::from_oura(x.policy.clone())?,
                                    &TokenName::from_oura(x.asset.clone())?,
                                    &amt,
                                ),
                        )
                    },
                )
        }
    }
}
pub mod handler {
    pub mod callback {
        use crate::{
            error::ErrorPolicyProvider,
            handler::{chain_event::ChainEvent, retry::{perform_with_retry, RetryPolicy}},
            progress_tracker::ProgressTracker,
        };
        use oura::{
            pipelining::{BootstrapResult, SinkProvider, StageReceiver},
            utils::Utils,
        };
        use std::{future::Future, sync::Arc};
        use strum_macros::Display;
        use tokio::runtime::Runtime;
        use tracing::{event, span, Instrument, Level};
        pub trait EventHandler
        where
            Self: Clone + Send + 'static,
        {
            type Error: std::error::Error + ErrorPolicyProvider;
            fn handle(
                &self,
                event: ChainEvent,
            ) -> impl Future<Output = Result<(), Self::Error>>;
        }
        /// This is a custom made sink for Oura. Based on a callback function.
        /// The idea is similar to a webhook, but instead of calling a web endpoint - we call a function directly.
        pub(crate) struct Callback<H: EventHandler> {
            pub(crate) handler: H,
            pub(crate) retry_policy: RetryPolicy,
            pub(crate) utils: Arc<Utils>,
            pub(crate) progress_tracker: Option<ProgressTracker>,
        }
        impl<H: EventHandler> Callback<H> {
            pub fn new(
                handler: H,
                retry_policy: RetryPolicy,
                utils: Arc<Utils>,
                progress_tracker: Option<ProgressTracker>,
            ) -> Self {
                Self {
                    handler,
                    retry_policy,
                    utils,
                    progress_tracker,
                }
            }
        }
        impl<H: EventHandler> SinkProvider for Callback<H> {
            fn bootstrap(&self, input: StageReceiver) -> BootstrapResult {
                let span = {
                    use ::tracing::__macro_support::Callsite as _;
                    static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                        static META: ::tracing::Metadata<'static> = {
                            ::tracing_core::metadata::Metadata::new(
                                "Callback::bootstrap",
                                "tx_indexer::handler::callback",
                                Level::DEBUG,
                                ::core::option::Option::Some("src/handler/callback.rs"),
                                ::core::option::Option::Some(54u32),
                                ::core::option::Option::Some(
                                    "tx_indexer::handler::callback",
                                ),
                                ::tracing_core::field::FieldSet::new(
                                    &[],
                                    ::tracing_core::callsite::Identifier(&__CALLSITE),
                                ),
                                ::tracing::metadata::Kind::SPAN,
                            )
                        };
                        ::tracing::callsite::DefaultCallsite::new(&META)
                    };
                    let mut interest = ::tracing::subscriber::Interest::never();
                    if Level::DEBUG <= ::tracing::level_filters::STATIC_MAX_LEVEL
                        && Level::DEBUG
                            <= ::tracing::level_filters::LevelFilter::current()
                        && {
                            interest = __CALLSITE.interest();
                            !interest.is_never()
                        }
                        && ::tracing::__macro_support::__is_enabled(
                            __CALLSITE.metadata(),
                            interest,
                        )
                    {
                        let meta = __CALLSITE.metadata();
                        ::tracing::Span::new(meta, &{ meta.fields().value_set(&[]) })
                    } else {
                        let span = ::tracing::__macro_support::__disabled_span(
                            __CALLSITE.metadata(),
                        );
                        {};
                        span
                    }
                };
                let _enter = span.enter();
                let retry_policy = self.retry_policy;
                let utils = self.utils.clone();
                let handler = self.handler.clone();
                let progress_tracker = self.progress_tracker.clone();
                let handle = {
                    use ::tracing::__macro_support::Callsite as _;
                    static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                        static META: ::tracing::Metadata<'static> = {
                            ::tracing_core::metadata::Metadata::new(
                                "SpawningThread",
                                "tx_indexer::handler::callback",
                                Level::DEBUG,
                                ::core::option::Option::Some("src/handler/callback.rs"),
                                ::core::option::Option::Some(62u32),
                                ::core::option::Option::Some(
                                    "tx_indexer::handler::callback",
                                ),
                                ::tracing_core::field::FieldSet::new(
                                    &[],
                                    ::tracing_core::callsite::Identifier(&__CALLSITE),
                                ),
                                ::tracing::metadata::Kind::SPAN,
                            )
                        };
                        ::tracing::callsite::DefaultCallsite::new(&META)
                    };
                    let mut interest = ::tracing::subscriber::Interest::never();
                    if Level::DEBUG <= ::tracing::level_filters::STATIC_MAX_LEVEL
                        && Level::DEBUG
                            <= ::tracing::level_filters::LevelFilter::current()
                        && {
                            interest = __CALLSITE.interest();
                            !interest.is_never()
                        }
                        && ::tracing::__macro_support::__is_enabled(
                            __CALLSITE.metadata(),
                            interest,
                        )
                    {
                        let meta = __CALLSITE.metadata();
                        ::tracing::Span::new(meta, &{ meta.fields().value_set(&[]) })
                    } else {
                        let span = ::tracing::__macro_support::__disabled_span(
                            __CALLSITE.metadata(),
                        );
                        {};
                        span
                    }
                }
                    .in_scope(|| {
                        std::thread::spawn(move || {
                            let span = {
                                use ::tracing::__macro_support::Callsite as _;
                                static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                    static META: ::tracing::Metadata<'static> = {
                                        ::tracing_core::metadata::Metadata::new(
                                            "EventHandlingThread",
                                            "tx_indexer::handler::callback",
                                            Level::DEBUG,
                                            ::core::option::Option::Some("src/handler/callback.rs"),
                                            ::core::option::Option::Some(64u32),
                                            ::core::option::Option::Some(
                                                "tx_indexer::handler::callback",
                                            ),
                                            ::tracing_core::field::FieldSet::new(
                                                &[],
                                                ::tracing_core::callsite::Identifier(&__CALLSITE),
                                            ),
                                            ::tracing::metadata::Kind::SPAN,
                                        )
                                    };
                                    ::tracing::callsite::DefaultCallsite::new(&META)
                                };
                                let mut interest = ::tracing::subscriber::Interest::never();
                                if Level::DEBUG
                                    <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                    && Level::DEBUG
                                        <= ::tracing::level_filters::LevelFilter::current()
                                    && {
                                        interest = __CALLSITE.interest();
                                        !interest.is_never()
                                    }
                                    && ::tracing::__macro_support::__is_enabled(
                                        __CALLSITE.metadata(),
                                        interest,
                                    )
                                {
                                    let meta = __CALLSITE.metadata();
                                    ::tracing::Span::new(
                                        meta,
                                        &{ meta.fields().value_set(&[]) },
                                    )
                                } else {
                                    let span = ::tracing::__macro_support::__disabled_span(
                                        __CALLSITE.metadata(),
                                    );
                                    {};
                                    span
                                }
                            };
                            let _enter = span.enter();
                            let rt = Runtime::new().unwrap();
                            rt.block_on(
                                    handle_event(
                                        handler,
                                        input,
                                        &retry_policy,
                                        utils,
                                        progress_tracker,
                                    ),
                                )
                                .map_err(|err| {
                                    {
                                        use ::tracing::__macro_support::Callsite as _;
                                        static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                            static META: ::tracing::Metadata<'static> = {
                                                ::tracing_core::metadata::Metadata::new(
                                                    "event src/handler/callback.rs:77",
                                                    "tx_indexer::handler::callback",
                                                    Level::ERROR,
                                                    ::core::option::Option::Some("src/handler/callback.rs"),
                                                    ::core::option::Option::Some(77u32),
                                                    ::core::option::Option::Some(
                                                        "tx_indexer::handler::callback",
                                                    ),
                                                    ::tracing_core::field::FieldSet::new(
                                                        &["label", "err"],
                                                        ::tracing_core::callsite::Identifier(&__CALLSITE),
                                                    ),
                                                    ::tracing::metadata::Kind::EVENT,
                                                )
                                            };
                                            ::tracing::callsite::DefaultCallsite::new(&META)
                                        };
                                        let enabled = Level::ERROR
                                            <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                            && Level::ERROR
                                                <= ::tracing::level_filters::LevelFilter::current()
                                            && {
                                                let interest = __CALLSITE.interest();
                                                !interest.is_never()
                                                    && ::tracing::__macro_support::__is_enabled(
                                                        __CALLSITE.metadata(),
                                                        interest,
                                                    )
                                            };
                                        if enabled {
                                            (|value_set: ::tracing::field::ValueSet| {
                                                let meta = __CALLSITE.metadata();
                                                ::tracing::Event::dispatch(meta, &value_set);
                                            })({
                                                #[allow(unused_imports)]
                                                use ::tracing::field::{debug, display, Value};
                                                let mut iter = __CALLSITE.metadata().fields().iter();
                                                __CALLSITE
                                                    .metadata()
                                                    .fields()
                                                    .value_set(
                                                        &[
                                                            (
                                                                &::core::iter::Iterator::next(&mut iter)
                                                                    .expect("FieldSet corrupted (this is a bug)"),
                                                                ::core::option::Option::Some(
                                                                    &display(&Events::EventHandlerFailure) as &dyn Value,
                                                                ),
                                                            ),
                                                            (
                                                                &::core::iter::Iterator::next(&mut iter)
                                                                    .expect("FieldSet corrupted (this is a bug)"),
                                                                ::core::option::Option::Some(&debug(&err) as &dyn Value),
                                                            ),
                                                        ],
                                                    )
                                            });
                                        } else {
                                        }
                                    };
                                    err
                                })
                                .expect("request loop failed");
                        })
                    });
                Ok(handle)
            }
        }
        async fn handle_event<'a, H: EventHandler>(
            handler: H,
            input: StageReceiver,
            retry_policy: &RetryPolicy,
            utils: Arc<Utils>,
            mut progress_tracker: Option<ProgressTracker>,
        ) -> Result<(), H::Error> {
            let span = {
                use ::tracing::__macro_support::Callsite as _;
                static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                    static META: ::tracing::Metadata<'static> = {
                        ::tracing_core::metadata::Metadata::new(
                            "handle_event",
                            "tx_indexer::handler::callback",
                            Level::DEBUG,
                            ::core::option::Option::Some("src/handler/callback.rs"),
                            ::core::option::Option::Some(96u32),
                            ::core::option::Option::Some(
                                "tx_indexer::handler::callback",
                            ),
                            ::tracing_core::field::FieldSet::new(
                                &[],
                                ::tracing_core::callsite::Identifier(&__CALLSITE),
                            ),
                            ::tracing::metadata::Kind::SPAN,
                        )
                    };
                    ::tracing::callsite::DefaultCallsite::new(&META)
                };
                let mut interest = ::tracing::subscriber::Interest::never();
                if Level::DEBUG <= ::tracing::level_filters::STATIC_MAX_LEVEL
                    && Level::DEBUG <= ::tracing::level_filters::LevelFilter::current()
                    && {
                        interest = __CALLSITE.interest();
                        !interest.is_never()
                    }
                    && ::tracing::__macro_support::__is_enabled(
                        __CALLSITE.metadata(),
                        interest,
                    )
                {
                    let meta = __CALLSITE.metadata();
                    ::tracing::Span::new(meta, &{ meta.fields().value_set(&[]) })
                } else {
                    let span = ::tracing::__macro_support::__disabled_span(
                        __CALLSITE.metadata(),
                    );
                    {};
                    span
                }
            };
            let _enter = span.enter();
            for chain_event in input.into_iter() {
                let span = {
                    use ::tracing::__macro_support::Callsite as _;
                    static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                        static META: ::tracing::Metadata<'static> = {
                            ::tracing_core::metadata::Metadata::new(
                                "HandlingEvent",
                                "tx_indexer::handler::callback",
                                Level::DEBUG,
                                ::core::option::Option::Some("src/handler/callback.rs"),
                                ::core::option::Option::Some(99u32),
                                ::core::option::Option::Some(
                                    "tx_indexer::handler::callback",
                                ),
                                ::tracing_core::field::FieldSet::new(
                                    &["context"],
                                    ::tracing_core::callsite::Identifier(&__CALLSITE),
                                ),
                                ::tracing::metadata::Kind::SPAN,
                            )
                        };
                        ::tracing::callsite::DefaultCallsite::new(&META)
                    };
                    let mut interest = ::tracing::subscriber::Interest::never();
                    if Level::DEBUG <= ::tracing::level_filters::STATIC_MAX_LEVEL
                        && Level::DEBUG
                            <= ::tracing::level_filters::LevelFilter::current()
                        && {
                            interest = __CALLSITE.interest();
                            !interest.is_never()
                        }
                        && ::tracing::__macro_support::__is_enabled(
                            __CALLSITE.metadata(),
                            interest,
                        )
                    {
                        let meta = __CALLSITE.metadata();
                        ::tracing::Span::new(
                            meta,
                            &{
                                #[allow(unused_imports)]
                                use ::tracing::field::{debug, display, Value};
                                let mut iter = meta.fields().iter();
                                meta.fields()
                                    .value_set(
                                        &[
                                            (
                                                &::core::iter::Iterator::next(&mut iter)
                                                    .expect("FieldSet corrupted (this is a bug)"),
                                                ::core::option::Option::Some(
                                                    &debug(&chain_event.context) as &dyn Value,
                                                ),
                                            ),
                                        ],
                                    )
                            },
                        )
                    } else {
                        let span = ::tracing::__macro_support::__disabled_span(
                            __CALLSITE.metadata(),
                        );
                        {};
                        span
                    }
                };
                perform_with_retry(
                        &handler,
                        chain_event.clone(),
                        retry_policy,
                        &mut progress_tracker,
                    )
                    .instrument(span)
                    .await
                    .map(|_| utils.track_sink_progress(&chain_event))?;
            }
            Ok(())
        }
        pub enum Events {
            EventHandlerFailure,
        }
        impl ::core::fmt::Display for Events {
            fn fmt(
                &self,
                f: &mut ::core::fmt::Formatter,
            ) -> ::core::result::Result<(), ::core::fmt::Error> {
                match *self {
                    Events::EventHandlerFailure => f.pad("EventHandlerFailure"),
                }
            }
        }
    }
    pub mod chain_event {
        use crate::{
            from_oura::{FromOura, OuraParseError},
            progress_tracker::ProgressTracker,
        };
        use num_bigint::BigInt;
        use oura::model as oura;
        use plutus_ledger_api::v2::{
            address::Address, datum::{Datum, DatumHash, OutputDatum},
            transaction::{
                TransactionHash, TransactionInput, TransactionOutput, TxInInfo,
            },
            value::Value,
        };
        use std::fmt::Debug;
        use std::{collections::HashMap, sync::atomic::Ordering};
        use tracing::{event, Level};
        /// Indication of when an event happened in the context of the chain.
        pub struct ChainEventTime {
            pub block_number: u64,
            pub block_hash: String,
            pub slot: u64,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for ChainEventTime {
            #[inline]
            fn clone(&self) -> ChainEventTime {
                ChainEventTime {
                    block_number: ::core::clone::Clone::clone(&self.block_number),
                    block_hash: ::core::clone::Clone::clone(&self.block_hash),
                    slot: ::core::clone::Clone::clone(&self.slot),
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for ChainEventTime {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field3_finish(
                    f,
                    "ChainEventTime",
                    "block_number",
                    &self.block_number,
                    "block_hash",
                    &self.block_hash,
                    "slot",
                    &&self.slot,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ChainEventTime {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ChainEventTime {
            #[inline]
            fn eq(&self, other: &ChainEventTime) -> bool {
                self.block_number == other.block_number
                    && self.block_hash == other.block_hash && self.slot == other.slot
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for ChainEventTime {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<u64>;
                let _: ::core::cmp::AssertParamIsEq<String>;
            }
        }
        /// Chain events that the indexer is configured to produce.
        pub enum ChainEvent {
            /// A filtered transaction was confirmed
            TransactionEvent {
                time: ChainEventTime,
                transaction: TransactionEventRecord,
            },
            /// Rollback event occurred
            RollbackEvent { block_slot: u64, block_hash: String },
            /// Chain syncronisation progressed
            SyncProgressEvent { block_slot: u64, block_hash: String, percentage: f32 },
        }
        #[automatically_derived]
        impl ::core::clone::Clone for ChainEvent {
            #[inline]
            fn clone(&self) -> ChainEvent {
                match self {
                    ChainEvent::TransactionEvent {
                        time: __self_0,
                        transaction: __self_1,
                    } => {
                        ChainEvent::TransactionEvent {
                            time: ::core::clone::Clone::clone(__self_0),
                            transaction: ::core::clone::Clone::clone(__self_1),
                        }
                    }
                    ChainEvent::RollbackEvent {
                        block_slot: __self_0,
                        block_hash: __self_1,
                    } => {
                        ChainEvent::RollbackEvent {
                            block_slot: ::core::clone::Clone::clone(__self_0),
                            block_hash: ::core::clone::Clone::clone(__self_1),
                        }
                    }
                    ChainEvent::SyncProgressEvent {
                        block_slot: __self_0,
                        block_hash: __self_1,
                        percentage: __self_2,
                    } => {
                        ChainEvent::SyncProgressEvent {
                            block_slot: ::core::clone::Clone::clone(__self_0),
                            block_hash: ::core::clone::Clone::clone(__self_1),
                            percentage: ::core::clone::Clone::clone(__self_2),
                        }
                    }
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for ChainEvent {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                match self {
                    ChainEvent::TransactionEvent {
                        time: __self_0,
                        transaction: __self_1,
                    } => {
                        ::core::fmt::Formatter::debug_struct_field2_finish(
                            f,
                            "TransactionEvent",
                            "time",
                            __self_0,
                            "transaction",
                            &__self_1,
                        )
                    }
                    ChainEvent::RollbackEvent {
                        block_slot: __self_0,
                        block_hash: __self_1,
                    } => {
                        ::core::fmt::Formatter::debug_struct_field2_finish(
                            f,
                            "RollbackEvent",
                            "block_slot",
                            __self_0,
                            "block_hash",
                            &__self_1,
                        )
                    }
                    ChainEvent::SyncProgressEvent {
                        block_slot: __self_0,
                        block_hash: __self_1,
                        percentage: __self_2,
                    } => {
                        ::core::fmt::Formatter::debug_struct_field3_finish(
                            f,
                            "SyncProgressEvent",
                            "block_slot",
                            __self_0,
                            "block_hash",
                            __self_1,
                            "percentage",
                            &__self_2,
                        )
                    }
                }
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ChainEvent {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ChainEvent {
            #[inline]
            fn eq(&self, other: &ChainEvent) -> bool {
                let __self_tag = ::core::intrinsics::discriminant_value(self);
                let __arg1_tag = ::core::intrinsics::discriminant_value(other);
                __self_tag == __arg1_tag
                    && match (self, other) {
                        (
                            ChainEvent::TransactionEvent {
                                time: __self_0,
                                transaction: __self_1,
                            },
                            ChainEvent::TransactionEvent {
                                time: __arg1_0,
                                transaction: __arg1_1,
                            },
                        ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                        (
                            ChainEvent::RollbackEvent {
                                block_slot: __self_0,
                                block_hash: __self_1,
                            },
                            ChainEvent::RollbackEvent {
                                block_slot: __arg1_0,
                                block_hash: __arg1_1,
                            },
                        ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                        (
                            ChainEvent::SyncProgressEvent {
                                block_slot: __self_0,
                                block_hash: __self_1,
                                percentage: __self_2,
                            },
                            ChainEvent::SyncProgressEvent {
                                block_slot: __arg1_0,
                                block_hash: __arg1_1,
                                percentage: __arg1_2,
                            },
                        ) => {
                            *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1
                                && *__self_2 == *__arg1_2
                        }
                        _ => unsafe { ::core::intrinsics::unreachable() }
                    }
            }
        }
        /// Details on an transaction event (excluding unnecessary information).
        pub struct TransactionEventRecord {
            pub hash: TransactionHash,
            pub fee: u64,
            pub size: u32,
            pub inputs: Vec<TransactionInput>,
            pub outputs: Vec<TxInInfo>,
            pub mint: Value,
            pub plutus_data: HashMap<DatumHash, Datum>,
        }
        #[automatically_derived]
        impl ::core::clone::Clone for TransactionEventRecord {
            #[inline]
            fn clone(&self) -> TransactionEventRecord {
                TransactionEventRecord {
                    hash: ::core::clone::Clone::clone(&self.hash),
                    fee: ::core::clone::Clone::clone(&self.fee),
                    size: ::core::clone::Clone::clone(&self.size),
                    inputs: ::core::clone::Clone::clone(&self.inputs),
                    outputs: ::core::clone::Clone::clone(&self.outputs),
                    mint: ::core::clone::Clone::clone(&self.mint),
                    plutus_data: ::core::clone::Clone::clone(&self.plutus_data),
                }
            }
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for TransactionEventRecord {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                let names: &'static _ = &[
                    "hash",
                    "fee",
                    "size",
                    "inputs",
                    "outputs",
                    "mint",
                    "plutus_data",
                ];
                let values: &[&dyn ::core::fmt::Debug] = &[
                    &self.hash,
                    &self.fee,
                    &self.size,
                    &self.inputs,
                    &self.outputs,
                    &self.mint,
                    &&self.plutus_data,
                ];
                ::core::fmt::Formatter::debug_struct_fields_finish(
                    f,
                    "TransactionEventRecord",
                    names,
                    values,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for TransactionEventRecord {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for TransactionEventRecord {
            #[inline]
            fn eq(&self, other: &TransactionEventRecord) -> bool {
                self.hash == other.hash && self.fee == other.fee
                    && self.size == other.size && self.inputs == other.inputs
                    && self.outputs == other.outputs && self.mint == other.mint
                    && self.plutus_data == other.plutus_data
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for TransactionEventRecord {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<TransactionHash>;
                let _: ::core::cmp::AssertParamIsEq<u64>;
                let _: ::core::cmp::AssertParamIsEq<u32>;
                let _: ::core::cmp::AssertParamIsEq<Vec<TransactionInput>>;
                let _: ::core::cmp::AssertParamIsEq<Vec<TxInInfo>>;
                let _: ::core::cmp::AssertParamIsEq<Value>;
                let _: ::core::cmp::AssertParamIsEq<HashMap<DatumHash, Datum>>;
            }
        }
        pub fn parse_oura_event(
            ev: oura::Event,
            progress_tracker: &mut Option<ProgressTracker>,
        ) -> Result<Option<ChainEvent>, OuraParseError> {
            Ok(
                match ev.data {
                    oura::EventData::Transaction(tx_rec) => {
                        {
                            use ::tracing::__macro_support::Callsite as _;
                            static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                static META: ::tracing::Metadata<'static> = {
                                    ::tracing_core::metadata::Metadata::new(
                                        "event src/handler/chain_event.rs:70",
                                        "tx_indexer::handler::chain_event",
                                        Level::DEBUG,
                                        ::core::option::Option::Some("src/handler/chain_event.rs"),
                                        ::core::option::Option::Some(70u32),
                                        ::core::option::Option::Some(
                                            "tx_indexer::handler::chain_event",
                                        ),
                                        ::tracing_core::field::FieldSet::new(
                                            &["label", "transaction_record"],
                                            ::tracing_core::callsite::Identifier(&__CALLSITE),
                                        ),
                                        ::tracing::metadata::Kind::EVENT,
                                    )
                                };
                                ::tracing::callsite::DefaultCallsite::new(&META)
                            };
                            let enabled = Level::DEBUG
                                <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                && Level::DEBUG
                                    <= ::tracing::level_filters::LevelFilter::current()
                                && {
                                    let interest = __CALLSITE.interest();
                                    !interest.is_never()
                                        && ::tracing::__macro_support::__is_enabled(
                                            __CALLSITE.metadata(),
                                            interest,
                                        )
                                };
                            if enabled {
                                (|value_set: ::tracing::field::ValueSet| {
                                    let meta = __CALLSITE.metadata();
                                    ::tracing::Event::dispatch(meta, &value_set);
                                })({
                                    #[allow(unused_imports)]
                                    use ::tracing::field::{debug, display, Value};
                                    let mut iter = __CALLSITE.metadata().fields().iter();
                                    __CALLSITE
                                        .metadata()
                                        .fields()
                                        .value_set(
                                            &[
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(
                                                        &"TransactionEvent" as &dyn Value,
                                                    ),
                                                ),
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(&debug(&tx_rec) as &dyn Value),
                                                ),
                                            ],
                                        )
                                });
                            } else {
                            }
                        };
                        Some(ChainEvent::TransactionEvent {
                            time: ChainEventTime {
                                block_hash: ev.context.block_hash.unwrap(),
                                block_number: ev.context.block_number.unwrap(),
                                slot: ev.context.slot.unwrap(),
                            },
                            transaction: TransactionEventRecord::from_oura(tx_rec)?,
                        })
                    }
                    oura::EventData::RollBack { block_slot, block_hash } => {
                        {
                            use ::tracing::__macro_support::Callsite as _;
                            static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                static META: ::tracing::Metadata<'static> = {
                                    ::tracing_core::metadata::Metadata::new(
                                        "event src/handler/chain_event.rs:86",
                                        "tx_indexer::handler::chain_event",
                                        Level::DEBUG,
                                        ::core::option::Option::Some("src/handler/chain_event.rs"),
                                        ::core::option::Option::Some(86u32),
                                        ::core::option::Option::Some(
                                            "tx_indexer::handler::chain_event",
                                        ),
                                        ::tracing_core::field::FieldSet::new(
                                            &["label", "block_slot", "block_hash"],
                                            ::tracing_core::callsite::Identifier(&__CALLSITE),
                                        ),
                                        ::tracing::metadata::Kind::EVENT,
                                    )
                                };
                                ::tracing::callsite::DefaultCallsite::new(&META)
                            };
                            let enabled = Level::DEBUG
                                <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                && Level::DEBUG
                                    <= ::tracing::level_filters::LevelFilter::current()
                                && {
                                    let interest = __CALLSITE.interest();
                                    !interest.is_never()
                                        && ::tracing::__macro_support::__is_enabled(
                                            __CALLSITE.metadata(),
                                            interest,
                                        )
                                };
                            if enabled {
                                (|value_set: ::tracing::field::ValueSet| {
                                    let meta = __CALLSITE.metadata();
                                    ::tracing::Event::dispatch(meta, &value_set);
                                })({
                                    #[allow(unused_imports)]
                                    use ::tracing::field::{debug, display, Value};
                                    let mut iter = __CALLSITE.metadata().fields().iter();
                                    __CALLSITE
                                        .metadata()
                                        .fields()
                                        .value_set(
                                            &[
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(&"RollbackEvent" as &dyn Value),
                                                ),
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(
                                                        &debug(&block_slot) as &dyn Value,
                                                    ),
                                                ),
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(
                                                        &debug(&block_hash) as &dyn Value,
                                                    ),
                                                ),
                                            ],
                                        )
                                });
                            } else {
                            }
                        };
                        Some(ChainEvent::RollbackEvent {
                            block_slot,
                            block_hash,
                        })
                    }
                    oura::EventData::Block(block_rec) => {
                        {
                            use ::tracing::__macro_support::Callsite as _;
                            static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                static META: ::tracing::Metadata<'static> = {
                                    ::tracing_core::metadata::Metadata::new(
                                        "event src/handler/chain_event.rs:93",
                                        "tx_indexer::handler::chain_event",
                                        Level::DEBUG,
                                        ::core::option::Option::Some("src/handler/chain_event.rs"),
                                        ::core::option::Option::Some(93u32),
                                        ::core::option::Option::Some(
                                            "tx_indexer::handler::chain_event",
                                        ),
                                        ::tracing_core::field::FieldSet::new(
                                            &["label", "block_record"],
                                            ::tracing_core::callsite::Identifier(&__CALLSITE),
                                        ),
                                        ::tracing::metadata::Kind::EVENT,
                                    )
                                };
                                ::tracing::callsite::DefaultCallsite::new(&META)
                            };
                            let enabled = Level::DEBUG
                                <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                && Level::DEBUG
                                    <= ::tracing::level_filters::LevelFilter::current()
                                && {
                                    let interest = __CALLSITE.interest();
                                    !interest.is_never()
                                        && ::tracing::__macro_support::__is_enabled(
                                            __CALLSITE.metadata(),
                                            interest,
                                        )
                                };
                            if enabled {
                                (|value_set: ::tracing::field::ValueSet| {
                                    let meta = __CALLSITE.metadata();
                                    ::tracing::Event::dispatch(meta, &value_set);
                                })({
                                    #[allow(unused_imports)]
                                    use ::tracing::field::{debug, display, Value};
                                    let mut iter = __CALLSITE.metadata().fields().iter();
                                    __CALLSITE
                                        .metadata()
                                        .fields()
                                        .value_set(
                                            &[
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(&"BlockEvent" as &dyn Value),
                                                ),
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(
                                                        &debug(&block_rec) as &dyn Value,
                                                    ),
                                                ),
                                            ],
                                        )
                                });
                            } else {
                            }
                        };
                        match progress_tracker {
                            Some(progress_tracker) => {
                                let block_slot = block_rec.slot;
                                let block_hash = block_rec.hash;
                                let percentage = progress_tracker
                                    .get_percentage(block_slot)?;
                                let throttled_sync_progress = (percentage * 10.0) as usize;
                                let is_updated = progress_tracker
                                    .sync_progress
                                    .fetch_update(
                                        Ordering::SeqCst,
                                        Ordering::SeqCst,
                                        |prev_status| {
                                            if prev_status < throttled_sync_progress {
                                                Some(throttled_sync_progress)
                                            } else {
                                                None
                                            }
                                        },
                                    )
                                    .is_ok();
                                if is_updated {
                                    {
                                        use ::tracing::__macro_support::Callsite as _;
                                        static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                            static META: ::tracing::Metadata<'static> = {
                                                ::tracing_core::metadata::Metadata::new(
                                                    "event src/handler/chain_event.rs:114",
                                                    "tx_indexer::handler::chain_event",
                                                    Level::INFO,
                                                    ::core::option::Option::Some("src/handler/chain_event.rs"),
                                                    ::core::option::Option::Some(114u32),
                                                    ::core::option::Option::Some(
                                                        "tx_indexer::handler::chain_event",
                                                    ),
                                                    ::tracing_core::field::FieldSet::new(
                                                        &["percentage", "block_slot", "block_hash", "label"],
                                                        ::tracing_core::callsite::Identifier(&__CALLSITE),
                                                    ),
                                                    ::tracing::metadata::Kind::EVENT,
                                                )
                                            };
                                            ::tracing::callsite::DefaultCallsite::new(&META)
                                        };
                                        let enabled = Level::INFO
                                            <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                            && Level::INFO
                                                <= ::tracing::level_filters::LevelFilter::current()
                                            && {
                                                let interest = __CALLSITE.interest();
                                                !interest.is_never()
                                                    && ::tracing::__macro_support::__is_enabled(
                                                        __CALLSITE.metadata(),
                                                        interest,
                                                    )
                                            };
                                        if enabled {
                                            (|value_set: ::tracing::field::ValueSet| {
                                                let meta = __CALLSITE.metadata();
                                                ::tracing::Event::dispatch(meta, &value_set);
                                            })({
                                                #[allow(unused_imports)]
                                                use ::tracing::field::{debug, display, Value};
                                                let mut iter = __CALLSITE.metadata().fields().iter();
                                                __CALLSITE
                                                    .metadata()
                                                    .fields()
                                                    .value_set(
                                                        &[
                                                            (
                                                                &::core::iter::Iterator::next(&mut iter)
                                                                    .expect("FieldSet corrupted (this is a bug)"),
                                                                ::core::option::Option::Some(
                                                                    &{
                                                                        let res = ::alloc::fmt::format(
                                                                            format_args!("{0:.1}%", percentage),
                                                                        );
                                                                        res
                                                                    } as &dyn Value,
                                                                ),
                                                            ),
                                                            (
                                                                &::core::iter::Iterator::next(&mut iter)
                                                                    .expect("FieldSet corrupted (this is a bug)"),
                                                                ::core::option::Option::Some(
                                                                    &debug(&block_slot) as &dyn Value,
                                                                ),
                                                            ),
                                                            (
                                                                &::core::iter::Iterator::next(&mut iter)
                                                                    .expect("FieldSet corrupted (this is a bug)"),
                                                                ::core::option::Option::Some(
                                                                    &debug(&block_hash) as &dyn Value,
                                                                ),
                                                            ),
                                                            (
                                                                &::core::iter::Iterator::next(&mut iter)
                                                                    .expect("FieldSet corrupted (this is a bug)"),
                                                                ::core::option::Option::Some(
                                                                    &"Chain synchronization progress" as &dyn Value,
                                                                ),
                                                            ),
                                                        ],
                                                    )
                                            });
                                        } else {
                                        }
                                    };
                                }
                                Some(ChainEvent::SyncProgressEvent {
                                    percentage,
                                    block_slot,
                                    block_hash,
                                })
                            }
                            None => {
                                Some(ChainEvent::SyncProgressEvent {
                                    percentage: 100.0,
                                    block_slot: block_rec.slot,
                                    block_hash: block_rec.hash,
                                })
                            }
                        }
                    }
                    _ => {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "absurd: Indexer filter should only allow transaction event variant.",
                            ),
                        );
                    }
                },
            )
        }
        impl FromOura<oura::TransactionRecord> for TransactionEventRecord {
            fn from_oura(
                tx: oura::TransactionRecord,
            ) -> Result<TransactionEventRecord, OuraParseError> {
                Ok(TransactionEventRecord {
                    hash: TransactionHash::from_oura(tx.hash.clone())?,
                    fee: tx.fee,
                    size: tx.size,
                    inputs: tx
                        .inputs
                        .unwrap()
                        .into_iter()
                        .map(|oura::TxInputRecord { tx_id, index }| {
                            Ok(TransactionInput {
                                transaction_id: TransactionHash::from_oura(tx_id)?,
                                index: BigInt::from(index),
                            })
                        })
                        .collect::<Result<_, OuraParseError>>()?,
                    outputs: tx
                        .outputs
                        .unwrap()
                        .into_iter()
                        .enumerate()
                        .map(|
                            (
                                index,
                                oura::TxOutputRecord {
                                    address,
                                    amount,
                                    assets,
                                    datum_hash,
                                    inline_datum,
                                },
                            )|
                        {
                            let reference = TransactionInput {
                                transaction_id: TransactionHash::from_oura(
                                    tx.hash.clone(),
                                )?,
                                index: index.into(),
                            };
                            let output = TransactionOutput {
                                address: Address::from_oura(address)?,
                                datum: match (datum_hash, inline_datum) {
                                    (None, None) => OutputDatum::None,
                                    (_, Some(datm)) => {
                                        OutputDatum::InlineDatum(
                                            Datum::from_oura(datm.plutus_data)?,
                                        )
                                    }
                                    (Some(dh), _) => {
                                        OutputDatum::DatumHash(DatumHash::from_oura(dh)?)
                                    }
                                },
                                reference_script: None,
                                value: Value::ada_value(&BigInt::from(amount))
                                    + Value::from_oura(assets.unwrap_or_default())?,
                            };
                            Ok(TxInInfo { reference, output })
                        })
                        .collect::<Result<_, OuraParseError>>()?,
                    mint: tx.mint.map_or(Ok(Value::new()), Value::from_oura)?,
                    plutus_data: tx
                        .plutus_data
                        .unwrap_or_default()
                        .into_iter()
                        .map(|oura::PlutusDatumRecord { plutus_data, datum_hash }| {
                            Ok((
                                DatumHash::from_oura(datum_hash)?,
                                Datum::from_oura(plutus_data)?,
                            ))
                        })
                        .collect::<Result<_, OuraParseError>>()?,
                })
            }
        }
    }
    pub mod retry {
        use crate::{
            error::{ErrorPolicy, ErrorPolicyProvider},
            handler::{callback::EventHandler, chain_event::parse_oura_event},
            progress_tracker::ProgressTracker,
        };
        use oura::model as oura;
        use std::{fmt::Debug, ops::Mul, time::Duration};
        use strum_macros::Display;
        use tracing::{event, span, Instrument, Level};
        /// Influence retrying behavior.
        /// i.e How many times and how often a failed operation should be retried.
        /// Given we are dealing with `ErrorPolicy::Retry`
        pub struct RetryPolicy {
            pub max_retries: u32,
            pub backoff_unit: Duration,
            pub backoff_factor: u32,
            pub max_backoff: Duration,
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for RetryPolicy {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field4_finish(
                    f,
                    "RetryPolicy",
                    "max_retries",
                    &self.max_retries,
                    "backoff_unit",
                    &self.backoff_unit,
                    "backoff_factor",
                    &self.backoff_factor,
                    "max_backoff",
                    &&self.max_backoff,
                )
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for RetryPolicy {}
        #[automatically_derived]
        impl ::core::clone::Clone for RetryPolicy {
            #[inline]
            fn clone(&self) -> RetryPolicy {
                let _: ::core::clone::AssertParamIsClone<u32>;
                let _: ::core::clone::AssertParamIsClone<Duration>;
                *self
            }
        }
        enum EventOutcome {
            Success,
            FailureExit,
            FailureSkip,
            FailureRetry,
            RetriesExhausted,
            RetryBackoff,
        }
        impl ::core::fmt::Display for EventOutcome {
            fn fmt(
                &self,
                f: &mut ::core::fmt::Formatter,
            ) -> ::core::result::Result<(), ::core::fmt::Error> {
                match *self {
                    EventOutcome::Success => f.pad("Success"),
                    EventOutcome::FailureExit => f.pad("FailureExit"),
                    EventOutcome::FailureSkip => f.pad("FailureSkip"),
                    EventOutcome::FailureRetry => f.pad("FailureRetry"),
                    EventOutcome::RetriesExhausted => f.pad("RetriesExhausted"),
                    EventOutcome::RetryBackoff => f.pad("RetryBackoff"),
                }
            }
        }
        impl Default for RetryPolicy {
            fn default() -> Self {
                Self {
                    max_retries: 20,
                    backoff_unit: Duration::from_millis(5_000),
                    backoff_factor: 2,
                    max_backoff: Duration::from_millis(20 * 5_000),
                }
            }
        }
        fn compute_backoff_delay(policy: &RetryPolicy, retry: u32) -> Duration {
            let units = policy.backoff_factor.pow(retry);
            let backoff = policy.backoff_unit.mul(units);
            core::cmp::min(backoff, policy.max_backoff)
        }
        /// Wrap an operation with retry logic.
        /// Retrying is based on ErrorPolicy associated with particular error.
        /// Retries are only performed for ErrorPolicy::Retry - other errors won't cause invocation of given operation again.
        pub(crate) async fn perform_with_retry<H: EventHandler>(
            handler: &H,
            oura_event: oura::Event,
            policy: &RetryPolicy,
            progress_tracker: &mut Option<ProgressTracker>,
        ) -> Result<(), H::Error> {
            let span = {
                use ::tracing::__macro_support::Callsite as _;
                static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                    static META: ::tracing::Metadata<'static> = {
                        ::tracing_core::metadata::Metadata::new(
                            "perform_with_retry",
                            "tx_indexer::handler::retry",
                            Level::DEBUG,
                            ::core::option::Option::Some("src/handler/retry.rs"),
                            ::core::option::Option::Some(58u32),
                            ::core::option::Option::Some("tx_indexer::handler::retry"),
                            ::tracing_core::field::FieldSet::new(
                                &[],
                                ::tracing_core::callsite::Identifier(&__CALLSITE),
                            ),
                            ::tracing::metadata::Kind::SPAN,
                        )
                    };
                    ::tracing::callsite::DefaultCallsite::new(&META)
                };
                let mut interest = ::tracing::subscriber::Interest::never();
                if Level::DEBUG <= ::tracing::level_filters::STATIC_MAX_LEVEL
                    && Level::DEBUG <= ::tracing::level_filters::LevelFilter::current()
                    && {
                        interest = __CALLSITE.interest();
                        !interest.is_never()
                    }
                    && ::tracing::__macro_support::__is_enabled(
                        __CALLSITE.metadata(),
                        interest,
                    )
                {
                    let meta = __CALLSITE.metadata();
                    ::tracing::Span::new(meta, &{ meta.fields().value_set(&[]) })
                } else {
                    let span = ::tracing::__macro_support::__disabled_span(
                        __CALLSITE.metadata(),
                    );
                    {};
                    span
                }
            };
            let _enter = span.enter();
            match parse_oura_event(oura_event, progress_tracker) {
                Ok(Some(event)) => {
                    let mut retry = 0;
                    loop {
                        let span = {
                            use ::tracing::__macro_support::Callsite as _;
                            static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                static META: ::tracing::Metadata<'static> = {
                                    ::tracing_core::metadata::Metadata::new(
                                        "TryingOperation",
                                        "tx_indexer::handler::retry",
                                        Level::DEBUG,
                                        ::core::option::Option::Some("src/handler/retry.rs"),
                                        ::core::option::Option::Some(69u32),
                                        ::core::option::Option::Some("tx_indexer::handler::retry"),
                                        ::tracing_core::field::FieldSet::new(
                                            &["retry_count"],
                                            ::tracing_core::callsite::Identifier(&__CALLSITE),
                                        ),
                                        ::tracing::metadata::Kind::SPAN,
                                    )
                                };
                                ::tracing::callsite::DefaultCallsite::new(&META)
                            };
                            let mut interest = ::tracing::subscriber::Interest::never();
                            if Level::DEBUG <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                && Level::DEBUG
                                    <= ::tracing::level_filters::LevelFilter::current()
                                && {
                                    interest = __CALLSITE.interest();
                                    !interest.is_never()
                                }
                                && ::tracing::__macro_support::__is_enabled(
                                    __CALLSITE.metadata(),
                                    interest,
                                )
                            {
                                let meta = __CALLSITE.metadata();
                                ::tracing::Span::new(
                                    meta,
                                    &{
                                        #[allow(unused_imports)]
                                        use ::tracing::field::{debug, display, Value};
                                        let mut iter = meta.fields().iter();
                                        meta.fields()
                                            .value_set(
                                                &[
                                                    (
                                                        &::core::iter::Iterator::next(&mut iter)
                                                            .expect("FieldSet corrupted (this is a bug)"),
                                                        ::core::option::Option::Some(&retry as &dyn Value),
                                                    ),
                                                ],
                                            )
                                    },
                                )
                            } else {
                                let span = ::tracing::__macro_support::__disabled_span(
                                    __CALLSITE.metadata(),
                                );
                                {};
                                span
                            }
                        };
                        let res = async {
                            let result = handler
                                .handle(event.clone())
                                .instrument({
                                    use ::tracing::__macro_support::Callsite as _;
                                    static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                        static META: ::tracing::Metadata<'static> = {
                                            ::tracing_core::metadata::Metadata::new(
                                                "UserDefinedHandler",
                                                "tx_indexer::handler::retry",
                                                Level::DEBUG,
                                                ::core::option::Option::Some("src/handler/retry.rs"),
                                                ::core::option::Option::Some(72u32),
                                                ::core::option::Option::Some("tx_indexer::handler::retry"),
                                                ::tracing_core::field::FieldSet::new(
                                                    &[],
                                                    ::tracing_core::callsite::Identifier(&__CALLSITE),
                                                ),
                                                ::tracing::metadata::Kind::SPAN,
                                            )
                                        };
                                        ::tracing::callsite::DefaultCallsite::new(&META)
                                    };
                                    let mut interest = ::tracing::subscriber::Interest::never();
                                    if Level::DEBUG
                                        <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                        && Level::DEBUG
                                            <= ::tracing::level_filters::LevelFilter::current()
                                        && {
                                            interest = __CALLSITE.interest();
                                            !interest.is_never()
                                        }
                                        && ::tracing::__macro_support::__is_enabled(
                                            __CALLSITE.metadata(),
                                            interest,
                                        )
                                    {
                                        let meta = __CALLSITE.metadata();
                                        ::tracing::Span::new(
                                            meta,
                                            &{ meta.fields().value_set(&[]) },
                                        )
                                    } else {
                                        let span = ::tracing::__macro_support::__disabled_span(
                                            __CALLSITE.metadata(),
                                        );
                                        {};
                                        span
                                    }
                                })
                                .await;
                            match result {
                                Ok(_) => {
                                    {
                                        use ::tracing::__macro_support::Callsite as _;
                                        static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                            static META: ::tracing::Metadata<'static> = {
                                                ::tracing_core::metadata::Metadata::new(
                                                    "event src/handler/retry.rs:76",
                                                    "tx_indexer::handler::retry",
                                                    Level::DEBUG,
                                                    ::core::option::Option::Some("src/handler/retry.rs"),
                                                    ::core::option::Option::Some(76u32),
                                                    ::core::option::Option::Some("tx_indexer::handler::retry"),
                                                    ::tracing_core::field::FieldSet::new(
                                                        &["label"],
                                                        ::tracing_core::callsite::Identifier(&__CALLSITE),
                                                    ),
                                                    ::tracing::metadata::Kind::EVENT,
                                                )
                                            };
                                            ::tracing::callsite::DefaultCallsite::new(&META)
                                        };
                                        let enabled = Level::DEBUG
                                            <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                            && Level::DEBUG
                                                <= ::tracing::level_filters::LevelFilter::current()
                                            && {
                                                let interest = __CALLSITE.interest();
                                                !interest.is_never()
                                                    && ::tracing::__macro_support::__is_enabled(
                                                        __CALLSITE.metadata(),
                                                        interest,
                                                    )
                                            };
                                        if enabled {
                                            (|value_set: ::tracing::field::ValueSet| {
                                                let meta = __CALLSITE.metadata();
                                                ::tracing::Event::dispatch(meta, &value_set);
                                            })({
                                                #[allow(unused_imports)]
                                                use ::tracing::field::{debug, display, Value};
                                                let mut iter = __CALLSITE.metadata().fields().iter();
                                                __CALLSITE
                                                    .metadata()
                                                    .fields()
                                                    .value_set(
                                                        &[
                                                            (
                                                                &::core::iter::Iterator::next(&mut iter)
                                                                    .expect("FieldSet corrupted (this is a bug)"),
                                                                ::core::option::Option::Some(
                                                                    &display(&EventOutcome::Success) as &dyn Value,
                                                                ),
                                                            ),
                                                        ],
                                                    )
                                            });
                                        } else {
                                        }
                                    };
                                    Some(Ok(()))
                                }
                                Err(err) => {
                                    match err.get_error_policy() {
                                        ErrorPolicy::Exit => {
                                            {
                                                use ::tracing::__macro_support::Callsite as _;
                                                static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                                    static META: ::tracing::Metadata<'static> = {
                                                        ::tracing_core::metadata::Metadata::new(
                                                            "event src/handler/retry.rs:81",
                                                            "tx_indexer::handler::retry",
                                                            Level::ERROR,
                                                            ::core::option::Option::Some("src/handler/retry.rs"),
                                                            ::core::option::Option::Some(81u32),
                                                            ::core::option::Option::Some("tx_indexer::handler::retry"),
                                                            ::tracing_core::field::FieldSet::new(
                                                                &["label"],
                                                                ::tracing_core::callsite::Identifier(&__CALLSITE),
                                                            ),
                                                            ::tracing::metadata::Kind::EVENT,
                                                        )
                                                    };
                                                    ::tracing::callsite::DefaultCallsite::new(&META)
                                                };
                                                let enabled = Level::ERROR
                                                    <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                                    && Level::ERROR
                                                        <= ::tracing::level_filters::LevelFilter::current()
                                                    && {
                                                        let interest = __CALLSITE.interest();
                                                        !interest.is_never()
                                                            && ::tracing::__macro_support::__is_enabled(
                                                                __CALLSITE.metadata(),
                                                                interest,
                                                            )
                                                    };
                                                if enabled {
                                                    (|value_set: ::tracing::field::ValueSet| {
                                                        let meta = __CALLSITE.metadata();
                                                        ::tracing::Event::dispatch(meta, &value_set);
                                                    })({
                                                        #[allow(unused_imports)]
                                                        use ::tracing::field::{debug, display, Value};
                                                        let mut iter = __CALLSITE.metadata().fields().iter();
                                                        __CALLSITE
                                                            .metadata()
                                                            .fields()
                                                            .value_set(
                                                                &[
                                                                    (
                                                                        &::core::iter::Iterator::next(&mut iter)
                                                                            .expect("FieldSet corrupted (this is a bug)"),
                                                                        ::core::option::Option::Some(
                                                                            &display(&EventOutcome::FailureExit) as &dyn Value,
                                                                        ),
                                                                    ),
                                                                ],
                                                            )
                                                    });
                                                } else {
                                                }
                                            };
                                            Some(Err(err))
                                        }
                                        ErrorPolicy::Skip => {
                                            {
                                                use ::tracing::__macro_support::Callsite as _;
                                                static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                                    static META: ::tracing::Metadata<'static> = {
                                                        ::tracing_core::metadata::Metadata::new(
                                                            "event src/handler/retry.rs:85",
                                                            "tx_indexer::handler::retry",
                                                            Level::WARN,
                                                            ::core::option::Option::Some("src/handler/retry.rs"),
                                                            ::core::option::Option::Some(85u32),
                                                            ::core::option::Option::Some("tx_indexer::handler::retry"),
                                                            ::tracing_core::field::FieldSet::new(
                                                                &["label", "err"],
                                                                ::tracing_core::callsite::Identifier(&__CALLSITE),
                                                            ),
                                                            ::tracing::metadata::Kind::EVENT,
                                                        )
                                                    };
                                                    ::tracing::callsite::DefaultCallsite::new(&META)
                                                };
                                                let enabled = Level::WARN
                                                    <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                                    && Level::WARN
                                                        <= ::tracing::level_filters::LevelFilter::current()
                                                    && {
                                                        let interest = __CALLSITE.interest();
                                                        !interest.is_never()
                                                            && ::tracing::__macro_support::__is_enabled(
                                                                __CALLSITE.metadata(),
                                                                interest,
                                                            )
                                                    };
                                                if enabled {
                                                    (|value_set: ::tracing::field::ValueSet| {
                                                        let meta = __CALLSITE.metadata();
                                                        ::tracing::Event::dispatch(meta, &value_set);
                                                    })({
                                                        #[allow(unused_imports)]
                                                        use ::tracing::field::{debug, display, Value};
                                                        let mut iter = __CALLSITE.metadata().fields().iter();
                                                        __CALLSITE
                                                            .metadata()
                                                            .fields()
                                                            .value_set(
                                                                &[
                                                                    (
                                                                        &::core::iter::Iterator::next(&mut iter)
                                                                            .expect("FieldSet corrupted (this is a bug)"),
                                                                        ::core::option::Option::Some(
                                                                            &display(&EventOutcome::FailureSkip) as &dyn Value,
                                                                        ),
                                                                    ),
                                                                    (
                                                                        &::core::iter::Iterator::next(&mut iter)
                                                                            .expect("FieldSet corrupted (this is a bug)"),
                                                                        ::core::option::Option::Some(&debug(&err) as &dyn Value),
                                                                    ),
                                                                ],
                                                            )
                                                    });
                                                } else {
                                                }
                                            };
                                            Some(Ok(()))
                                        }
                                        ErrorPolicy::Call(err_f) => {
                                            {
                                                use ::tracing::__macro_support::Callsite as _;
                                                static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                                    static META: ::tracing::Metadata<'static> = {
                                                        ::tracing_core::metadata::Metadata::new(
                                                            "OperationFailureCall",
                                                            "tx_indexer::handler::retry",
                                                            Level::WARN,
                                                            ::core::option::Option::Some("src/handler/retry.rs"),
                                                            ::core::option::Option::Some(88u32),
                                                            ::core::option::Option::Some("tx_indexer::handler::retry"),
                                                            ::tracing_core::field::FieldSet::new(
                                                                &[],
                                                                ::tracing_core::callsite::Identifier(&__CALLSITE),
                                                            ),
                                                            ::tracing::metadata::Kind::SPAN,
                                                        )
                                                    };
                                                    ::tracing::callsite::DefaultCallsite::new(&META)
                                                };
                                                let mut interest = ::tracing::subscriber::Interest::never();
                                                if Level::WARN <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                                    && Level::WARN
                                                        <= ::tracing::level_filters::LevelFilter::current()
                                                    && {
                                                        interest = __CALLSITE.interest();
                                                        !interest.is_never()
                                                    }
                                                    && ::tracing::__macro_support::__is_enabled(
                                                        __CALLSITE.metadata(),
                                                        interest,
                                                    )
                                                {
                                                    let meta = __CALLSITE.metadata();
                                                    ::tracing::Span::new(
                                                        meta,
                                                        &{ meta.fields().value_set(&[]) },
                                                    )
                                                } else {
                                                    let span = ::tracing::__macro_support::__disabled_span(
                                                        __CALLSITE.metadata(),
                                                    );
                                                    {};
                                                    span
                                                }
                                            }
                                                .in_scope(|| {
                                                    err_f(err);
                                                    Some(Ok(()))
                                                })
                                        }
                                        ErrorPolicy::Retry if retry < policy.max_retries => {
                                            {
                                                use ::tracing::__macro_support::Callsite as _;
                                                static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                                    static META: ::tracing::Metadata<'static> = {
                                                        ::tracing_core::metadata::Metadata::new(
                                                            "event src/handler/retry.rs:93",
                                                            "tx_indexer::handler::retry",
                                                            Level::WARN,
                                                            ::core::option::Option::Some("src/handler/retry.rs"),
                                                            ::core::option::Option::Some(93u32),
                                                            ::core::option::Option::Some("tx_indexer::handler::retry"),
                                                            ::tracing_core::field::FieldSet::new(
                                                                &["label", "err"],
                                                                ::tracing_core::callsite::Identifier(&__CALLSITE),
                                                            ),
                                                            ::tracing::metadata::Kind::EVENT,
                                                        )
                                                    };
                                                    ::tracing::callsite::DefaultCallsite::new(&META)
                                                };
                                                let enabled = Level::WARN
                                                    <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                                    && Level::WARN
                                                        <= ::tracing::level_filters::LevelFilter::current()
                                                    && {
                                                        let interest = __CALLSITE.interest();
                                                        !interest.is_never()
                                                            && ::tracing::__macro_support::__is_enabled(
                                                                __CALLSITE.metadata(),
                                                                interest,
                                                            )
                                                    };
                                                if enabled {
                                                    (|value_set: ::tracing::field::ValueSet| {
                                                        let meta = __CALLSITE.metadata();
                                                        ::tracing::Event::dispatch(meta, &value_set);
                                                    })({
                                                        #[allow(unused_imports)]
                                                        use ::tracing::field::{debug, display, Value};
                                                        let mut iter = __CALLSITE.metadata().fields().iter();
                                                        __CALLSITE
                                                            .metadata()
                                                            .fields()
                                                            .value_set(
                                                                &[
                                                                    (
                                                                        &::core::iter::Iterator::next(&mut iter)
                                                                            .expect("FieldSet corrupted (this is a bug)"),
                                                                        ::core::option::Option::Some(
                                                                            &display(&EventOutcome::FailureRetry) as &dyn Value,
                                                                        ),
                                                                    ),
                                                                    (
                                                                        &::core::iter::Iterator::next(&mut iter)
                                                                            .expect("FieldSet corrupted (this is a bug)"),
                                                                        ::core::option::Option::Some(&debug(&err) as &dyn Value),
                                                                    ),
                                                                ],
                                                            )
                                                    });
                                                } else {
                                                }
                                            };
                                            retry += 1;
                                            let backoff = compute_backoff_delay(policy, retry);
                                            {
                                                use ::tracing::__macro_support::Callsite as _;
                                                static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                                    static META: ::tracing::Metadata<'static> = {
                                                        ::tracing_core::metadata::Metadata::new(
                                                            "event src/handler/retry.rs:99",
                                                            "tx_indexer::handler::retry",
                                                            Level::DEBUG,
                                                            ::core::option::Option::Some("src/handler/retry.rs"),
                                                            ::core::option::Option::Some(99u32),
                                                            ::core::option::Option::Some("tx_indexer::handler::retry"),
                                                            ::tracing_core::field::FieldSet::new(
                                                                &["label", "backoff_secs"],
                                                                ::tracing_core::callsite::Identifier(&__CALLSITE),
                                                            ),
                                                            ::tracing::metadata::Kind::EVENT,
                                                        )
                                                    };
                                                    ::tracing::callsite::DefaultCallsite::new(&META)
                                                };
                                                let enabled = Level::DEBUG
                                                    <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                                    && Level::DEBUG
                                                        <= ::tracing::level_filters::LevelFilter::current()
                                                    && {
                                                        let interest = __CALLSITE.interest();
                                                        !interest.is_never()
                                                            && ::tracing::__macro_support::__is_enabled(
                                                                __CALLSITE.metadata(),
                                                                interest,
                                                            )
                                                    };
                                                if enabled {
                                                    (|value_set: ::tracing::field::ValueSet| {
                                                        let meta = __CALLSITE.metadata();
                                                        ::tracing::Event::dispatch(meta, &value_set);
                                                    })({
                                                        #[allow(unused_imports)]
                                                        use ::tracing::field::{debug, display, Value};
                                                        let mut iter = __CALLSITE.metadata().fields().iter();
                                                        __CALLSITE
                                                            .metadata()
                                                            .fields()
                                                            .value_set(
                                                                &[
                                                                    (
                                                                        &::core::iter::Iterator::next(&mut iter)
                                                                            .expect("FieldSet corrupted (this is a bug)"),
                                                                        ::core::option::Option::Some(
                                                                            &display(&EventOutcome::RetryBackoff) as &dyn Value,
                                                                        ),
                                                                    ),
                                                                    (
                                                                        &::core::iter::Iterator::next(&mut iter)
                                                                            .expect("FieldSet corrupted (this is a bug)"),
                                                                        ::core::option::Option::Some(
                                                                            &backoff.as_secs() as &dyn Value,
                                                                        ),
                                                                    ),
                                                                ],
                                                            )
                                                    });
                                                } else {
                                                }
                                            };
                                            std::thread::sleep(backoff);
                                            None
                                        }
                                        _ => {
                                            {
                                                use ::tracing::__macro_support::Callsite as _;
                                                static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                                                    static META: ::tracing::Metadata<'static> = {
                                                        ::tracing_core::metadata::Metadata::new(
                                                            "event src/handler/retry.rs:106",
                                                            "tx_indexer::handler::retry",
                                                            Level::DEBUG,
                                                            ::core::option::Option::Some("src/handler/retry.rs"),
                                                            ::core::option::Option::Some(106u32),
                                                            ::core::option::Option::Some("tx_indexer::handler::retry"),
                                                            ::tracing_core::field::FieldSet::new(
                                                                &["label"],
                                                                ::tracing_core::callsite::Identifier(&__CALLSITE),
                                                            ),
                                                            ::tracing::metadata::Kind::EVENT,
                                                        )
                                                    };
                                                    ::tracing::callsite::DefaultCallsite::new(&META)
                                                };
                                                let enabled = Level::DEBUG
                                                    <= ::tracing::level_filters::STATIC_MAX_LEVEL
                                                    && Level::DEBUG
                                                        <= ::tracing::level_filters::LevelFilter::current()
                                                    && {
                                                        let interest = __CALLSITE.interest();
                                                        !interest.is_never()
                                                            && ::tracing::__macro_support::__is_enabled(
                                                                __CALLSITE.metadata(),
                                                                interest,
                                                            )
                                                    };
                                                if enabled {
                                                    (|value_set: ::tracing::field::ValueSet| {
                                                        let meta = __CALLSITE.metadata();
                                                        ::tracing::Event::dispatch(meta, &value_set);
                                                    })({
                                                        #[allow(unused_imports)]
                                                        use ::tracing::field::{debug, display, Value};
                                                        let mut iter = __CALLSITE.metadata().fields().iter();
                                                        __CALLSITE
                                                            .metadata()
                                                            .fields()
                                                            .value_set(
                                                                &[
                                                                    (
                                                                        &::core::iter::Iterator::next(&mut iter)
                                                                            .expect("FieldSet corrupted (this is a bug)"),
                                                                        ::core::option::Option::Some(
                                                                            &display(&EventOutcome::RetriesExhausted) as &dyn Value,
                                                                        ),
                                                                    ),
                                                                ],
                                                            )
                                                    });
                                                } else {
                                                }
                                            };
                                            Some(Err(err))
                                        }
                                    }
                                }
                            }
                        }
                            .instrument(span)
                            .await;
                        if let Some(res) = res {
                            break res;
                        }
                    }
                }
                Ok(None) => Ok(()),
                Err(err) => {
                    {
                        use ::tracing::__macro_support::Callsite as _;
                        static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                            static META: ::tracing::Metadata<'static> = {
                                ::tracing_core::metadata::Metadata::new(
                                    "event src/handler/retry.rs:122",
                                    "tx_indexer::handler::retry",
                                    Level::ERROR,
                                    ::core::option::Option::Some("src/handler/retry.rs"),
                                    ::core::option::Option::Some(122u32),
                                    ::core::option::Option::Some("tx_indexer::handler::retry"),
                                    ::tracing_core::field::FieldSet::new(
                                        &["err"],
                                        ::tracing_core::callsite::Identifier(&__CALLSITE),
                                    ),
                                    ::tracing::metadata::Kind::EVENT,
                                )
                            };
                            ::tracing::callsite::DefaultCallsite::new(&META)
                        };
                        let enabled = Level::ERROR
                            <= ::tracing::level_filters::STATIC_MAX_LEVEL
                            && Level::ERROR
                                <= ::tracing::level_filters::LevelFilter::current()
                            && {
                                let interest = __CALLSITE.interest();
                                !interest.is_never()
                                    && ::tracing::__macro_support::__is_enabled(
                                        __CALLSITE.metadata(),
                                        interest,
                                    )
                            };
                        if enabled {
                            (|value_set: ::tracing::field::ValueSet| {
                                let meta = __CALLSITE.metadata();
                                ::tracing::Event::dispatch(meta, &value_set);
                            })({
                                #[allow(unused_imports)]
                                use ::tracing::field::{debug, display, Value};
                                let mut iter = __CALLSITE.metadata().fields().iter();
                                __CALLSITE
                                    .metadata()
                                    .fields()
                                    .value_set(
                                        &[
                                            (
                                                &::core::iter::Iterator::next(&mut iter)
                                                    .expect("FieldSet corrupted (this is a bug)"),
                                                ::core::option::Option::Some(&debug(&err) as &dyn Value),
                                            ),
                                        ],
                                    )
                            });
                        } else {
                        }
                    };
                    Ok(())
                }
            }
        }
    }
}
pub use indexer::TxIndexer;
mod indexer {
    use crate::{
        config::{n2c_config, n2n_config, NodeAddress, TxIndexerConfig},
        handler::callback::{Callback, EventHandler},
        progress_tracker::ProgressTracker,
    };
    use anyhow::Result;
    use oura::{
        pipelining::{FilterProvider, SinkProvider, SourceProvider},
        sources::{AddressArg, BearerKind},
        utils::{Utils, WithUtils},
        Error,
    };
    use std::sync::Arc;
    use std::thread::JoinHandle;
    use tracing::{span, Level};
    pub struct TxIndexer {
        pub source_handle: JoinHandle<()>,
        pub filter_handle: JoinHandle<()>,
        pub sink_handle: JoinHandle<()>,
    }
    impl TxIndexer {
        pub async fn run<H: EventHandler>(
            conf: TxIndexerConfig<H>,
        ) -> Result<TxIndexer, Error> {
            let span = {
                use ::tracing::__macro_support::Callsite as _;
                static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                    static META: ::tracing::Metadata<'static> = {
                        ::tracing_core::metadata::Metadata::new(
                            "Run TxIndexer",
                            "tx_indexer::indexer",
                            Level::INFO,
                            ::core::option::Option::Some("src/indexer.rs"),
                            ::core::option::Option::Some(27u32),
                            ::core::option::Option::Some("tx_indexer::indexer"),
                            ::tracing_core::field::FieldSet::new(
                                &[],
                                ::tracing_core::callsite::Identifier(&__CALLSITE),
                            ),
                            ::tracing::metadata::Kind::SPAN,
                        )
                    };
                    ::tracing::callsite::DefaultCallsite::new(&META)
                };
                let mut interest = ::tracing::subscriber::Interest::never();
                if Level::INFO <= ::tracing::level_filters::STATIC_MAX_LEVEL
                    && Level::INFO <= ::tracing::level_filters::LevelFilter::current()
                    && {
                        interest = __CALLSITE.interest();
                        !interest.is_never()
                    }
                    && ::tracing::__macro_support::__is_enabled(
                        __CALLSITE.metadata(),
                        interest,
                    )
                {
                    let meta = __CALLSITE.metadata();
                    ::tracing::Span::new(meta, &{ meta.fields().value_set(&[]) })
                } else {
                    let span = ::tracing::__macro_support::__disabled_span(
                        __CALLSITE.metadata(),
                    );
                    {};
                    span
                }
            };
            let _enter = span.enter();
            let chain = conf.network.to_chain_info()?;
            let progress_tracker = match conf.since_slot {
                Some((since_slot, _)) => Some(ProgressTracker::new(since_slot, &chain)?),
                None => None,
            };
            let utils = Arc::new(Utils::new(chain));
            let (source_handle, source_rx) = match conf.node_address {
                NodeAddress::UnixSocket(path) => {
                    {
                        use ::tracing::__macro_support::Callsite as _;
                        static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                            static META: ::tracing::Metadata<'static> = {
                                ::tracing_core::metadata::Metadata::new(
                                    "BootstrapSourceViaSocket",
                                    "tx_indexer::indexer",
                                    Level::INFO,
                                    ::core::option::Option::Some("src/indexer.rs"),
                                    ::core::option::Option::Some(41u32),
                                    ::core::option::Option::Some("tx_indexer::indexer"),
                                    ::tracing_core::field::FieldSet::new(
                                        &["socket_path"],
                                        ::tracing_core::callsite::Identifier(&__CALLSITE),
                                    ),
                                    ::tracing::metadata::Kind::SPAN,
                                )
                            };
                            ::tracing::callsite::DefaultCallsite::new(&META)
                        };
                        let mut interest = ::tracing::subscriber::Interest::never();
                        if Level::INFO <= ::tracing::level_filters::STATIC_MAX_LEVEL
                            && Level::INFO
                                <= ::tracing::level_filters::LevelFilter::current()
                            && {
                                interest = __CALLSITE.interest();
                                !interest.is_never()
                            }
                            && ::tracing::__macro_support::__is_enabled(
                                __CALLSITE.metadata(),
                                interest,
                            )
                        {
                            let meta = __CALLSITE.metadata();
                            ::tracing::Span::new(
                                meta,
                                &{
                                    #[allow(unused_imports)]
                                    use ::tracing::field::{debug, display, Value};
                                    let mut iter = meta.fields().iter();
                                    meta.fields()
                                        .value_set(
                                            &[
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(&path as &dyn Value),
                                                ),
                                            ],
                                        )
                                },
                            )
                        } else {
                            let span = ::tracing::__macro_support::__disabled_span(
                                __CALLSITE.metadata(),
                            );
                            {};
                            span
                        }
                    }
                        .in_scope(|| {
                            WithUtils::new(
                                    n2c_config(
                                        AddressArg(BearerKind::Unix, path),
                                        conf.network.to_magic_arg(),
                                        conf.since_slot.clone(),
                                        conf.safe_block_depth,
                                    ),
                                    utils.clone(),
                                )
                                .bootstrap()
                        })
                }
                NodeAddress::TcpAddress(hostname, port) => {
                    {
                        use ::tracing::__macro_support::Callsite as _;
                        static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                            static META: ::tracing::Metadata<'static> = {
                                ::tracing_core::metadata::Metadata::new(
                                    "BootstrapSourceViaTcp",
                                    "tx_indexer::indexer",
                                    Level::INFO,
                                    ::core::option::Option::Some("src/indexer.rs"),
                                    ::core::option::Option::Some(55u32),
                                    ::core::option::Option::Some("tx_indexer::indexer"),
                                    ::tracing_core::field::FieldSet::new(
                                        &["hostname", "port"],
                                        ::tracing_core::callsite::Identifier(&__CALLSITE),
                                    ),
                                    ::tracing::metadata::Kind::SPAN,
                                )
                            };
                            ::tracing::callsite::DefaultCallsite::new(&META)
                        };
                        let mut interest = ::tracing::subscriber::Interest::never();
                        if Level::INFO <= ::tracing::level_filters::STATIC_MAX_LEVEL
                            && Level::INFO
                                <= ::tracing::level_filters::LevelFilter::current()
                            && {
                                interest = __CALLSITE.interest();
                                !interest.is_never()
                            }
                            && ::tracing::__macro_support::__is_enabled(
                                __CALLSITE.metadata(),
                                interest,
                            )
                        {
                            let meta = __CALLSITE.metadata();
                            ::tracing::Span::new(
                                meta,
                                &{
                                    #[allow(unused_imports)]
                                    use ::tracing::field::{debug, display, Value};
                                    let mut iter = meta.fields().iter();
                                    meta.fields()
                                        .value_set(
                                            &[
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(&hostname as &dyn Value),
                                                ),
                                                (
                                                    &::core::iter::Iterator::next(&mut iter)
                                                        .expect("FieldSet corrupted (this is a bug)"),
                                                    ::core::option::Option::Some(&port as &dyn Value),
                                                ),
                                            ],
                                        )
                                },
                            )
                        } else {
                            let span = ::tracing::__macro_support::__disabled_span(
                                __CALLSITE.metadata(),
                            );
                            {};
                            span
                        }
                    }
                        .in_scope(|| {
                            WithUtils::new(
                                    n2n_config(
                                        AddressArg(
                                            BearerKind::Tcp,
                                            {
                                                let res = ::alloc::fmt::format(
                                                    format_args!("{0}:{1}", hostname, port),
                                                );
                                                res
                                            },
                                        ),
                                        conf.network.to_magic_arg(),
                                        conf.since_slot.clone(),
                                        conf.safe_block_depth,
                                    ),
                                    utils.clone(),
                                )
                                .bootstrap()
                        })
                }
            }?;
            let (filter_handle, filter_rx) = conf
                .event_filter
                .to_selection_config()
                .bootstrap(source_rx)?;
            let sink_handle = {
                use ::tracing::__macro_support::Callsite as _;
                static __CALLSITE: ::tracing::callsite::DefaultCallsite = {
                    static META: ::tracing::Metadata<'static> = {
                        ::tracing_core::metadata::Metadata::new(
                            "BootstrapSink",
                            "tx_indexer::indexer",
                            Level::INFO,
                            ::core::option::Option::Some("src/indexer.rs"),
                            ::core::option::Option::Some(76u32),
                            ::core::option::Option::Some("tx_indexer::indexer"),
                            ::tracing_core::field::FieldSet::new(
                                &[],
                                ::tracing_core::callsite::Identifier(&__CALLSITE),
                            ),
                            ::tracing::metadata::Kind::SPAN,
                        )
                    };
                    ::tracing::callsite::DefaultCallsite::new(&META)
                };
                let mut interest = ::tracing::subscriber::Interest::never();
                if Level::INFO <= ::tracing::level_filters::STATIC_MAX_LEVEL
                    && Level::INFO <= ::tracing::level_filters::LevelFilter::current()
                    && {
                        interest = __CALLSITE.interest();
                        !interest.is_never()
                    }
                    && ::tracing::__macro_support::__is_enabled(
                        __CALLSITE.metadata(),
                        interest,
                    )
                {
                    let meta = __CALLSITE.metadata();
                    ::tracing::Span::new(meta, &{ meta.fields().value_set(&[]) })
                } else {
                    let span = ::tracing::__macro_support::__disabled_span(
                        __CALLSITE.metadata(),
                    );
                    {};
                    span
                }
            }
                .in_scope(|| {
                    Callback::new(
                            conf.handler,
                            conf.retry_policy,
                            utils,
                            progress_tracker,
                        )
                        .bootstrap(filter_rx)
                })?;
            Ok(TxIndexer {
                source_handle,
                filter_handle,
                sink_handle,
            })
        }
    }
}
pub mod progress_tracker {
    use std::sync::{atomic::AtomicUsize, Arc};
    use anyhow::anyhow;
    use chrono::{DateTime, Duration, Utc};
    use oura::utils::ChainWellKnownInfo;
    use tx_bakery::chain_query::{EraParameters, EraSummary, EraTime};
    use crate::from_oura::OuraParseError;
    /// A progress tracker holds information about the chain info required to calculate
    /// POSIX time from slots
    pub struct ProgressTracker {
        pub system_start: DateTime<Utc>,
        pub era_summaries: Vec<EraSummary>,
        pub since_slot: u64,
        pub sync_progress: Arc<AtomicUsize>,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ProgressTracker {
        #[inline]
        fn clone(&self) -> ProgressTracker {
            ProgressTracker {
                system_start: ::core::clone::Clone::clone(&self.system_start),
                era_summaries: ::core::clone::Clone::clone(&self.era_summaries),
                since_slot: ::core::clone::Clone::clone(&self.since_slot),
                sync_progress: ::core::clone::Clone::clone(&self.sync_progress),
            }
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ProgressTracker {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "ProgressTracker",
                "system_start",
                &self.system_start,
                "era_summaries",
                &self.era_summaries,
                "since_slot",
                &self.since_slot,
                "sync_progress",
                &&self.sync_progress,
            )
        }
    }
    impl ProgressTracker {
        pub fn new(
            since_slot: u64,
            chain_info: &ChainWellKnownInfo,
        ) -> Result<Self, anyhow::Error> {
            let system_start = DateTime::from_timestamp(
                    chain_info.byron_known_time as i64,
                    0,
                )
                .ok_or(
                    ::anyhow::__private::must_use({
                        let error = ::anyhow::__private::format_err(
                            format_args!(
                                "Unable to convert shelley_known_time to to DateTime",
                            ),
                        );
                        error
                    }),
                )?;
            Ok(ProgressTracker {
                system_start,
                era_summaries: chain_info_to_era_summaries(&system_start, chain_info)?,
                since_slot,
                sync_progress: Arc::new(AtomicUsize::new(0)),
            })
        }
        pub fn get_percentage(&self, slot: u64) -> Result<f32, OuraParseError> {
            let current_time = Utc::now();
            let current_slot = tx_bakery::time::time_into_slot(
                    &self.era_summaries,
                    &self.system_start,
                    current_time,
                )
                .map_err(OuraParseError::TimeConversionError)?;
            let synced = slot - self.since_slot;
            let to_be_synced = current_slot - self.since_slot;
            Ok(synced as f32 * 100.0 / to_be_synced as f32)
        }
    }
    /// Convert Oura chain info into Ogmios EraSummaries.
    /// Oura does not include all eras, only Byron and Shelley, all other eras are part of
    /// Shelley. This is good enough for time calculations.
    fn chain_info_to_era_summaries(
        system_start_time: &DateTime<Utc>,
        chain_info: &ChainWellKnownInfo,
    ) -> Result<Vec<EraSummary>, anyhow::Error> {
        let byron_start = EraTime {
            time: Duration::zero(),
            slot: 0,
            epoch: 0,
        };
        let shelley_start = EraTime {
            time: DateTime::from_timestamp(chain_info.shelley_known_time as i64, 0)
                .ok_or(
                    ::anyhow::__private::must_use({
                        let error = ::anyhow::__private::format_err(
                            format_args!(
                                "Unable to convert shelley_known_time to to DateTime",
                            ),
                        );
                        error
                    }),
                )? - system_start_time,
            slot: chain_info.shelley_known_slot,
            epoch: chain_info.shelley_known_slot / chain_info.byron_epoch_length as u64,
        };
        Ok(
            <[_]>::into_vec(
                #[rustc_box]
                ::alloc::boxed::Box::new([
                    EraSummary {
                        start: byron_start,
                        end: Some(shelley_start.clone()),
                        parameters: EraParameters {
                            epoch_length: chain_info.byron_epoch_length as u64,
                            slot_length: chain_info.byron_slot_length as u64 * 1000,
                            safe_zone: Some(4320),
                        },
                    },
                    EraSummary {
                        start: shelley_start,
                        end: None,
                        parameters: EraParameters {
                            epoch_length: chain_info.shelley_epoch_length as u64,
                            slot_length: chain_info.shelley_slot_length as u64 * 1000,
                            safe_zone: Some(4320),
                        },
                    },
                ]),
            ),
        )
    }
}
#[cfg(feature = "diesel")]
pub mod schema {
    #[allow(unused_imports, dead_code, unreachable_pub, unused_qualifications)]
    pub mod sync_progress {
        use ::diesel;
        pub use self::columns::*;
        use crate::database::plutus::sql_types::*;
        use diesel::sql_types::*;
        /// Re-exports all of the columns of this table, as well as the
        /// table struct renamed to the module name. This is meant to be
        /// glob imported for functions which only deal with one table.
        pub mod dsl {
            pub use super::columns::block_slot;
            pub use super::columns::block_hash;
            pub use super::columns::processed;
            pub use super::table as sync_progress;
        }
        #[allow(non_upper_case_globals, dead_code)]
        /// A tuple of all of the columns on this table
        pub const all_columns: (block_slot, block_hash, processed) = (
            block_slot,
            block_hash,
            processed,
        );
        #[allow(non_camel_case_types)]
        /// The actual table struct
        ///
        /// This is the type which provides the base methods of the query
        /// builder, such as `.select` and `.filter`.
        pub struct table;
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::fmt::Debug for table {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::write_str(f, "table")
            }
        }
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::clone::Clone for table {
            #[inline]
            fn clone(&self) -> table {
                *self
            }
        }
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::marker::Copy for table {}
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::query_builder::QueryId;
            #[allow(non_camel_case_types)]
            impl QueryId for table {
                type QueryId = table;
                const HAS_STATIC_QUERY_ID: bool = true;
            }
        };
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::default::Default for table {
            #[inline]
            fn default() -> table {
                table {}
            }
        }
        impl table {
            #[allow(dead_code)]
            /// Represents `table_name.*`, which is sometimes necessary
            /// for efficient count queries. It cannot be used in place of
            /// `all_columns`
            pub fn star(&self) -> star {
                star
            }
        }
        /// The SQL type of all of the columns on this table
        pub type SqlType = (Int8, Bytea, Bool);
        /// Helper type for representing a boxed query from this table
        pub type BoxedQuery<'a, DB, ST = SqlType> = diesel::internal::table_macro::BoxedSelectStatement<
            'a,
            ST,
            diesel::internal::table_macro::FromClause<table>,
            DB,
        >;
        impl diesel::QuerySource for table {
            type FromClause = diesel::internal::table_macro::StaticQueryFragmentInstance<
                table,
            >;
            type DefaultSelection = <Self as diesel::Table>::AllColumns;
            fn from_clause(&self) -> Self::FromClause {
                diesel::internal::table_macro::StaticQueryFragmentInstance::new()
            }
            fn default_selection(&self) -> Self::DefaultSelection {
                use diesel::Table;
                Self::all_columns()
            }
        }
        impl<DB> diesel::query_builder::QueryFragment<DB> for table
        where
            DB: diesel::backend::Backend,
            <table as diesel::internal::table_macro::StaticQueryFragment>::Component: diesel::query_builder::QueryFragment<
                DB,
            >,
        {
            fn walk_ast<'b>(
                &'b self,
                __diesel_internal_pass: diesel::query_builder::AstPass<'_, 'b, DB>,
            ) -> diesel::result::QueryResult<()> {
                <table as diesel::internal::table_macro::StaticQueryFragment>::STATIC_COMPONENT
                    .walk_ast(__diesel_internal_pass)
            }
        }
        impl diesel::internal::table_macro::StaticQueryFragment for table {
            type Component = diesel::internal::table_macro::Identifier<'static>;
            const STATIC_COMPONENT: &'static Self::Component = &diesel::internal::table_macro::Identifier(
                "sync_progress",
            );
        }
        impl diesel::query_builder::AsQuery for table {
            type SqlType = SqlType;
            type Query = diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<Self>,
            >;
            fn as_query(self) -> Self::Query {
                diesel::internal::table_macro::SelectStatement::simple(self)
            }
        }
        impl diesel::Table for table {
            type PrimaryKey = processed;
            type AllColumns = (block_slot, block_hash, processed);
            fn primary_key(&self) -> Self::PrimaryKey {
                processed
            }
            fn all_columns() -> Self::AllColumns {
                (block_slot, block_hash, processed)
            }
        }
        impl diesel::associations::HasTable for table {
            type Table = Self;
            fn table() -> Self::Table {
                table
            }
        }
        impl diesel::query_builder::IntoUpdateTarget for table {
            type WhereClause = <<Self as diesel::query_builder::AsQuery>::Query as diesel::query_builder::IntoUpdateTarget>::WhereClause;
            fn into_update_target(
                self,
            ) -> diesel::query_builder::UpdateTarget<Self::Table, Self::WhereClause> {
                use diesel::query_builder::AsQuery;
                let q: diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<table>,
                > = self.as_query();
                q.into_update_target()
            }
        }
        impl diesel::query_source::AppearsInFromClause<table> for table {
            type Count = diesel::query_source::Once;
        }
        impl<S> diesel::internal::table_macro::AliasAppearsInFromClause<S, table>
        for table
        where
            S: diesel::query_source::AliasSource<Target = table>,
        {
            type Count = diesel::query_source::Never;
        }
        impl<
            S1,
            S2,
        > diesel::internal::table_macro::AliasAliasAppearsInFromClause<table, S2, S1>
        for table
        where
            S1: diesel::query_source::AliasSource<Target = table>,
            S2: diesel::query_source::AliasSource<Target = table>,
            S1: diesel::internal::table_macro::AliasAliasAppearsInFromClauseSameTable<
                S2,
                table,
            >,
        {
            type Count = <S1 as diesel::internal::table_macro::AliasAliasAppearsInFromClauseSameTable<
                S2,
                table,
            >>::Count;
        }
        impl<S> diesel::query_source::AppearsInFromClause<diesel::query_source::Alias<S>>
        for table
        where
            S: diesel::query_source::AliasSource,
        {
            type Count = diesel::query_source::Never;
        }
        impl<
            S,
            C,
        > diesel::internal::table_macro::FieldAliasMapperAssociatedTypesDisjointnessTrick<
            table,
            S,
            C,
        > for table
        where
            S: diesel::query_source::AliasSource<Target = table> + ::std::clone::Clone,
            C: diesel::query_source::Column<Table = table>,
        {
            type Out = diesel::query_source::AliasedField<S, C>;
            fn map(
                __diesel_internal_column: C,
                __diesel_internal_alias: &diesel::query_source::Alias<S>,
            ) -> Self::Out {
                __diesel_internal_alias.field(__diesel_internal_column)
            }
        }
        impl diesel::query_source::AppearsInFromClause<table>
        for diesel::internal::table_macro::NoFromClause {
            type Count = diesel::query_source::Never;
        }
        impl<
            Left,
            Right,
            Kind,
        > diesel::JoinTo<diesel::internal::table_macro::Join<Left, Right, Kind>>
        for table
        where
            diesel::internal::table_macro::Join<
                Left,
                Right,
                Kind,
            >: diesel::JoinTo<table>,
            Left: diesel::query_source::QuerySource,
            Right: diesel::query_source::QuerySource,
        {
            type FromClause = diesel::internal::table_macro::Join<Left, Right, Kind>;
            type OnClause = <diesel::internal::table_macro::Join<
                Left,
                Right,
                Kind,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    Kind,
                >,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::Join::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<Join, On> diesel::JoinTo<diesel::internal::table_macro::JoinOn<Join, On>>
        for table
        where
            diesel::internal::table_macro::JoinOn<Join, On>: diesel::JoinTo<table>,
        {
            type FromClause = diesel::internal::table_macro::JoinOn<Join, On>;
            type OnClause = <diesel::internal::table_macro::JoinOn<
                Join,
                On,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::JoinOn<Join, On>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::JoinOn::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<
            F,
            S,
            D,
            W,
            O,
            L,
            Of,
            G,
        > diesel::JoinTo<
            diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            >,
        > for table
        where
            diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            >: diesel::JoinTo<table>,
            F: diesel::query_source::QuerySource,
        {
            type FromClause = diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            >;
            type OnClause = <diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<F>,
                    S,
                    D,
                    W,
                    O,
                    L,
                    Of,
                    G,
                >,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::SelectStatement::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<
            'a,
            QS,
            ST,
            DB,
        > diesel::JoinTo<
            diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            >,
        > for table
        where
            diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            >: diesel::JoinTo<table>,
            QS: diesel::query_source::QuerySource,
        {
            type FromClause = diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            >;
            type OnClause = <diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::BoxedSelectStatement<
                    'a,
                    diesel::internal::table_macro::FromClause<QS>,
                    ST,
                    DB,
                >,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::BoxedSelectStatement::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<S> diesel::JoinTo<diesel::query_source::Alias<S>> for table
        where
            diesel::query_source::Alias<S>: diesel::JoinTo<table>,
        {
            type FromClause = diesel::query_source::Alias<S>;
            type OnClause = <diesel::query_source::Alias<
                S,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::query_source::Alias<S>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::query_source::Alias::<
                    S,
                >::join_target(table);
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<T> diesel::insertable::Insertable<T> for table
        where
            <table as diesel::query_builder::AsQuery>::Query: diesel::insertable::Insertable<
                T,
            >,
        {
            type Values = <<table as diesel::query_builder::AsQuery>::Query as diesel::insertable::Insertable<
                T,
            >>::Values;
            fn values(self) -> Self::Values {
                use diesel::query_builder::AsQuery;
                self.as_query().values()
            }
        }
        impl<'a, T> diesel::insertable::Insertable<T> for &'a table
        where
            table: diesel::insertable::Insertable<T>,
        {
            type Values = <table as diesel::insertable::Insertable<T>>::Values;
            fn values(self) -> Self::Values {
                (*self).values()
            }
        }
        impl<S> diesel::JoinTo<diesel::query_builder::Only<S>> for table
        where
            diesel::query_builder::Only<S>: diesel::JoinTo<table>,
        {
            type FromClause = diesel::query_builder::Only<S>;
            type OnClause = <diesel::query_builder::Only<
                S,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::query_builder::Only<S>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::query_builder::Only::<
                    S,
                >::join_target(table);
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl diesel::query_source::AppearsInFromClause<
            diesel::query_builder::Only<table>,
        > for table {
            type Count = diesel::query_source::Once;
        }
        impl diesel::query_source::AppearsInFromClause<table>
        for diesel::query_builder::Only<table> {
            type Count = diesel::query_source::Once;
        }
        impl<S, TSM> diesel::JoinTo<diesel::query_builder::Tablesample<S, TSM>> for table
        where
            diesel::query_builder::Tablesample<S, TSM>: diesel::JoinTo<table>,
            TSM: diesel::internal::table_macro::TablesampleMethod,
        {
            type FromClause = diesel::query_builder::Tablesample<S, TSM>;
            type OnClause = <diesel::query_builder::Tablesample<
                S,
                TSM,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::query_builder::Tablesample<S, TSM>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::query_builder::Tablesample::<
                    S,
                    TSM,
                >::join_target(table);
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<
            TSM,
        > diesel::query_source::AppearsInFromClause<
            diesel::query_builder::Tablesample<table, TSM>,
        > for table
        where
            TSM: diesel::internal::table_macro::TablesampleMethod,
        {
            type Count = diesel::query_source::Once;
        }
        impl<TSM> diesel::query_source::AppearsInFromClause<table>
        for diesel::query_builder::Tablesample<table, TSM>
        where
            TSM: diesel::internal::table_macro::TablesampleMethod,
        {
            type Count = diesel::query_source::Once;
        }
        /// Contains all of the columns of this table
        pub mod columns {
            use ::diesel;
            use super::table;
            use crate::database::plutus::sql_types::*;
            use diesel::sql_types::*;
            #[allow(non_camel_case_types, dead_code)]
            /// Represents `table_name.*`, which is sometimes needed for
            /// efficient count queries. It cannot be used in place of
            /// `all_columns`, and has a `SqlType` of `()` to prevent it
            /// being used that way
            pub struct star;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for star {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "star")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for star {
                #[inline]
                fn clone(&self) -> star {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for star {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for star {
                    type QueryId = star;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            impl<__GB> diesel::expression::ValidGrouping<__GB> for star
            where
                (
                    block_slot,
                    block_hash,
                    processed,
                ): diesel::expression::ValidGrouping<__GB>,
            {
                type IsAggregate = <(
                    block_slot,
                    block_hash,
                    processed,
                ) as diesel::expression::ValidGrouping<__GB>>::IsAggregate;
            }
            impl diesel::Expression for star {
                type SqlType = diesel::expression::expression_types::NotSelectable;
            }
            impl<DB: diesel::backend::Backend> diesel::query_builder::QueryFragment<DB>
            for star
            where
                <table as diesel::QuerySource>::FromClause: diesel::query_builder::QueryFragment<
                    DB,
                >,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    use diesel::QuerySource;
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_sql("*");
                    Ok(())
                }
            }
            impl diesel::SelectableExpression<table> for star {}
            impl diesel::AppearsOnTable<table> for star {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct block_slot;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for block_slot {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "block_slot")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for block_slot {
                #[inline]
                fn clone(&self) -> block_slot {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for block_slot {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for block_slot {
                    type QueryId = block_slot;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for block_slot {
                #[inline]
                fn default() -> block_slot {
                    block_slot {}
                }
            }
            impl diesel::expression::Expression for block_slot {
                type SqlType = Int8;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for block_slot
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("block_slot")
                }
            }
            impl diesel::SelectableExpression<super::table> for block_slot {}
            impl<QS> diesel::AppearsOnTable<QS> for block_slot
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for block_slot
            where
                block_slot: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for block_slot
            where
                block_slot: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for block_slot
            where
                block_slot: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for block_slot
            where
                From: diesel::query_source::QuerySource,
                block_slot: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for block_slot
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    block_slot,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for block_slot {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<block_slot> for block_slot {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for block_slot {
                type Table = super::table;
                const NAME: &'static str = "block_slot";
            }
            impl<T> diesel::EqAll<T> for block_slot
            where
                T: diesel::expression::AsExpression<Int8>,
                diesel::dsl::Eq<
                    block_slot,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl<Rhs> ::std::ops::Add<Rhs> for block_slot
            where
                Rhs: diesel::expression::AsExpression<
                    <<block_slot as diesel::Expression>::SqlType as diesel::sql_types::ops::Add>::Rhs,
                >,
            {
                type Output = diesel::internal::table_macro::ops::Add<
                    Self,
                    Rhs::Expression,
                >;
                fn add(self, __diesel_internal_rhs: Rhs) -> Self::Output {
                    diesel::internal::table_macro::ops::Add::new(
                        self,
                        __diesel_internal_rhs.as_expression(),
                    )
                }
            }
            impl<Rhs> ::std::ops::Sub<Rhs> for block_slot
            where
                Rhs: diesel::expression::AsExpression<
                    <<block_slot as diesel::Expression>::SqlType as diesel::sql_types::ops::Sub>::Rhs,
                >,
            {
                type Output = diesel::internal::table_macro::ops::Sub<
                    Self,
                    Rhs::Expression,
                >;
                fn sub(self, __diesel_internal_rhs: Rhs) -> Self::Output {
                    diesel::internal::table_macro::ops::Sub::new(
                        self,
                        __diesel_internal_rhs.as_expression(),
                    )
                }
            }
            impl<Rhs> ::std::ops::Div<Rhs> for block_slot
            where
                Rhs: diesel::expression::AsExpression<
                    <<block_slot as diesel::Expression>::SqlType as diesel::sql_types::ops::Div>::Rhs,
                >,
            {
                type Output = diesel::internal::table_macro::ops::Div<
                    Self,
                    Rhs::Expression,
                >;
                fn div(self, __diesel_internal_rhs: Rhs) -> Self::Output {
                    diesel::internal::table_macro::ops::Div::new(
                        self,
                        __diesel_internal_rhs.as_expression(),
                    )
                }
            }
            impl<Rhs> ::std::ops::Mul<Rhs> for block_slot
            where
                Rhs: diesel::expression::AsExpression<
                    <<block_slot as diesel::Expression>::SqlType as diesel::sql_types::ops::Mul>::Rhs,
                >,
            {
                type Output = diesel::internal::table_macro::ops::Mul<
                    Self,
                    Rhs::Expression,
                >;
                fn mul(self, __diesel_internal_rhs: Rhs) -> Self::Output {
                    diesel::internal::table_macro::ops::Mul::new(
                        self,
                        __diesel_internal_rhs.as_expression(),
                    )
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for block_slot {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for block_slot {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for block_slot
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for block_slot
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct block_hash;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for block_hash {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "block_hash")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for block_hash {
                #[inline]
                fn clone(&self) -> block_hash {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for block_hash {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for block_hash {
                    type QueryId = block_hash;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for block_hash {
                #[inline]
                fn default() -> block_hash {
                    block_hash {}
                }
            }
            impl diesel::expression::Expression for block_hash {
                type SqlType = Bytea;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for block_hash
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("block_hash")
                }
            }
            impl diesel::SelectableExpression<super::table> for block_hash {}
            impl<QS> diesel::AppearsOnTable<QS> for block_hash
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for block_hash
            where
                block_hash: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for block_hash
            where
                block_hash: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for block_hash
            where
                block_hash: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for block_hash
            where
                From: diesel::query_source::QuerySource,
                block_hash: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for block_hash
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    block_hash,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for block_hash {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<block_hash> for block_hash {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for block_hash {
                type Table = super::table;
                const NAME: &'static str = "block_hash";
            }
            impl<T> diesel::EqAll<T> for block_hash
            where
                T: diesel::expression::AsExpression<Bytea>,
                diesel::dsl::Eq<
                    block_hash,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for block_hash {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for block_hash {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for block_hash
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for block_hash
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct processed;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for processed {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "processed")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for processed {
                #[inline]
                fn clone(&self) -> processed {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for processed {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for processed {
                    type QueryId = processed;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for processed {
                #[inline]
                fn default() -> processed {
                    processed {}
                }
            }
            impl diesel::expression::Expression for processed {
                type SqlType = Bool;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for processed
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("processed")
                }
            }
            impl diesel::SelectableExpression<super::table> for processed {}
            impl<QS> diesel::AppearsOnTable<QS> for processed
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for processed
            where
                processed: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for processed
            where
                processed: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for processed
            where
                processed: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for processed
            where
                From: diesel::query_source::QuerySource,
                processed: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for processed
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    processed,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for processed {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<processed> for processed {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for processed {
                type Table = super::table;
                const NAME: &'static str = "processed";
            }
            impl<T> diesel::EqAll<T> for processed
            where
                T: diesel::expression::AsExpression<Bool>,
                diesel::dsl::Eq<
                    processed,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for processed {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for processed {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for processed
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for processed
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            impl diesel::expression::IsContainedInGroupBy<block_slot> for block_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<block_hash> for block_slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<block_slot> for processed {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<processed> for block_slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<block_hash> for processed {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<processed> for block_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
        }
    }
    #[allow(unused_imports, dead_code, unreachable_pub, unused_qualifications)]
    pub mod testdb {
        use ::diesel;
        pub use self::columns::*;
        use crate::database::plutus::sql_types::*;
        use diesel::sql_types::*;
        /// Re-exports all of the columns of this table, as well as the
        /// table struct renamed to the module name. This is meant to be
        /// glob imported for functions which only deal with one table.
        pub mod dsl {
            pub use super::columns::id;
            pub use super::columns::cur_sym;
            pub use super::columns::token_name;
            pub use super::columns::tx_hash;
            pub use super::columns::pub_key_hash;
            pub use super::columns::script_hash;
            pub use super::columns::datum_hash;
            pub use super::columns::slot;
            pub use super::columns::plutus_data;
            pub use super::columns::cred;
            pub use super::columns::chain_pointer;
            pub use super::columns::staking_cred;
            pub use super::columns::address;
            pub use super::columns::asset_quantity;
            pub use super::columns::value;
            pub use super::columns::tx_in;
            pub use super::columns::datum;
            pub use super::columns::tx_out;
            pub use super::columns::tx_in_info;
            pub use super::table as testdb;
        }
        #[allow(non_upper_case_globals, dead_code)]
        /// A tuple of all of the columns on this table
        pub const all_columns: (
            id,
            cur_sym,
            token_name,
            tx_hash,
            pub_key_hash,
            script_hash,
            datum_hash,
            slot,
            plutus_data,
            cred,
            chain_pointer,
            staking_cred,
            address,
            asset_quantity,
            value,
            tx_in,
            datum,
            tx_out,
            tx_in_info,
        ) = (
            id,
            cur_sym,
            token_name,
            tx_hash,
            pub_key_hash,
            script_hash,
            datum_hash,
            slot,
            plutus_data,
            cred,
            chain_pointer,
            staking_cred,
            address,
            asset_quantity,
            value,
            tx_in,
            datum,
            tx_out,
            tx_in_info,
        );
        #[allow(non_camel_case_types)]
        /// The actual table struct
        ///
        /// This is the type which provides the base methods of the query
        /// builder, such as `.select` and `.filter`.
        pub struct table;
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::fmt::Debug for table {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::write_str(f, "table")
            }
        }
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::clone::Clone for table {
            #[inline]
            fn clone(&self) -> table {
                *self
            }
        }
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::marker::Copy for table {}
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::query_builder::QueryId;
            #[allow(non_camel_case_types)]
            impl QueryId for table {
                type QueryId = table;
                const HAS_STATIC_QUERY_ID: bool = true;
            }
        };
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::default::Default for table {
            #[inline]
            fn default() -> table {
                table {}
            }
        }
        impl table {
            #[allow(dead_code)]
            /// Represents `table_name.*`, which is sometimes necessary
            /// for efficient count queries. It cannot be used in place of
            /// `all_columns`
            pub fn star(&self) -> star {
                star
            }
        }
        /// The SQL type of all of the columns on this table
        pub type SqlType = (
            Int8,
            Nullable<CurrencySymbol>,
            Nullable<TokenName>,
            Nullable<TransactionHash>,
            Nullable<Ed25519PubKeyHash>,
            Nullable<ScriptHash>,
            Nullable<DatumHash>,
            Nullable<Slot>,
            Nullable<PlutusData>,
            Nullable<Credential>,
            Nullable<ChainPointer>,
            Nullable<StakingCredential>,
            Nullable<Address>,
            Nullable<AssetQuantity>,
            Nullable<Value>,
            Nullable<TransactionInput>,
            Nullable<OutputDatum>,
            Nullable<TransactionOutput>,
            Nullable<TxInInfo>,
        );
        /// Helper type for representing a boxed query from this table
        pub type BoxedQuery<'a, DB, ST = SqlType> = diesel::internal::table_macro::BoxedSelectStatement<
            'a,
            ST,
            diesel::internal::table_macro::FromClause<table>,
            DB,
        >;
        impl diesel::QuerySource for table {
            type FromClause = diesel::internal::table_macro::StaticQueryFragmentInstance<
                table,
            >;
            type DefaultSelection = <Self as diesel::Table>::AllColumns;
            fn from_clause(&self) -> Self::FromClause {
                diesel::internal::table_macro::StaticQueryFragmentInstance::new()
            }
            fn default_selection(&self) -> Self::DefaultSelection {
                use diesel::Table;
                Self::all_columns()
            }
        }
        impl<DB> diesel::query_builder::QueryFragment<DB> for table
        where
            DB: diesel::backend::Backend,
            <table as diesel::internal::table_macro::StaticQueryFragment>::Component: diesel::query_builder::QueryFragment<
                DB,
            >,
        {
            fn walk_ast<'b>(
                &'b self,
                __diesel_internal_pass: diesel::query_builder::AstPass<'_, 'b, DB>,
            ) -> diesel::result::QueryResult<()> {
                <table as diesel::internal::table_macro::StaticQueryFragment>::STATIC_COMPONENT
                    .walk_ast(__diesel_internal_pass)
            }
        }
        impl diesel::internal::table_macro::StaticQueryFragment for table {
            type Component = diesel::internal::table_macro::Identifier<'static>;
            const STATIC_COMPONENT: &'static Self::Component = &diesel::internal::table_macro::Identifier(
                "testdb",
            );
        }
        impl diesel::query_builder::AsQuery for table {
            type SqlType = SqlType;
            type Query = diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<Self>,
            >;
            fn as_query(self) -> Self::Query {
                diesel::internal::table_macro::SelectStatement::simple(self)
            }
        }
        impl diesel::Table for table {
            type PrimaryKey = id;
            type AllColumns = (
                id,
                cur_sym,
                token_name,
                tx_hash,
                pub_key_hash,
                script_hash,
                datum_hash,
                slot,
                plutus_data,
                cred,
                chain_pointer,
                staking_cred,
                address,
                asset_quantity,
                value,
                tx_in,
                datum,
                tx_out,
                tx_in_info,
            );
            fn primary_key(&self) -> Self::PrimaryKey {
                id
            }
            fn all_columns() -> Self::AllColumns {
                (
                    id,
                    cur_sym,
                    token_name,
                    tx_hash,
                    pub_key_hash,
                    script_hash,
                    datum_hash,
                    slot,
                    plutus_data,
                    cred,
                    chain_pointer,
                    staking_cred,
                    address,
                    asset_quantity,
                    value,
                    tx_in,
                    datum,
                    tx_out,
                    tx_in_info,
                )
            }
        }
        impl diesel::associations::HasTable for table {
            type Table = Self;
            fn table() -> Self::Table {
                table
            }
        }
        impl diesel::query_builder::IntoUpdateTarget for table {
            type WhereClause = <<Self as diesel::query_builder::AsQuery>::Query as diesel::query_builder::IntoUpdateTarget>::WhereClause;
            fn into_update_target(
                self,
            ) -> diesel::query_builder::UpdateTarget<Self::Table, Self::WhereClause> {
                use diesel::query_builder::AsQuery;
                let q: diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<table>,
                > = self.as_query();
                q.into_update_target()
            }
        }
        impl diesel::query_source::AppearsInFromClause<table> for table {
            type Count = diesel::query_source::Once;
        }
        impl<S> diesel::internal::table_macro::AliasAppearsInFromClause<S, table>
        for table
        where
            S: diesel::query_source::AliasSource<Target = table>,
        {
            type Count = diesel::query_source::Never;
        }
        impl<
            S1,
            S2,
        > diesel::internal::table_macro::AliasAliasAppearsInFromClause<table, S2, S1>
        for table
        where
            S1: diesel::query_source::AliasSource<Target = table>,
            S2: diesel::query_source::AliasSource<Target = table>,
            S1: diesel::internal::table_macro::AliasAliasAppearsInFromClauseSameTable<
                S2,
                table,
            >,
        {
            type Count = <S1 as diesel::internal::table_macro::AliasAliasAppearsInFromClauseSameTable<
                S2,
                table,
            >>::Count;
        }
        impl<S> diesel::query_source::AppearsInFromClause<diesel::query_source::Alias<S>>
        for table
        where
            S: diesel::query_source::AliasSource,
        {
            type Count = diesel::query_source::Never;
        }
        impl<
            S,
            C,
        > diesel::internal::table_macro::FieldAliasMapperAssociatedTypesDisjointnessTrick<
            table,
            S,
            C,
        > for table
        where
            S: diesel::query_source::AliasSource<Target = table> + ::std::clone::Clone,
            C: diesel::query_source::Column<Table = table>,
        {
            type Out = diesel::query_source::AliasedField<S, C>;
            fn map(
                __diesel_internal_column: C,
                __diesel_internal_alias: &diesel::query_source::Alias<S>,
            ) -> Self::Out {
                __diesel_internal_alias.field(__diesel_internal_column)
            }
        }
        impl diesel::query_source::AppearsInFromClause<table>
        for diesel::internal::table_macro::NoFromClause {
            type Count = diesel::query_source::Never;
        }
        impl<
            Left,
            Right,
            Kind,
        > diesel::JoinTo<diesel::internal::table_macro::Join<Left, Right, Kind>>
        for table
        where
            diesel::internal::table_macro::Join<
                Left,
                Right,
                Kind,
            >: diesel::JoinTo<table>,
            Left: diesel::query_source::QuerySource,
            Right: diesel::query_source::QuerySource,
        {
            type FromClause = diesel::internal::table_macro::Join<Left, Right, Kind>;
            type OnClause = <diesel::internal::table_macro::Join<
                Left,
                Right,
                Kind,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    Kind,
                >,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::Join::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<Join, On> diesel::JoinTo<diesel::internal::table_macro::JoinOn<Join, On>>
        for table
        where
            diesel::internal::table_macro::JoinOn<Join, On>: diesel::JoinTo<table>,
        {
            type FromClause = diesel::internal::table_macro::JoinOn<Join, On>;
            type OnClause = <diesel::internal::table_macro::JoinOn<
                Join,
                On,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::JoinOn<Join, On>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::JoinOn::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<
            F,
            S,
            D,
            W,
            O,
            L,
            Of,
            G,
        > diesel::JoinTo<
            diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            >,
        > for table
        where
            diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            >: diesel::JoinTo<table>,
            F: diesel::query_source::QuerySource,
        {
            type FromClause = diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            >;
            type OnClause = <diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<F>,
                    S,
                    D,
                    W,
                    O,
                    L,
                    Of,
                    G,
                >,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::SelectStatement::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<
            'a,
            QS,
            ST,
            DB,
        > diesel::JoinTo<
            diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            >,
        > for table
        where
            diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            >: diesel::JoinTo<table>,
            QS: diesel::query_source::QuerySource,
        {
            type FromClause = diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            >;
            type OnClause = <diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::BoxedSelectStatement<
                    'a,
                    diesel::internal::table_macro::FromClause<QS>,
                    ST,
                    DB,
                >,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::BoxedSelectStatement::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<S> diesel::JoinTo<diesel::query_source::Alias<S>> for table
        where
            diesel::query_source::Alias<S>: diesel::JoinTo<table>,
        {
            type FromClause = diesel::query_source::Alias<S>;
            type OnClause = <diesel::query_source::Alias<
                S,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::query_source::Alias<S>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::query_source::Alias::<
                    S,
                >::join_target(table);
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<T> diesel::insertable::Insertable<T> for table
        where
            <table as diesel::query_builder::AsQuery>::Query: diesel::insertable::Insertable<
                T,
            >,
        {
            type Values = <<table as diesel::query_builder::AsQuery>::Query as diesel::insertable::Insertable<
                T,
            >>::Values;
            fn values(self) -> Self::Values {
                use diesel::query_builder::AsQuery;
                self.as_query().values()
            }
        }
        impl<'a, T> diesel::insertable::Insertable<T> for &'a table
        where
            table: diesel::insertable::Insertable<T>,
        {
            type Values = <table as diesel::insertable::Insertable<T>>::Values;
            fn values(self) -> Self::Values {
                (*self).values()
            }
        }
        impl<S> diesel::JoinTo<diesel::query_builder::Only<S>> for table
        where
            diesel::query_builder::Only<S>: diesel::JoinTo<table>,
        {
            type FromClause = diesel::query_builder::Only<S>;
            type OnClause = <diesel::query_builder::Only<
                S,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::query_builder::Only<S>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::query_builder::Only::<
                    S,
                >::join_target(table);
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl diesel::query_source::AppearsInFromClause<
            diesel::query_builder::Only<table>,
        > for table {
            type Count = diesel::query_source::Once;
        }
        impl diesel::query_source::AppearsInFromClause<table>
        for diesel::query_builder::Only<table> {
            type Count = diesel::query_source::Once;
        }
        impl<S, TSM> diesel::JoinTo<diesel::query_builder::Tablesample<S, TSM>> for table
        where
            diesel::query_builder::Tablesample<S, TSM>: diesel::JoinTo<table>,
            TSM: diesel::internal::table_macro::TablesampleMethod,
        {
            type FromClause = diesel::query_builder::Tablesample<S, TSM>;
            type OnClause = <diesel::query_builder::Tablesample<
                S,
                TSM,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::query_builder::Tablesample<S, TSM>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::query_builder::Tablesample::<
                    S,
                    TSM,
                >::join_target(table);
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<
            TSM,
        > diesel::query_source::AppearsInFromClause<
            diesel::query_builder::Tablesample<table, TSM>,
        > for table
        where
            TSM: diesel::internal::table_macro::TablesampleMethod,
        {
            type Count = diesel::query_source::Once;
        }
        impl<TSM> diesel::query_source::AppearsInFromClause<table>
        for diesel::query_builder::Tablesample<table, TSM>
        where
            TSM: diesel::internal::table_macro::TablesampleMethod,
        {
            type Count = diesel::query_source::Once;
        }
        /// Contains all of the columns of this table
        pub mod columns {
            use ::diesel;
            use super::table;
            use crate::database::plutus::sql_types::*;
            use diesel::sql_types::*;
            #[allow(non_camel_case_types, dead_code)]
            /// Represents `table_name.*`, which is sometimes needed for
            /// efficient count queries. It cannot be used in place of
            /// `all_columns`, and has a `SqlType` of `()` to prevent it
            /// being used that way
            pub struct star;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for star {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "star")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for star {
                #[inline]
                fn clone(&self) -> star {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for star {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for star {
                    type QueryId = star;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            impl<__GB> diesel::expression::ValidGrouping<__GB> for star
            where
                (
                    id,
                    cur_sym,
                    token_name,
                    tx_hash,
                    pub_key_hash,
                    script_hash,
                    datum_hash,
                    slot,
                    plutus_data,
                    cred,
                    chain_pointer,
                    staking_cred,
                    address,
                    asset_quantity,
                    value,
                    tx_in,
                    datum,
                    tx_out,
                    tx_in_info,
                ): diesel::expression::ValidGrouping<__GB>,
            {
                type IsAggregate = <(
                    id,
                    cur_sym,
                    token_name,
                    tx_hash,
                    pub_key_hash,
                    script_hash,
                    datum_hash,
                    slot,
                    plutus_data,
                    cred,
                    chain_pointer,
                    staking_cred,
                    address,
                    asset_quantity,
                    value,
                    tx_in,
                    datum,
                    tx_out,
                    tx_in_info,
                ) as diesel::expression::ValidGrouping<__GB>>::IsAggregate;
            }
            impl diesel::Expression for star {
                type SqlType = diesel::expression::expression_types::NotSelectable;
            }
            impl<DB: diesel::backend::Backend> diesel::query_builder::QueryFragment<DB>
            for star
            where
                <table as diesel::QuerySource>::FromClause: diesel::query_builder::QueryFragment<
                    DB,
                >,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    use diesel::QuerySource;
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_sql("*");
                    Ok(())
                }
            }
            impl diesel::SelectableExpression<table> for star {}
            impl diesel::AppearsOnTable<table> for star {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct id;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for id {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "id")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for id {
                #[inline]
                fn clone(&self) -> id {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for id {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for id {
                    type QueryId = id;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for id {
                #[inline]
                fn default() -> id {
                    id {}
                }
            }
            impl diesel::expression::Expression for id {
                type SqlType = Int8;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for id
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("id")
                }
            }
            impl diesel::SelectableExpression<super::table> for id {}
            impl<QS> diesel::AppearsOnTable<QS> for id
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for id
            where
                id: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for id
            where
                id: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for id
            where
                id: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for id
            where
                From: diesel::query_source::QuerySource,
                id: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for id
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    id,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for id {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for id {
                type Table = super::table;
                const NAME: &'static str = "id";
            }
            impl<T> diesel::EqAll<T> for id
            where
                T: diesel::expression::AsExpression<Int8>,
                diesel::dsl::Eq<
                    id,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl<Rhs> ::std::ops::Add<Rhs> for id
            where
                Rhs: diesel::expression::AsExpression<
                    <<id as diesel::Expression>::SqlType as diesel::sql_types::ops::Add>::Rhs,
                >,
            {
                type Output = diesel::internal::table_macro::ops::Add<
                    Self,
                    Rhs::Expression,
                >;
                fn add(self, __diesel_internal_rhs: Rhs) -> Self::Output {
                    diesel::internal::table_macro::ops::Add::new(
                        self,
                        __diesel_internal_rhs.as_expression(),
                    )
                }
            }
            impl<Rhs> ::std::ops::Sub<Rhs> for id
            where
                Rhs: diesel::expression::AsExpression<
                    <<id as diesel::Expression>::SqlType as diesel::sql_types::ops::Sub>::Rhs,
                >,
            {
                type Output = diesel::internal::table_macro::ops::Sub<
                    Self,
                    Rhs::Expression,
                >;
                fn sub(self, __diesel_internal_rhs: Rhs) -> Self::Output {
                    diesel::internal::table_macro::ops::Sub::new(
                        self,
                        __diesel_internal_rhs.as_expression(),
                    )
                }
            }
            impl<Rhs> ::std::ops::Div<Rhs> for id
            where
                Rhs: diesel::expression::AsExpression<
                    <<id as diesel::Expression>::SqlType as diesel::sql_types::ops::Div>::Rhs,
                >,
            {
                type Output = diesel::internal::table_macro::ops::Div<
                    Self,
                    Rhs::Expression,
                >;
                fn div(self, __diesel_internal_rhs: Rhs) -> Self::Output {
                    diesel::internal::table_macro::ops::Div::new(
                        self,
                        __diesel_internal_rhs.as_expression(),
                    )
                }
            }
            impl<Rhs> ::std::ops::Mul<Rhs> for id
            where
                Rhs: diesel::expression::AsExpression<
                    <<id as diesel::Expression>::SqlType as diesel::sql_types::ops::Mul>::Rhs,
                >,
            {
                type Output = diesel::internal::table_macro::ops::Mul<
                    Self,
                    Rhs::Expression,
                >;
                fn mul(self, __diesel_internal_rhs: Rhs) -> Self::Output {
                    diesel::internal::table_macro::ops::Mul::new(
                        self,
                        __diesel_internal_rhs.as_expression(),
                    )
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for id {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for id {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for id
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for id
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct cur_sym;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for cur_sym {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "cur_sym")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for cur_sym {
                #[inline]
                fn clone(&self) -> cur_sym {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for cur_sym {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for cur_sym {
                    type QueryId = cur_sym;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for cur_sym {
                #[inline]
                fn default() -> cur_sym {
                    cur_sym {}
                }
            }
            impl diesel::expression::Expression for cur_sym {
                type SqlType = Nullable<CurrencySymbol>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for cur_sym
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("cur_sym")
                }
            }
            impl diesel::SelectableExpression<super::table> for cur_sym {}
            impl<QS> diesel::AppearsOnTable<QS> for cur_sym
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for cur_sym
            where
                cur_sym: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for cur_sym
            where
                cur_sym: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for cur_sym
            where
                cur_sym: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for cur_sym
            where
                From: diesel::query_source::QuerySource,
                cur_sym: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for cur_sym
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    cur_sym,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for cur_sym {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for cur_sym {
                type Table = super::table;
                const NAME: &'static str = "cur_sym";
            }
            impl<T> diesel::EqAll<T> for cur_sym
            where
                T: diesel::expression::AsExpression<Nullable<CurrencySymbol>>,
                diesel::dsl::Eq<
                    cur_sym,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for cur_sym {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for cur_sym {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for cur_sym
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for cur_sym
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct token_name;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for token_name {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "token_name")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for token_name {
                #[inline]
                fn clone(&self) -> token_name {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for token_name {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for token_name {
                    type QueryId = token_name;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for token_name {
                #[inline]
                fn default() -> token_name {
                    token_name {}
                }
            }
            impl diesel::expression::Expression for token_name {
                type SqlType = Nullable<TokenName>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for token_name
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("token_name")
                }
            }
            impl diesel::SelectableExpression<super::table> for token_name {}
            impl<QS> diesel::AppearsOnTable<QS> for token_name
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for token_name
            where
                token_name: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for token_name
            where
                token_name: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for token_name
            where
                token_name: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for token_name
            where
                From: diesel::query_source::QuerySource,
                token_name: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for token_name
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    token_name,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for token_name {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for token_name {
                type Table = super::table;
                const NAME: &'static str = "token_name";
            }
            impl<T> diesel::EqAll<T> for token_name
            where
                T: diesel::expression::AsExpression<Nullable<TokenName>>,
                diesel::dsl::Eq<
                    token_name,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for token_name {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for token_name {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for token_name
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for token_name
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct tx_hash;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for tx_hash {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "tx_hash")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for tx_hash {
                #[inline]
                fn clone(&self) -> tx_hash {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for tx_hash {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for tx_hash {
                    type QueryId = tx_hash;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for tx_hash {
                #[inline]
                fn default() -> tx_hash {
                    tx_hash {}
                }
            }
            impl diesel::expression::Expression for tx_hash {
                type SqlType = Nullable<TransactionHash>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for tx_hash
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("tx_hash")
                }
            }
            impl diesel::SelectableExpression<super::table> for tx_hash {}
            impl<QS> diesel::AppearsOnTable<QS> for tx_hash
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for tx_hash
            where
                tx_hash: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for tx_hash
            where
                tx_hash: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for tx_hash
            where
                tx_hash: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for tx_hash
            where
                From: diesel::query_source::QuerySource,
                tx_hash: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for tx_hash
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    tx_hash,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for tx_hash {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for tx_hash {
                type Table = super::table;
                const NAME: &'static str = "tx_hash";
            }
            impl<T> diesel::EqAll<T> for tx_hash
            where
                T: diesel::expression::AsExpression<Nullable<TransactionHash>>,
                diesel::dsl::Eq<
                    tx_hash,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for tx_hash {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for tx_hash {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for tx_hash
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for tx_hash
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct pub_key_hash;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for pub_key_hash {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "pub_key_hash")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for pub_key_hash {
                #[inline]
                fn clone(&self) -> pub_key_hash {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for pub_key_hash {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for pub_key_hash {
                    type QueryId = pub_key_hash;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for pub_key_hash {
                #[inline]
                fn default() -> pub_key_hash {
                    pub_key_hash {}
                }
            }
            impl diesel::expression::Expression for pub_key_hash {
                type SqlType = Nullable<Ed25519PubKeyHash>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for pub_key_hash
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("pub_key_hash")
                }
            }
            impl diesel::SelectableExpression<super::table> for pub_key_hash {}
            impl<QS> diesel::AppearsOnTable<QS> for pub_key_hash
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for pub_key_hash
            where
                pub_key_hash: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for pub_key_hash
            where
                pub_key_hash: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for pub_key_hash
            where
                pub_key_hash: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for pub_key_hash
            where
                From: diesel::query_source::QuerySource,
                pub_key_hash: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for pub_key_hash
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    pub_key_hash,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for pub_key_hash {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash>
            for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for pub_key_hash {
                type Table = super::table;
                const NAME: &'static str = "pub_key_hash";
            }
            impl<T> diesel::EqAll<T> for pub_key_hash
            where
                T: diesel::expression::AsExpression<Nullable<Ed25519PubKeyHash>>,
                diesel::dsl::Eq<
                    pub_key_hash,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for pub_key_hash {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for pub_key_hash {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for pub_key_hash
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for pub_key_hash
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct script_hash;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for script_hash {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "script_hash")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for script_hash {
                #[inline]
                fn clone(&self) -> script_hash {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for script_hash {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for script_hash {
                    type QueryId = script_hash;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for script_hash {
                #[inline]
                fn default() -> script_hash {
                    script_hash {}
                }
            }
            impl diesel::expression::Expression for script_hash {
                type SqlType = Nullable<ScriptHash>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for script_hash
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("script_hash")
                }
            }
            impl diesel::SelectableExpression<super::table> for script_hash {}
            impl<QS> diesel::AppearsOnTable<QS> for script_hash
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for script_hash
            where
                script_hash: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for script_hash
            where
                script_hash: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for script_hash
            where
                script_hash: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for script_hash
            where
                From: diesel::query_source::QuerySource,
                script_hash: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for script_hash
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    script_hash,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for script_hash {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for script_hash {
                type Table = super::table;
                const NAME: &'static str = "script_hash";
            }
            impl<T> diesel::EqAll<T> for script_hash
            where
                T: diesel::expression::AsExpression<Nullable<ScriptHash>>,
                diesel::dsl::Eq<
                    script_hash,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for script_hash {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for script_hash {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for script_hash
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for script_hash
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct datum_hash;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for datum_hash {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "datum_hash")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for datum_hash {
                #[inline]
                fn clone(&self) -> datum_hash {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for datum_hash {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for datum_hash {
                    type QueryId = datum_hash;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for datum_hash {
                #[inline]
                fn default() -> datum_hash {
                    datum_hash {}
                }
            }
            impl diesel::expression::Expression for datum_hash {
                type SqlType = Nullable<DatumHash>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for datum_hash
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("datum_hash")
                }
            }
            impl diesel::SelectableExpression<super::table> for datum_hash {}
            impl<QS> diesel::AppearsOnTable<QS> for datum_hash
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for datum_hash
            where
                datum_hash: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for datum_hash
            where
                datum_hash: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for datum_hash
            where
                datum_hash: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for datum_hash
            where
                From: diesel::query_source::QuerySource,
                datum_hash: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for datum_hash
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    datum_hash,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for datum_hash {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for datum_hash {
                type Table = super::table;
                const NAME: &'static str = "datum_hash";
            }
            impl<T> diesel::EqAll<T> for datum_hash
            where
                T: diesel::expression::AsExpression<Nullable<DatumHash>>,
                diesel::dsl::Eq<
                    datum_hash,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for datum_hash {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for datum_hash {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for datum_hash
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for datum_hash
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct slot;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for slot {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "slot")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for slot {
                #[inline]
                fn clone(&self) -> slot {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for slot {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for slot {
                    type QueryId = slot;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for slot {
                #[inline]
                fn default() -> slot {
                    slot {}
                }
            }
            impl diesel::expression::Expression for slot {
                type SqlType = Nullable<Slot>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for slot
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("slot")
                }
            }
            impl diesel::SelectableExpression<super::table> for slot {}
            impl<QS> diesel::AppearsOnTable<QS> for slot
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for slot
            where
                slot: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for slot
            where
                slot: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for slot
            where
                slot: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for slot
            where
                From: diesel::query_source::QuerySource,
                slot: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for slot
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    slot,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for slot {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for slot {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for slot {
                type Table = super::table;
                const NAME: &'static str = "slot";
            }
            impl<T> diesel::EqAll<T> for slot
            where
                T: diesel::expression::AsExpression<Nullable<Slot>>,
                diesel::dsl::Eq<
                    slot,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for slot {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for slot {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for slot
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for slot
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct plutus_data;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for plutus_data {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "plutus_data")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for plutus_data {
                #[inline]
                fn clone(&self) -> plutus_data {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for plutus_data {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for plutus_data {
                    type QueryId = plutus_data;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for plutus_data {
                #[inline]
                fn default() -> plutus_data {
                    plutus_data {}
                }
            }
            impl diesel::expression::Expression for plutus_data {
                type SqlType = Nullable<PlutusData>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for plutus_data
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("plutus_data")
                }
            }
            impl diesel::SelectableExpression<super::table> for plutus_data {}
            impl<QS> diesel::AppearsOnTable<QS> for plutus_data
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for plutus_data
            where
                plutus_data: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for plutus_data
            where
                plutus_data: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for plutus_data
            where
                plutus_data: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for plutus_data
            where
                From: diesel::query_source::QuerySource,
                plutus_data: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for plutus_data
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    plutus_data,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for plutus_data {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for plutus_data {
                type Table = super::table;
                const NAME: &'static str = "plutus_data";
            }
            impl<T> diesel::EqAll<T> for plutus_data
            where
                T: diesel::expression::AsExpression<Nullable<PlutusData>>,
                diesel::dsl::Eq<
                    plutus_data,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for plutus_data {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for plutus_data {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for plutus_data
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for plutus_data
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct cred;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for cred {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "cred")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for cred {
                #[inline]
                fn clone(&self) -> cred {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for cred {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for cred {
                    type QueryId = cred;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for cred {
                #[inline]
                fn default() -> cred {
                    cred {}
                }
            }
            impl diesel::expression::Expression for cred {
                type SqlType = Nullable<Credential>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for cred
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("cred")
                }
            }
            impl diesel::SelectableExpression<super::table> for cred {}
            impl<QS> diesel::AppearsOnTable<QS> for cred
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for cred
            where
                cred: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for cred
            where
                cred: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for cred
            where
                cred: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for cred
            where
                From: diesel::query_source::QuerySource,
                cred: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for cred
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    cred,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for cred {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for cred {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for cred {
                type Table = super::table;
                const NAME: &'static str = "cred";
            }
            impl<T> diesel::EqAll<T> for cred
            where
                T: diesel::expression::AsExpression<Nullable<Credential>>,
                diesel::dsl::Eq<
                    cred,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for cred {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for cred {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for cred
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for cred
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct chain_pointer;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for chain_pointer {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "chain_pointer")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for chain_pointer {
                #[inline]
                fn clone(&self) -> chain_pointer {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for chain_pointer {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for chain_pointer {
                    type QueryId = chain_pointer;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for chain_pointer {
                #[inline]
                fn default() -> chain_pointer {
                    chain_pointer {}
                }
            }
            impl diesel::expression::Expression for chain_pointer {
                type SqlType = Nullable<ChainPointer>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for chain_pointer
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("chain_pointer")
                }
            }
            impl diesel::SelectableExpression<super::table> for chain_pointer {}
            impl<QS> diesel::AppearsOnTable<QS> for chain_pointer
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for chain_pointer
            where
                chain_pointer: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for chain_pointer
            where
                chain_pointer: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for chain_pointer
            where
                chain_pointer: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for chain_pointer
            where
                From: diesel::query_source::QuerySource,
                chain_pointer: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for chain_pointer
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    chain_pointer,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for chain_pointer {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer>
            for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for chain_pointer {
                type Table = super::table;
                const NAME: &'static str = "chain_pointer";
            }
            impl<T> diesel::EqAll<T> for chain_pointer
            where
                T: diesel::expression::AsExpression<Nullable<ChainPointer>>,
                diesel::dsl::Eq<
                    chain_pointer,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for chain_pointer {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for chain_pointer {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for chain_pointer
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for chain_pointer
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct staking_cred;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for staking_cred {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "staking_cred")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for staking_cred {
                #[inline]
                fn clone(&self) -> staking_cred {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for staking_cred {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for staking_cred {
                    type QueryId = staking_cred;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for staking_cred {
                #[inline]
                fn default() -> staking_cred {
                    staking_cred {}
                }
            }
            impl diesel::expression::Expression for staking_cred {
                type SqlType = Nullable<StakingCredential>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for staking_cred
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("staking_cred")
                }
            }
            impl diesel::SelectableExpression<super::table> for staking_cred {}
            impl<QS> diesel::AppearsOnTable<QS> for staking_cred
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for staking_cred
            where
                staking_cred: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for staking_cred
            where
                staking_cred: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for staking_cred
            where
                staking_cred: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for staking_cred
            where
                From: diesel::query_source::QuerySource,
                staking_cred: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for staking_cred
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    staking_cred,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for staking_cred {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred>
            for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for staking_cred {
                type Table = super::table;
                const NAME: &'static str = "staking_cred";
            }
            impl<T> diesel::EqAll<T> for staking_cred
            where
                T: diesel::expression::AsExpression<Nullable<StakingCredential>>,
                diesel::dsl::Eq<
                    staking_cred,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for staking_cred {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for staking_cred {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for staking_cred
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for staking_cred
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct address;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for address {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "address")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for address {
                #[inline]
                fn clone(&self) -> address {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for address {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for address {
                    type QueryId = address;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for address {
                #[inline]
                fn default() -> address {
                    address {}
                }
            }
            impl diesel::expression::Expression for address {
                type SqlType = Nullable<Address>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for address
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("address")
                }
            }
            impl diesel::SelectableExpression<super::table> for address {}
            impl<QS> diesel::AppearsOnTable<QS> for address
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for address
            where
                address: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for address
            where
                address: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for address
            where
                address: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for address
            where
                From: diesel::query_source::QuerySource,
                address: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for address
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    address,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for address {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for address {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for address {
                type Table = super::table;
                const NAME: &'static str = "address";
            }
            impl<T> diesel::EqAll<T> for address
            where
                T: diesel::expression::AsExpression<Nullable<Address>>,
                diesel::dsl::Eq<
                    address,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for address {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for address {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for address
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for address
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct asset_quantity;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for asset_quantity {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "asset_quantity")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for asset_quantity {
                #[inline]
                fn clone(&self) -> asset_quantity {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for asset_quantity {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for asset_quantity {
                    type QueryId = asset_quantity;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for asset_quantity {
                #[inline]
                fn default() -> asset_quantity {
                    asset_quantity {}
                }
            }
            impl diesel::expression::Expression for asset_quantity {
                type SqlType = Nullable<AssetQuantity>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for asset_quantity
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("asset_quantity")
                }
            }
            impl diesel::SelectableExpression<super::table> for asset_quantity {}
            impl<QS> diesel::AppearsOnTable<QS> for asset_quantity
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for asset_quantity
            where
                asset_quantity: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for asset_quantity
            where
                asset_quantity: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for asset_quantity
            where
                asset_quantity: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for asset_quantity
            where
                From: diesel::query_source::QuerySource,
                asset_quantity: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for asset_quantity
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    asset_quantity,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for asset_quantity {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity>
            for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for asset_quantity {
                type Table = super::table;
                const NAME: &'static str = "asset_quantity";
            }
            impl<T> diesel::EqAll<T> for asset_quantity
            where
                T: diesel::expression::AsExpression<Nullable<AssetQuantity>>,
                diesel::dsl::Eq<
                    asset_quantity,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for asset_quantity {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for asset_quantity {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for asset_quantity
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for asset_quantity
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct value;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for value {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "value")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for value {
                #[inline]
                fn clone(&self) -> value {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for value {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for value {
                    type QueryId = value;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for value {
                #[inline]
                fn default() -> value {
                    value {}
                }
            }
            impl diesel::expression::Expression for value {
                type SqlType = Nullable<Value>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for value
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("value")
                }
            }
            impl diesel::SelectableExpression<super::table> for value {}
            impl<QS> diesel::AppearsOnTable<QS> for value
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for value
            where
                value: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for value
            where
                value: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for value
            where
                value: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for value
            where
                From: diesel::query_source::QuerySource,
                value: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for value
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    value,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for value {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for value {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for value {
                type Table = super::table;
                const NAME: &'static str = "value";
            }
            impl<T> diesel::EqAll<T> for value
            where
                T: diesel::expression::AsExpression<Nullable<Value>>,
                diesel::dsl::Eq<
                    value,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for value {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for value {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for value
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for value
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct tx_in;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for tx_in {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "tx_in")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for tx_in {
                #[inline]
                fn clone(&self) -> tx_in {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for tx_in {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for tx_in {
                    type QueryId = tx_in;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for tx_in {
                #[inline]
                fn default() -> tx_in {
                    tx_in {}
                }
            }
            impl diesel::expression::Expression for tx_in {
                type SqlType = Nullable<TransactionInput>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for tx_in
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("tx_in")
                }
            }
            impl diesel::SelectableExpression<super::table> for tx_in {}
            impl<QS> diesel::AppearsOnTable<QS> for tx_in
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for tx_in
            where
                tx_in: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for tx_in
            where
                tx_in: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for tx_in
            where
                tx_in: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for tx_in
            where
                From: diesel::query_source::QuerySource,
                tx_in: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for tx_in
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    tx_in,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for tx_in {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for tx_in {
                type Table = super::table;
                const NAME: &'static str = "tx_in";
            }
            impl<T> diesel::EqAll<T> for tx_in
            where
                T: diesel::expression::AsExpression<Nullable<TransactionInput>>,
                diesel::dsl::Eq<
                    tx_in,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for tx_in {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for tx_in {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for tx_in
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for tx_in
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct datum;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for datum {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "datum")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for datum {
                #[inline]
                fn clone(&self) -> datum {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for datum {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for datum {
                    type QueryId = datum;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for datum {
                #[inline]
                fn default() -> datum {
                    datum {}
                }
            }
            impl diesel::expression::Expression for datum {
                type SqlType = Nullable<OutputDatum>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for datum
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("datum")
                }
            }
            impl diesel::SelectableExpression<super::table> for datum {}
            impl<QS> diesel::AppearsOnTable<QS> for datum
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for datum
            where
                datum: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for datum
            where
                datum: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for datum
            where
                datum: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for datum
            where
                From: diesel::query_source::QuerySource,
                datum: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for datum
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    datum,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for datum {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for datum {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for datum {
                type Table = super::table;
                const NAME: &'static str = "datum";
            }
            impl<T> diesel::EqAll<T> for datum
            where
                T: diesel::expression::AsExpression<Nullable<OutputDatum>>,
                diesel::dsl::Eq<
                    datum,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for datum {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for datum {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for datum
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for datum
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct tx_out;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for tx_out {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "tx_out")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for tx_out {
                #[inline]
                fn clone(&self) -> tx_out {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for tx_out {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for tx_out {
                    type QueryId = tx_out;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for tx_out {
                #[inline]
                fn default() -> tx_out {
                    tx_out {}
                }
            }
            impl diesel::expression::Expression for tx_out {
                type SqlType = Nullable<TransactionOutput>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for tx_out
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("tx_out")
                }
            }
            impl diesel::SelectableExpression<super::table> for tx_out {}
            impl<QS> diesel::AppearsOnTable<QS> for tx_out
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for tx_out
            where
                tx_out: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for tx_out
            where
                tx_out: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for tx_out
            where
                tx_out: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for tx_out
            where
                From: diesel::query_source::QuerySource,
                tx_out: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for tx_out
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    tx_out,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for tx_out {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for tx_out {
                type Table = super::table;
                const NAME: &'static str = "tx_out";
            }
            impl<T> diesel::EqAll<T> for tx_out
            where
                T: diesel::expression::AsExpression<Nullable<TransactionOutput>>,
                diesel::dsl::Eq<
                    tx_out,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for tx_out {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for tx_out {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for tx_out
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for tx_out
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct tx_in_info;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for tx_in_info {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "tx_in_info")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for tx_in_info {
                #[inline]
                fn clone(&self) -> tx_in_info {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for tx_in_info {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for tx_in_info {
                    type QueryId = tx_in_info;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for tx_in_info {
                #[inline]
                fn default() -> tx_in_info {
                    tx_in_info {}
                }
            }
            impl diesel::expression::Expression for tx_in_info {
                type SqlType = Nullable<TxInInfo>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for tx_in_info
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("tx_in_info")
                }
            }
            impl diesel::SelectableExpression<super::table> for tx_in_info {}
            impl<QS> diesel::AppearsOnTable<QS> for tx_in_info
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for tx_in_info
            where
                tx_in_info: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for tx_in_info
            where
                tx_in_info: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for tx_in_info
            where
                tx_in_info: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for tx_in_info
            where
                From: diesel::query_source::QuerySource,
                tx_in_info: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for tx_in_info
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    tx_in_info,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for tx_in_info {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for tx_in_info {
                type Table = super::table;
                const NAME: &'static str = "tx_in_info";
            }
            impl<T> diesel::EqAll<T> for tx_in_info
            where
                T: diesel::expression::AsExpression<Nullable<TxInInfo>>,
                diesel::dsl::Eq<
                    tx_in_info,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for tx_in_info {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for tx_in_info {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for tx_in_info
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for tx_in_info
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            impl diesel::expression::IsContainedInGroupBy<id> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<id> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for id {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cur_sym> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for cur_sym {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name>
            for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity>
            for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<token_name> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for token_name {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_hash> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for tx_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash>
            for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer>
            for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash>
            for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred>
            for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash>
            for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity>
            for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<pub_key_hash> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for pub_key_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash>
            for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer>
            for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash>
            for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity>
            for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<script_hash> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for script_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash>
            for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity>
            for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum_hash> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for datum_hash {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<slot> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for slot {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data>
            for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer>
            for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data>
            for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity>
            for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<plutus_data> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for plutus_data {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<cred> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer>
            for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred>
            for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer>
            for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity>
            for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<chain_pointer> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for chain_pointer {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred>
            for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity>
            for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<staking_cred> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for staking_cred {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<asset_quantity>
            for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info>
            for asset_quantity {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for tx_in {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_out> for tx_in_info {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<tx_in_info> for tx_out {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
        }
    }
    #[allow(unused_imports, dead_code, unreachable_pub, unused_qualifications)]
    pub mod utxos {
        use ::diesel;
        pub use self::columns::*;
        use crate::database::plutus::sql_types::*;
        use diesel::sql_types::*;
        /// Re-exports all of the columns of this table, as well as the
        /// table struct renamed to the module name. This is meant to be
        /// glob imported for functions which only deal with one table.
        pub mod dsl {
            pub use super::columns::utxo_ref;
            pub use super::columns::value;
            pub use super::columns::address;
            pub use super::columns::datum;
            pub use super::columns::created_at;
            pub use super::columns::deleted_at;
            pub use super::table as utxos;
        }
        #[allow(non_upper_case_globals, dead_code)]
        /// A tuple of all of the columns on this table
        pub const all_columns: (
            utxo_ref,
            value,
            address,
            datum,
            created_at,
            deleted_at,
        ) = (utxo_ref, value, address, datum, created_at, deleted_at);
        #[allow(non_camel_case_types)]
        /// The actual table struct
        ///
        /// This is the type which provides the base methods of the query
        /// builder, such as `.select` and `.filter`.
        pub struct table;
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::fmt::Debug for table {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::write_str(f, "table")
            }
        }
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::clone::Clone for table {
            #[inline]
            fn clone(&self) -> table {
                *self
            }
        }
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::marker::Copy for table {}
        #[allow(unused_imports)]
        const _: () = {
            use diesel;
            use diesel::query_builder::QueryId;
            #[allow(non_camel_case_types)]
            impl QueryId for table {
                type QueryId = table;
                const HAS_STATIC_QUERY_ID: bool = true;
            }
        };
        #[automatically_derived]
        #[allow(non_camel_case_types)]
        impl ::core::default::Default for table {
            #[inline]
            fn default() -> table {
                table {}
            }
        }
        impl table {
            #[allow(dead_code)]
            /// Represents `table_name.*`, which is sometimes necessary
            /// for efficient count queries. It cannot be used in place of
            /// `all_columns`
            pub fn star(&self) -> star {
                star
            }
        }
        /// The SQL type of all of the columns on this table
        pub type SqlType = (
            TransactionInput,
            Value,
            Address,
            OutputDatum,
            Slot,
            Nullable<Slot>,
        );
        /// Helper type for representing a boxed query from this table
        pub type BoxedQuery<'a, DB, ST = SqlType> = diesel::internal::table_macro::BoxedSelectStatement<
            'a,
            ST,
            diesel::internal::table_macro::FromClause<table>,
            DB,
        >;
        impl diesel::QuerySource for table {
            type FromClause = diesel::internal::table_macro::StaticQueryFragmentInstance<
                table,
            >;
            type DefaultSelection = <Self as diesel::Table>::AllColumns;
            fn from_clause(&self) -> Self::FromClause {
                diesel::internal::table_macro::StaticQueryFragmentInstance::new()
            }
            fn default_selection(&self) -> Self::DefaultSelection {
                use diesel::Table;
                Self::all_columns()
            }
        }
        impl<DB> diesel::query_builder::QueryFragment<DB> for table
        where
            DB: diesel::backend::Backend,
            <table as diesel::internal::table_macro::StaticQueryFragment>::Component: diesel::query_builder::QueryFragment<
                DB,
            >,
        {
            fn walk_ast<'b>(
                &'b self,
                __diesel_internal_pass: diesel::query_builder::AstPass<'_, 'b, DB>,
            ) -> diesel::result::QueryResult<()> {
                <table as diesel::internal::table_macro::StaticQueryFragment>::STATIC_COMPONENT
                    .walk_ast(__diesel_internal_pass)
            }
        }
        impl diesel::internal::table_macro::StaticQueryFragment for table {
            type Component = diesel::internal::table_macro::Identifier<'static>;
            const STATIC_COMPONENT: &'static Self::Component = &diesel::internal::table_macro::Identifier(
                "utxos",
            );
        }
        impl diesel::query_builder::AsQuery for table {
            type SqlType = SqlType;
            type Query = diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<Self>,
            >;
            fn as_query(self) -> Self::Query {
                diesel::internal::table_macro::SelectStatement::simple(self)
            }
        }
        impl diesel::Table for table {
            type PrimaryKey = utxo_ref;
            type AllColumns = (utxo_ref, value, address, datum, created_at, deleted_at);
            fn primary_key(&self) -> Self::PrimaryKey {
                utxo_ref
            }
            fn all_columns() -> Self::AllColumns {
                (utxo_ref, value, address, datum, created_at, deleted_at)
            }
        }
        impl diesel::associations::HasTable for table {
            type Table = Self;
            fn table() -> Self::Table {
                table
            }
        }
        impl diesel::query_builder::IntoUpdateTarget for table {
            type WhereClause = <<Self as diesel::query_builder::AsQuery>::Query as diesel::query_builder::IntoUpdateTarget>::WhereClause;
            fn into_update_target(
                self,
            ) -> diesel::query_builder::UpdateTarget<Self::Table, Self::WhereClause> {
                use diesel::query_builder::AsQuery;
                let q: diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<table>,
                > = self.as_query();
                q.into_update_target()
            }
        }
        impl diesel::query_source::AppearsInFromClause<table> for table {
            type Count = diesel::query_source::Once;
        }
        impl<S> diesel::internal::table_macro::AliasAppearsInFromClause<S, table>
        for table
        where
            S: diesel::query_source::AliasSource<Target = table>,
        {
            type Count = diesel::query_source::Never;
        }
        impl<
            S1,
            S2,
        > diesel::internal::table_macro::AliasAliasAppearsInFromClause<table, S2, S1>
        for table
        where
            S1: diesel::query_source::AliasSource<Target = table>,
            S2: diesel::query_source::AliasSource<Target = table>,
            S1: diesel::internal::table_macro::AliasAliasAppearsInFromClauseSameTable<
                S2,
                table,
            >,
        {
            type Count = <S1 as diesel::internal::table_macro::AliasAliasAppearsInFromClauseSameTable<
                S2,
                table,
            >>::Count;
        }
        impl<S> diesel::query_source::AppearsInFromClause<diesel::query_source::Alias<S>>
        for table
        where
            S: diesel::query_source::AliasSource,
        {
            type Count = diesel::query_source::Never;
        }
        impl<
            S,
            C,
        > diesel::internal::table_macro::FieldAliasMapperAssociatedTypesDisjointnessTrick<
            table,
            S,
            C,
        > for table
        where
            S: diesel::query_source::AliasSource<Target = table> + ::std::clone::Clone,
            C: diesel::query_source::Column<Table = table>,
        {
            type Out = diesel::query_source::AliasedField<S, C>;
            fn map(
                __diesel_internal_column: C,
                __diesel_internal_alias: &diesel::query_source::Alias<S>,
            ) -> Self::Out {
                __diesel_internal_alias.field(__diesel_internal_column)
            }
        }
        impl diesel::query_source::AppearsInFromClause<table>
        for diesel::internal::table_macro::NoFromClause {
            type Count = diesel::query_source::Never;
        }
        impl<
            Left,
            Right,
            Kind,
        > diesel::JoinTo<diesel::internal::table_macro::Join<Left, Right, Kind>>
        for table
        where
            diesel::internal::table_macro::Join<
                Left,
                Right,
                Kind,
            >: diesel::JoinTo<table>,
            Left: diesel::query_source::QuerySource,
            Right: diesel::query_source::QuerySource,
        {
            type FromClause = diesel::internal::table_macro::Join<Left, Right, Kind>;
            type OnClause = <diesel::internal::table_macro::Join<
                Left,
                Right,
                Kind,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    Kind,
                >,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::Join::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<Join, On> diesel::JoinTo<diesel::internal::table_macro::JoinOn<Join, On>>
        for table
        where
            diesel::internal::table_macro::JoinOn<Join, On>: diesel::JoinTo<table>,
        {
            type FromClause = diesel::internal::table_macro::JoinOn<Join, On>;
            type OnClause = <diesel::internal::table_macro::JoinOn<
                Join,
                On,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::JoinOn<Join, On>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::JoinOn::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<
            F,
            S,
            D,
            W,
            O,
            L,
            Of,
            G,
        > diesel::JoinTo<
            diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            >,
        > for table
        where
            diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            >: diesel::JoinTo<table>,
            F: diesel::query_source::QuerySource,
        {
            type FromClause = diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            >;
            type OnClause = <diesel::internal::table_macro::SelectStatement<
                diesel::internal::table_macro::FromClause<F>,
                S,
                D,
                W,
                O,
                L,
                Of,
                G,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<F>,
                    S,
                    D,
                    W,
                    O,
                    L,
                    Of,
                    G,
                >,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::SelectStatement::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<
            'a,
            QS,
            ST,
            DB,
        > diesel::JoinTo<
            diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            >,
        > for table
        where
            diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            >: diesel::JoinTo<table>,
            QS: diesel::query_source::QuerySource,
        {
            type FromClause = diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            >;
            type OnClause = <diesel::internal::table_macro::BoxedSelectStatement<
                'a,
                diesel::internal::table_macro::FromClause<QS>,
                ST,
                DB,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::internal::table_macro::BoxedSelectStatement<
                    'a,
                    diesel::internal::table_macro::FromClause<QS>,
                    ST,
                    DB,
                >,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::internal::table_macro::BoxedSelectStatement::join_target(
                    table,
                );
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<S> diesel::JoinTo<diesel::query_source::Alias<S>> for table
        where
            diesel::query_source::Alias<S>: diesel::JoinTo<table>,
        {
            type FromClause = diesel::query_source::Alias<S>;
            type OnClause = <diesel::query_source::Alias<
                S,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::query_source::Alias<S>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::query_source::Alias::<
                    S,
                >::join_target(table);
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<T> diesel::insertable::Insertable<T> for table
        where
            <table as diesel::query_builder::AsQuery>::Query: diesel::insertable::Insertable<
                T,
            >,
        {
            type Values = <<table as diesel::query_builder::AsQuery>::Query as diesel::insertable::Insertable<
                T,
            >>::Values;
            fn values(self) -> Self::Values {
                use diesel::query_builder::AsQuery;
                self.as_query().values()
            }
        }
        impl<'a, T> diesel::insertable::Insertable<T> for &'a table
        where
            table: diesel::insertable::Insertable<T>,
        {
            type Values = <table as diesel::insertable::Insertable<T>>::Values;
            fn values(self) -> Self::Values {
                (*self).values()
            }
        }
        impl<S> diesel::JoinTo<diesel::query_builder::Only<S>> for table
        where
            diesel::query_builder::Only<S>: diesel::JoinTo<table>,
        {
            type FromClause = diesel::query_builder::Only<S>;
            type OnClause = <diesel::query_builder::Only<
                S,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::query_builder::Only<S>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::query_builder::Only::<
                    S,
                >::join_target(table);
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl diesel::query_source::AppearsInFromClause<
            diesel::query_builder::Only<table>,
        > for table {
            type Count = diesel::query_source::Once;
        }
        impl diesel::query_source::AppearsInFromClause<table>
        for diesel::query_builder::Only<table> {
            type Count = diesel::query_source::Once;
        }
        impl<S, TSM> diesel::JoinTo<diesel::query_builder::Tablesample<S, TSM>> for table
        where
            diesel::query_builder::Tablesample<S, TSM>: diesel::JoinTo<table>,
            TSM: diesel::internal::table_macro::TablesampleMethod,
        {
            type FromClause = diesel::query_builder::Tablesample<S, TSM>;
            type OnClause = <diesel::query_builder::Tablesample<
                S,
                TSM,
            > as diesel::JoinTo<table>>::OnClause;
            fn join_target(
                __diesel_internal_rhs: diesel::query_builder::Tablesample<S, TSM>,
            ) -> (Self::FromClause, Self::OnClause) {
                let (_, __diesel_internal_on_clause) = diesel::query_builder::Tablesample::<
                    S,
                    TSM,
                >::join_target(table);
                (__diesel_internal_rhs, __diesel_internal_on_clause)
            }
        }
        impl<
            TSM,
        > diesel::query_source::AppearsInFromClause<
            diesel::query_builder::Tablesample<table, TSM>,
        > for table
        where
            TSM: diesel::internal::table_macro::TablesampleMethod,
        {
            type Count = diesel::query_source::Once;
        }
        impl<TSM> diesel::query_source::AppearsInFromClause<table>
        for diesel::query_builder::Tablesample<table, TSM>
        where
            TSM: diesel::internal::table_macro::TablesampleMethod,
        {
            type Count = diesel::query_source::Once;
        }
        /// Contains all of the columns of this table
        pub mod columns {
            use ::diesel;
            use super::table;
            use crate::database::plutus::sql_types::*;
            use diesel::sql_types::*;
            #[allow(non_camel_case_types, dead_code)]
            /// Represents `table_name.*`, which is sometimes needed for
            /// efficient count queries. It cannot be used in place of
            /// `all_columns`, and has a `SqlType` of `()` to prevent it
            /// being used that way
            pub struct star;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for star {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "star")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for star {
                #[inline]
                fn clone(&self) -> star {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for star {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for star {
                    type QueryId = star;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            impl<__GB> diesel::expression::ValidGrouping<__GB> for star
            where
                (
                    utxo_ref,
                    value,
                    address,
                    datum,
                    created_at,
                    deleted_at,
                ): diesel::expression::ValidGrouping<__GB>,
            {
                type IsAggregate = <(
                    utxo_ref,
                    value,
                    address,
                    datum,
                    created_at,
                    deleted_at,
                ) as diesel::expression::ValidGrouping<__GB>>::IsAggregate;
            }
            impl diesel::Expression for star {
                type SqlType = diesel::expression::expression_types::NotSelectable;
            }
            impl<DB: diesel::backend::Backend> diesel::query_builder::QueryFragment<DB>
            for star
            where
                <table as diesel::QuerySource>::FromClause: diesel::query_builder::QueryFragment<
                    DB,
                >,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    use diesel::QuerySource;
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_sql("*");
                    Ok(())
                }
            }
            impl diesel::SelectableExpression<table> for star {}
            impl diesel::AppearsOnTable<table> for star {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct utxo_ref;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for utxo_ref {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "utxo_ref")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for utxo_ref {
                #[inline]
                fn clone(&self) -> utxo_ref {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for utxo_ref {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for utxo_ref {
                    type QueryId = utxo_ref;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for utxo_ref {
                #[inline]
                fn default() -> utxo_ref {
                    utxo_ref {}
                }
            }
            impl diesel::expression::Expression for utxo_ref {
                type SqlType = TransactionInput;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for utxo_ref
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("utxo_ref")
                }
            }
            impl diesel::SelectableExpression<super::table> for utxo_ref {}
            impl<QS> diesel::AppearsOnTable<QS> for utxo_ref
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for utxo_ref
            where
                utxo_ref: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for utxo_ref
            where
                utxo_ref: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for utxo_ref
            where
                utxo_ref: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for utxo_ref
            where
                From: diesel::query_source::QuerySource,
                utxo_ref: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for utxo_ref
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    utxo_ref,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for utxo_ref {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<utxo_ref> for utxo_ref {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for utxo_ref {
                type Table = super::table;
                const NAME: &'static str = "utxo_ref";
            }
            impl<T> diesel::EqAll<T> for utxo_ref
            where
                T: diesel::expression::AsExpression<TransactionInput>,
                diesel::dsl::Eq<
                    utxo_ref,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for utxo_ref {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for utxo_ref {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for utxo_ref
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for utxo_ref
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct value;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for value {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "value")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for value {
                #[inline]
                fn clone(&self) -> value {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for value {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for value {
                    type QueryId = value;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for value {
                #[inline]
                fn default() -> value {
                    value {}
                }
            }
            impl diesel::expression::Expression for value {
                type SqlType = Value;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for value
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("value")
                }
            }
            impl diesel::SelectableExpression<super::table> for value {}
            impl<QS> diesel::AppearsOnTable<QS> for value
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for value
            where
                value: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for value
            where
                value: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for value
            where
                value: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for value
            where
                From: diesel::query_source::QuerySource,
                value: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for value
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    value,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for value {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for value {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for value {
                type Table = super::table;
                const NAME: &'static str = "value";
            }
            impl<T> diesel::EqAll<T> for value
            where
                T: diesel::expression::AsExpression<Value>,
                diesel::dsl::Eq<
                    value,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for value {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for value {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for value
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for value
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct address;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for address {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "address")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for address {
                #[inline]
                fn clone(&self) -> address {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for address {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for address {
                    type QueryId = address;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for address {
                #[inline]
                fn default() -> address {
                    address {}
                }
            }
            impl diesel::expression::Expression for address {
                type SqlType = Address;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for address
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("address")
                }
            }
            impl diesel::SelectableExpression<super::table> for address {}
            impl<QS> diesel::AppearsOnTable<QS> for address
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for address
            where
                address: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for address
            where
                address: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for address
            where
                address: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for address
            where
                From: diesel::query_source::QuerySource,
                address: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for address
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    address,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for address {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for address {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for address {
                type Table = super::table;
                const NAME: &'static str = "address";
            }
            impl<T> diesel::EqAll<T> for address
            where
                T: diesel::expression::AsExpression<Address>,
                diesel::dsl::Eq<
                    address,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for address {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for address {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for address
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for address
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct datum;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for datum {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "datum")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for datum {
                #[inline]
                fn clone(&self) -> datum {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for datum {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for datum {
                    type QueryId = datum;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for datum {
                #[inline]
                fn default() -> datum {
                    datum {}
                }
            }
            impl diesel::expression::Expression for datum {
                type SqlType = OutputDatum;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for datum
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("datum")
                }
            }
            impl diesel::SelectableExpression<super::table> for datum {}
            impl<QS> diesel::AppearsOnTable<QS> for datum
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for datum
            where
                datum: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for datum
            where
                datum: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for datum
            where
                datum: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for datum
            where
                From: diesel::query_source::QuerySource,
                datum: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for datum
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    datum,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for datum {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for datum {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for datum {
                type Table = super::table;
                const NAME: &'static str = "datum";
            }
            impl<T> diesel::EqAll<T> for datum
            where
                T: diesel::expression::AsExpression<OutputDatum>,
                diesel::dsl::Eq<
                    datum,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for datum {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for datum {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for datum
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for datum
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct created_at;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for created_at {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "created_at")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for created_at {
                #[inline]
                fn clone(&self) -> created_at {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for created_at {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for created_at {
                    type QueryId = created_at;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for created_at {
                #[inline]
                fn default() -> created_at {
                    created_at {}
                }
            }
            impl diesel::expression::Expression for created_at {
                type SqlType = Slot;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for created_at
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("created_at")
                }
            }
            impl diesel::SelectableExpression<super::table> for created_at {}
            impl<QS> diesel::AppearsOnTable<QS> for created_at
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for created_at
            where
                created_at: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for created_at
            where
                created_at: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for created_at
            where
                created_at: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for created_at
            where
                From: diesel::query_source::QuerySource,
                created_at: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for created_at
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    created_at,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for created_at {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<created_at> for created_at {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for created_at {
                type Table = super::table;
                const NAME: &'static str = "created_at";
            }
            impl<T> diesel::EqAll<T> for created_at
            where
                T: diesel::expression::AsExpression<Slot>,
                diesel::dsl::Eq<
                    created_at,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for created_at {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for created_at {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for created_at
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for created_at
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            #[allow(non_camel_case_types, dead_code)]
            pub struct deleted_at;
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::fmt::Debug for deleted_at {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(f, "deleted_at")
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::clone::Clone for deleted_at {
                #[inline]
                fn clone(&self) -> deleted_at {
                    *self
                }
            }
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::marker::Copy for deleted_at {}
            #[allow(unused_imports)]
            const _: () = {
                use diesel;
                use diesel::query_builder::QueryId;
                #[allow(non_camel_case_types)]
                impl QueryId for deleted_at {
                    type QueryId = deleted_at;
                    const HAS_STATIC_QUERY_ID: bool = true;
                }
            };
            #[automatically_derived]
            #[allow(non_camel_case_types, dead_code)]
            impl ::core::default::Default for deleted_at {
                #[inline]
                fn default() -> deleted_at {
                    deleted_at {}
                }
            }
            impl diesel::expression::Expression for deleted_at {
                type SqlType = Nullable<Slot>;
            }
            impl<DB> diesel::query_builder::QueryFragment<DB> for deleted_at
            where
                DB: diesel::backend::Backend,
                diesel::internal::table_macro::StaticQueryFragmentInstance<
                    table,
                >: diesel::query_builder::QueryFragment<DB>,
            {
                #[allow(non_snake_case)]
                fn walk_ast<'b>(
                    &'b self,
                    mut __diesel_internal_out: diesel::query_builder::AstPass<'_, 'b, DB>,
                ) -> diesel::result::QueryResult<()> {
                    if !__diesel_internal_out.should_skip_from() {
                        const FROM_CLAUSE: diesel::internal::table_macro::StaticQueryFragmentInstance<
                            table,
                        > = diesel::internal::table_macro::StaticQueryFragmentInstance::new();
                        FROM_CLAUSE.walk_ast(__diesel_internal_out.reborrow())?;
                        __diesel_internal_out.push_sql(".");
                    }
                    __diesel_internal_out.push_identifier("deleted_at")
                }
            }
            impl diesel::SelectableExpression<super::table> for deleted_at {}
            impl<QS> diesel::AppearsOnTable<QS> for deleted_at
            where
                QS: diesel::query_source::AppearsInFromClause<
                    super::table,
                    Count = diesel::query_source::Once,
                >,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::LeftOuter,
                >,
            > for deleted_at
            where
                deleted_at: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::LeftOuter,
                    >,
                >,
                Self: diesel::SelectableExpression<Left>,
                Right: diesel::query_source::AppearsInFromClause<
                        super::table,
                        Count = diesel::query_source::Never,
                    > + diesel::query_source::QuerySource,
                Left: diesel::query_source::QuerySource,
            {}
            impl<
                Left,
                Right,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::Join<
                    Left,
                    Right,
                    diesel::internal::table_macro::Inner,
                >,
            > for deleted_at
            where
                deleted_at: diesel::AppearsOnTable<
                    diesel::internal::table_macro::Join<
                        Left,
                        Right,
                        diesel::internal::table_macro::Inner,
                    >,
                >,
                Left: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                Right: diesel::query_source::AppearsInFromClause<super::table>
                    + diesel::query_source::QuerySource,
                (
                    Left::Count,
                    Right::Count,
                ): diesel::internal::table_macro::Pick<Left, Right>,
                Self: diesel::SelectableExpression<
                    <(
                        Left::Count,
                        Right::Count,
                    ) as diesel::internal::table_macro::Pick<Left, Right>>::Selection,
                >,
            {}
            impl<
                Join,
                On,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::JoinOn<Join, On>,
            > for deleted_at
            where
                deleted_at: diesel::SelectableExpression<Join>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::JoinOn<Join, On>,
                    >,
            {}
            impl<
                From,
            > diesel::SelectableExpression<
                diesel::internal::table_macro::SelectStatement<
                    diesel::internal::table_macro::FromClause<From>,
                >,
            > for deleted_at
            where
                From: diesel::query_source::QuerySource,
                deleted_at: diesel::SelectableExpression<From>
                    + diesel::AppearsOnTable<
                        diesel::internal::table_macro::SelectStatement<
                            diesel::internal::table_macro::FromClause<From>,
                        >,
                    >,
            {}
            impl<__GB> diesel::expression::ValidGrouping<__GB> for deleted_at
            where
                __GB: diesel::expression::IsContainedInGroupBy<
                    deleted_at,
                    Output = diesel::expression::is_contained_in_group_by::Yes,
                >,
            {
                type IsAggregate = diesel::expression::is_aggregate::Yes;
            }
            impl diesel::expression::ValidGrouping<()> for deleted_at {
                type IsAggregate = diesel::expression::is_aggregate::No;
            }
            impl diesel::expression::IsContainedInGroupBy<deleted_at> for deleted_at {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::query_source::Column for deleted_at {
                type Table = super::table;
                const NAME: &'static str = "deleted_at";
            }
            impl<T> diesel::EqAll<T> for deleted_at
            where
                T: diesel::expression::AsExpression<Nullable<Slot>>,
                diesel::dsl::Eq<
                    deleted_at,
                    T::Expression,
                >: diesel::Expression<SqlType = diesel::sql_types::Bool>,
            {
                type Output = diesel::dsl::Eq<Self, T::Expression>;
                fn eq_all(self, __diesel_internal_rhs: T) -> Self::Output {
                    use diesel::expression_methods::ExpressionMethods;
                    self.eq(__diesel_internal_rhs)
                }
            }
            impl diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Only<super::table>,
            > for deleted_at {
                type Count = diesel::query_source::Once;
            }
            impl diesel::SelectableExpression<diesel::query_builder::Only<super::table>>
            for deleted_at {}
            impl<
                TSM,
            > diesel::query_source::AppearsInFromClause<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for deleted_at
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {
                type Count = diesel::query_source::Once;
            }
            impl<
                TSM,
            > diesel::SelectableExpression<
                diesel::query_builder::Tablesample<super::table, TSM>,
            > for deleted_at
            where
                TSM: diesel::internal::table_macro::TablesampleMethod,
            {}
            impl diesel::expression::IsContainedInGroupBy<utxo_ref> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for utxo_ref {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<utxo_ref> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for utxo_ref {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<utxo_ref> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for utxo_ref {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<utxo_ref> for created_at {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<created_at> for utxo_ref {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<utxo_ref> for deleted_at {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<deleted_at> for utxo_ref {
                type Output = diesel::expression::is_contained_in_group_by::Yes;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for created_at {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<created_at> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<value> for deleted_at {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<deleted_at> for value {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for created_at {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<created_at> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<address> for deleted_at {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<deleted_at> for address {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for created_at {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<created_at> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<datum> for deleted_at {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<deleted_at> for datum {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<created_at> for deleted_at {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
            impl diesel::expression::IsContainedInGroupBy<deleted_at> for created_at {
                type Output = diesel::expression::is_contained_in_group_by::No;
            }
        }
    }
    impl ::diesel::query_source::TableNotEqual<sync_progress::table> for testdb::table {}
    impl ::diesel::query_source::TableNotEqual<testdb::table> for sync_progress::table {}
    impl ::diesel::query_source::TableNotEqual<sync_progress::table>
    for ::diesel::query_builder::Only<testdb::table> {}
    impl ::diesel::query_source::TableNotEqual<testdb::table>
    for ::diesel::query_builder::Only<sync_progress::table> {}
    impl ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Only<sync_progress::table>,
    > for testdb::table {}
    impl ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Only<testdb::table>,
    > for sync_progress::table {}
    impl<TSM> ::diesel::query_source::TableNotEqual<sync_progress::table>
    for ::diesel::query_builder::Tablesample<testdb::table, TSM>
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
    impl<TSM> ::diesel::query_source::TableNotEqual<testdb::table>
    for ::diesel::query_builder::Tablesample<sync_progress::table, TSM>
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
    impl<
        TSM,
    > ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Tablesample<sync_progress::table, TSM>,
    > for testdb::table
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
    impl<
        TSM,
    > ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Tablesample<testdb::table, TSM>,
    > for sync_progress::table
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
    impl ::diesel::query_source::TableNotEqual<sync_progress::table> for utxos::table {}
    impl ::diesel::query_source::TableNotEqual<utxos::table> for sync_progress::table {}
    impl ::diesel::query_source::TableNotEqual<sync_progress::table>
    for ::diesel::query_builder::Only<utxos::table> {}
    impl ::diesel::query_source::TableNotEqual<utxos::table>
    for ::diesel::query_builder::Only<sync_progress::table> {}
    impl ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Only<sync_progress::table>,
    > for utxos::table {}
    impl ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Only<utxos::table>,
    > for sync_progress::table {}
    impl<TSM> ::diesel::query_source::TableNotEqual<sync_progress::table>
    for ::diesel::query_builder::Tablesample<utxos::table, TSM>
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
    impl<TSM> ::diesel::query_source::TableNotEqual<utxos::table>
    for ::diesel::query_builder::Tablesample<sync_progress::table, TSM>
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
    impl<
        TSM,
    > ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Tablesample<sync_progress::table, TSM>,
    > for utxos::table
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
    impl<
        TSM,
    > ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Tablesample<utxos::table, TSM>,
    > for sync_progress::table
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
    impl ::diesel::query_source::TableNotEqual<testdb::table> for utxos::table {}
    impl ::diesel::query_source::TableNotEqual<utxos::table> for testdb::table {}
    impl ::diesel::query_source::TableNotEqual<testdb::table>
    for ::diesel::query_builder::Only<utxos::table> {}
    impl ::diesel::query_source::TableNotEqual<utxos::table>
    for ::diesel::query_builder::Only<testdb::table> {}
    impl ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Only<testdb::table>,
    > for utxos::table {}
    impl ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Only<utxos::table>,
    > for testdb::table {}
    impl<TSM> ::diesel::query_source::TableNotEqual<testdb::table>
    for ::diesel::query_builder::Tablesample<utxos::table, TSM>
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
    impl<TSM> ::diesel::query_source::TableNotEqual<utxos::table>
    for ::diesel::query_builder::Tablesample<testdb::table, TSM>
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
    impl<
        TSM,
    > ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Tablesample<testdb::table, TSM>,
    > for utxos::table
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
    impl<
        TSM,
    > ::diesel::query_source::TableNotEqual<
        ::diesel::query_builder::Tablesample<utxos::table, TSM>,
    > for testdb::table
    where
        TSM: ::diesel::internal::table_macro::TablesampleMethod,
    {}
}
