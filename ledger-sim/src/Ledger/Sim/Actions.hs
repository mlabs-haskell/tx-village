module Ledger.Sim.Actions (
  lookupUTxO,
  lookupUTxO',
  mustLookupUTxO,
  mustLookupUTxO',
  utxosAtAddress,
  submitTx,
  getCurrentSlot,
  getsAppState,
  getAppState,
  setAppState,
  asksAppCtx,
  askAppCtx,
  throwAppError,
  incrementSlot,
  getTxId,
) where

import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)

import Codec.Serialise (serialise)
import Crypto.Hash (Blake2b_256 (Blake2b_256), hashWith)

import Control.Monad.Except (MonadError (throwError), withExceptT)
import Control.Monad.Reader (asks, mapReaderT, withReaderT)
import Control.Monad.State (mapStateT, modify')
import Control.Monad.State.Strict (gets)
import Ledger.Sim.Submission qualified as Submission
import Ledger.Sim.Types.LedgerConfig (LedgerConfig (lc'appCtx))
import Ledger.Sim.Types.LedgerSim (LedgerSimError (LedgerSimError'Application, LedgerSimError'Submission, LedgerSimError'UtxoNotFound), LedgerSimT)
import Ledger.Sim.Types.LedgerState (LedgerState (ls'currentTime, ls'userState, ls'utxos))
import Ledger.Sim.Types.Submission (SubmissionEnv (SubmissionEnv), SubmissionResult)
import PlutusLedgerApi.V2 (
  Address,
  POSIXTime (getPOSIXTime),
  TxId (TxId),
  TxInInfo (TxInInfo),
  TxInfo,
  TxOut (txOutAddress),
  TxOutRef,
 )
import PlutusTx.Builtins qualified as PlutusTx

lookupUTxO :: (Monad m) => TxOutRef -> LedgerSimT ctx st e m (Maybe TxOut)
lookupUTxO ref = M.lookup ref <$> gets ls'utxos

lookupUTxO' :: (Monad m) => TxOutRef -> LedgerSimT ctx st e m (Maybe TxInInfo)
lookupUTxO' ref = (TxInInfo ref <$>) <$> lookupUTxO ref

mustLookupUTxO :: (Monad m) => TxOutRef -> LedgerSimT ctx st e m TxOut
mustLookupUTxO ref = do
  txOut <- lookupUTxO ref
  maybe (throwError (LedgerSimError'UtxoNotFound ref)) pure txOut

mustLookupUTxO' :: (Monad m) => TxOutRef -> LedgerSimT ctx st e m TxInInfo
mustLookupUTxO' ref = do
  txInInfo <- lookupUTxO' ref
  maybe (throwError (LedgerSimError'UtxoNotFound ref)) pure txInInfo

utxosAtAddress :: (Monad m) => Address -> LedgerSimT ctx st e m [TxInInfo]
utxosAtAddress addr =
  mapMaybe
    ( \(ref, txOut) ->
        if txOutAddress txOut == addr
          then Just $ TxInInfo ref txOut
          else Nothing
    )
    . M.assocs
    <$> gets ls'utxos

{- Known shortcomings:
- Tx id must be generated and is not generated by hashing the tx body (how it works on the chain).
  This is because we don't have a real Cardano.Api TxBody available at hand and creating one from PLA would
  require a lot of effort.
  As a stopgap, Tx id is generated from current POSIX time. This may violate the assumptions of any scripts that
  use hashing inside them. ex: script that checks `txId txInfo != blake2b_224 time` where time is some time from validity range.
- See: 'checkTx'
-}
submitTx :: (Monad m) => TxInfo -> LedgerSimT ctx st e m SubmissionResult
submitTx txInfo =
  withReaderT (SubmissionEnv txInfo) $
    mapReaderT
      (mapStateT (withExceptT LedgerSimError'Submission))
      Submission.submit

getCurrentSlot :: (Monad m) => LedgerSimT ctx st e m POSIXTime
getCurrentSlot = gets ls'currentTime

-- | Get a specific component of the user state from the ledger, using given projection function.
getsAppState :: (Monad m) => (st -> a) -> LedgerSimT ctx st e m a
getsAppState f = gets $ f . ls'userState

-- | Get the user state from the ledger.
getAppState :: (Monad m) => LedgerSimT ctx st e m st
getAppState = getsAppState id

-- | Set the user state
setAppState :: (Monad m) => st -> LedgerSimT ctx st e m ()
setAppState st = modify' $ \s ->
  s
    { ls'userState = st
    }

-- | Get a specific component of the user state from the ledger, using given projection function.
asksAppCtx :: (Monad m) => (ctx -> a) -> LedgerSimT ctx st e m a
asksAppCtx f = asks $ f . lc'appCtx

-- | Get the user state from the ledger.
askAppCtx :: (Monad m) => LedgerSimT ctx st e m ctx
askAppCtx = asksAppCtx id

-- | Throw custom application error.
throwAppError :: (Monad m) => e -> LedgerSimT ctx st e m a
throwAppError = throwError . LedgerSimError'Application

incrementSlot :: (Monad m) => LedgerSimT ctx st e m ()
incrementSlot =
  modify' $ \st -> st {ls'currentTime = ls'currentTime st + 1}

{- | Generate tx id from time. NOTE: This is simply a stopgap measure used in the simulator. In reality,
tx ids are hashes of a transaction body.
-}
genTxId :: POSIXTime -> TxId
genTxId =
  TxId
    . PlutusTx.toBuiltin
    . convert @_ @ByteString
    . hashWith Blake2b_256
    . LBS.toStrict
    . serialise
    . getPOSIXTime

getTxId :: (Monad m) => LedgerSimT ctx st e m TxId
getTxId = gets $ genTxId . ls'currentTime
