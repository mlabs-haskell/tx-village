module Ledger.Sim.Types.LedgerSim (
  LedgerSimT,
  LedgerSim,
  LedgerSimError (..),
  runLedgerSimT,
  runLedgerSim,
) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT))
import Ledger.Sim.Types.LedgerConfig (LedgerConfig)
import Ledger.Sim.Types.LedgerState (LedgerState)
import Ledger.Sim.Types.Submission (SubmissionError)
import PlutusLedgerApi.V2 (TxOutRef)

type LedgerSimT ctx st e m =
  ReaderT
    (LedgerConfig ctx)
    ( StateT
        (LedgerState st)
        (ExceptT (LedgerSimError e) m)
    )

type LedgerSim ctx st e = LedgerSimT ctx st e Identity

data LedgerSimError e
  = LedgerSimError'Submission SubmissionError
  | LedgerSimError'UtxoNotFound TxOutRef
  | LedgerSimError'Application e
  deriving stock (Show, Eq)

runLedgerSimT ::
  (Functor m) =>
  LedgerConfig ctx ->
  LedgerState st ->
  LedgerSimT ctx st e m a ->
  m (Either (LedgerSimError e) a)
runLedgerSimT ledgerCfg ledgerState =
  runExceptT
    . fmap fst
    . flip runStateT ledgerState
    . flip runReaderT ledgerCfg

runLedgerSim ::
  LedgerConfig ctx ->
  LedgerState st ->
  LedgerSim ctx st e a ->
  Either (LedgerSimError e) a
runLedgerSim cfg st = runIdentity . runLedgerSimT cfg st
