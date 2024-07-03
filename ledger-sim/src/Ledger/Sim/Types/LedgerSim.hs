module Ledger.Sim.Types.LedgerSim (
  LedgerSim,
  LedgerSimError (..),
  runLedgerSim,
) where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT))
import Ledger.Sim.Types.LedgerConfig (LedgerConfig)
import Ledger.Sim.Types.LedgerState (LedgerState)
import Ledger.Sim.Types.Submission (SubmissionError)
import PlutusLedgerApi.V2 (TxOutRef)

type LedgerSim ctx st e =
  ReaderT
    (LedgerConfig ctx)
    ( StateT
        (LedgerState st)
        (Except (LedgerSimError e))
    )

data LedgerSimError e
  = LedgerSimError'Submission SubmissionError
  | LedgerSimError'UtxoNotFound TxOutRef
  | LedgerSimError'Application e
  deriving stock (Show, Eq)

runLedgerSim ::
  LedgerConfig ctx ->
  LedgerState st ->
  LedgerSim ctx st e a ->
  Either (LedgerSimError e) a
runLedgerSim ledgerCfg ledgerState =
  fmap fst
    . runExcept
    . flip runStateT ledgerState
    . flip runReaderT ledgerCfg
