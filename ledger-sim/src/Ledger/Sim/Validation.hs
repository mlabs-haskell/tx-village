module Ledger.Sim.Validation (
  InvalidTxInfoError (..),
  validateTxInfo,
  runTxInfoValidation,
) where

import Control.Composition ((.*))
import Ledger.Sim.Types.Config (LedgerConfig)
import Ledger.Sim.Types.State (LedgerState)
import Ledger.Sim.Validation.Local qualified as Local
import Ledger.Sim.Validation.Normality qualified as Normality
import Ledger.Sim.Validation.Stateful qualified as Stateful
import Ledger.Sim.Validation.Validator (
  Validator,
  Validity,
  mapErr,
  runValidator,
 )
import PlutusLedgerApi.V2 (TxInfo)

data InvalidTxInfoError
  = InvalidTxInfoError'Normality Normality.InvalidTxInfoError
  | InvalidTxInfoError'Local Local.InvalidTxInfoError
  | InvalidTxInfoError'Stateful Stateful.InvalidTxInfoError
  deriving stock (Show, Eq)

validateTxInfo :: LedgerConfig cfg -> LedgerState st -> Validator InvalidTxInfoError TxInfo
validateTxInfo config state =
  mconcat
    [ mapErr InvalidTxInfoError'Normality Normality.validateTxInfo
    , mapErr InvalidTxInfoError'Local Local.validateTxInfo
    , mapErr InvalidTxInfoError'Stateful $ Stateful.validateTxInfo config state
    ]

runTxInfoValidation :: LedgerConfig cfg -> LedgerState st -> TxInfo -> Validity InvalidTxInfoError
runTxInfoValidation = runValidator .* validateTxInfo
