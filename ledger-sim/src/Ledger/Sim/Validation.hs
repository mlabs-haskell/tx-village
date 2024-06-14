module Ledger.Sim.Validation (BadTxInfo (..), runTxInfoValidation) where

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

data BadTxInfo
  = BadTxInfo'Normality Normality.BadTxInfo
  | BadTxInfo'Local Local.BadTxInfo
  | BadTxInfo'Stateful Stateful.BadTxInfo

validateTxInfo :: LedgerConfig cfg -> LedgerState st -> Validator BadTxInfo TxInfo
validateTxInfo config state =
  mconcat
    [ mapErr BadTxInfo'Normality Normality.validateTxInfo
    , mapErr BadTxInfo'Local Local.validateTxInfo
    , mapErr BadTxInfo'Stateful $ Stateful.validateTxInfo config state
    ]

runTxInfoValidation :: LedgerConfig cfg -> LedgerState st -> TxInfo -> Validity BadTxInfo
runTxInfoValidation = runValidator .* validateTxInfo
