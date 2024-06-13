module Ledger.Sim.Validation (BadTxInfo (..), runTxInfoValidation) where

import Ledger.Sim (LedgerState)
import Ledger.Sim.Types.Config (LedgerConfig)
import Ledger.Sim.Validation.Local qualified as Local
import Ledger.Sim.Validation.Normality qualified as Normality
import Ledger.Sim.Validation.Stateful qualified as Stateful
import Ledger.Sim.Validation.Validator (
    InContext (InContext, getSubject),
    Validator,
    Validity,
    contramapAndMapErr,
    mapErr,
    runValidator,
 )
import PlutusLedgerApi.V2 (TxInfo)

data BadTxInfo
    = BadTxInfo'Normality Normality.BadTxInfo
    | BadTxInfo'Local Local.BadTxInfo
    | BadTxInfo'Stateful Stateful.BadTxInfo

validateTxInfo :: Validator BadTxInfo (TxInfo `InContext` (LedgerConfig cfg, LedgerState st))
validateTxInfo =
    mconcat
        [ contramapAndMapErr getSubject BadTxInfo'Normality Normality.validateTxInfo
        , contramapAndMapErr getSubject BadTxInfo'Local Local.validateTxInfo
        , mapErr BadTxInfo'Stateful Stateful.validateTxInfo
        ]

runTxInfoValidation :: LedgerConfig cfg -> LedgerState st -> TxInfo -> Validity BadTxInfo
runTxInfoValidation cfg st txInfo = runValidator validateTxInfo $ txInfo `InContext` (cfg, st)
