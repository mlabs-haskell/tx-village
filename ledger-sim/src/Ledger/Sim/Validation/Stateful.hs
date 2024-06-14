module Ledger.Sim.Validation.Stateful (
    BadInput (..),
    BadInputs (..),
    BadReferenceInputs (..),
    BadValidRange (..),
    BadTxInfo (..),
    validateTxInfo,
) where

import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Map qualified as M
import Ledger.Sim.Types.Config (LedgerConfig (lc'scriptStorage))
import Ledger.Sim.Types.State (LedgerState (ls'currentTime, ls'utxos))
import Ledger.Sim.Validation.Validator (
    Validator,
    contramapAndMapErr,
    validateIf,
    validateListAndAnnotateErrWithIdx,
    validateOptional,
 )
import PlutusLedgerApi.V1.Interval qualified as IV
import PlutusLedgerApi.V2 (
    POSIXTime,
    POSIXTimeRange,
    ScriptHash,
    TxInInfo (
        txInInfoOutRef,
        txInInfoResolved
    ),
    TxInfo (txInfoInputs, txInfoReferenceInputs, txInfoValidRange),
    TxOut (txOutReferenceScript),
    TxOutRef,
 )

--------------------------------------------------------------------------------

data BadInput
    = BadInput'NotFound TxOutRef
    | BadInput'ReferenceScriptNotAvailable ScriptHash

validateInputExist :: LedgerState st -> Validator BadInput TxInInfo
validateInputExist state =
    contramap txInInfoOutRef $
        validateIf (`M.member` ls'utxos state) BadInput'NotFound

validateInputReferenceScriptAvailable :: LedgerConfig ctx -> Validator BadInput TxInInfo
validateInputReferenceScriptAvailable config =
    contramap (txOutReferenceScript . txInInfoResolved) $
        validateOptional $
            validateIf (`M.member` lc'scriptStorage config) BadInput'ReferenceScriptNotAvailable

--------------------------------------------------------------------------------

data BadInputs = BadInputs'BadInput Int BadInput

validateInputs ::
    LedgerConfig cfg -> LedgerState st -> Validator BadInputs [TxInInfo]
validateInputs config state =
    validateListAndAnnotateErrWithIdx BadInputs'BadInput $
        mconcat
            [ validateInputExist state
            , validateInputReferenceScriptAvailable config
            ]

--------------------------------------------------------------------------------

data BadReferenceInputs = BadReferenceInput'BadInput Int BadInput

validateReferenceInputs ::
    LedgerState st ->
    Validator
        BadReferenceInputs
        [TxInInfo]
validateReferenceInputs =
    validateListAndAnnotateErrWithIdx BadReferenceInput'BadInput . validateInputExist

--------------------------------------------------------------------------------

newtype BadValidRange = BadValidaRage'CurrentTimeOutOfRange POSIXTime

validateValidRange :: LedgerState st -> Validator BadValidRange POSIXTimeRange
validateValidRange state =
    let currentTime = ls'currentTime state
     in validateIf (IV.member currentTime) $ const $ BadValidaRage'CurrentTimeOutOfRange currentTime

--------------------------------------------------------------------------------

data BadTxInfo
    = BadTxInfo'BadInputs BadInputs
    | BadTxInfo'BadReferenceInputs BadReferenceInputs
    | BadTxInfo'BadValidRange BadValidRange

validateTxInfo :: LedgerConfig cfg -> LedgerState st -> Validator BadTxInfo TxInfo
validateTxInfo config state =
    mconcat
        [ contramapAndMapErr txInfoInputs BadTxInfo'BadInputs $ validateInputs config state
        , contramapAndMapErr txInfoReferenceInputs BadTxInfo'BadReferenceInputs $ validateReferenceInputs state
        , contramapAndMapErr txInfoValidRange BadTxInfo'BadValidRange $ validateValidRange state
        ]
