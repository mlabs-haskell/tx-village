module Ledger.Sim.Validation.Stateful (
    BadInput (..),
    BadInputs (..),
    BadReferenceInputs (..),
    BadValidRange (..),
    BadTxInfo (..),
    validateTxInfo,
) where

import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Map qualified as M
import Ledger.Sim (
    LedgerConfig (lc'scriptStorage),
    LedgerState (ls'currentTime, ls'utxos),
 )
import Ledger.Sim.Validation.Validator (
    InContext (getContext, getSubject),
    Validator,
    contramapAndMapErr,
    itemsInContext,
    sequenceFirst,
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

validateInputExist :: Validator BadInput (TxInInfo `InContext` LedgerState st)
validateInputExist =
    contramap (bimap txInInfoOutRef ls'utxos) $
        validateIf (liftA2 M.member getSubject getContext) $
            BadInput'NotFound . getSubject

validateInputReferenceScriptAvailable :: Validator BadInput (TxInInfo `InContext` LedgerConfig ctx)
validateInputReferenceScriptAvailable =
    contramap (sequenceFirst . bimap (txOutReferenceScript . txInInfoResolved) lc'scriptStorage) $
        validateOptional $
            validateIf (liftA2 M.member getSubject getContext) $
                BadInput'ReferenceScriptNotAvailable . getSubject

--------------------------------------------------------------------------------

data BadInputs = BadInputs'BadInput Int BadInput

validateInputs ::
    Validator
        BadInputs
        ([TxInInfo] `InContext` (LedgerState st, LedgerConfig cfg))
validateInputs =
    contramap itemsInContext $
        validateListAndAnnotateErrWithIdx BadInputs'BadInput $
            mconcat
                [ contramap (second fst) validateInputExist
                , contramap (second snd) validateInputReferenceScriptAvailable
                ]

--------------------------------------------------------------------------------

data BadReferenceInputs = BadReferenceInput'BadInput Int BadInput

validateReferenceInputs ::
    Validator
        BadReferenceInputs
        ([TxInInfo] `InContext` LedgerState st)
validateReferenceInputs =
    contramap itemsInContext $
        validateListAndAnnotateErrWithIdx BadReferenceInput'BadInput validateInputExist

--------------------------------------------------------------------------------

newtype BadValidRange = BadValidaRage'CurrentTimeOutOfRange POSIXTime

validateValidRange :: Validator BadValidRange (POSIXTimeRange `InContext` LedgerState st)
validateValidRange =
    contramap (second ls'currentTime) $
        validateIf (liftA2 IV.member getContext getSubject) $
            BadValidaRage'CurrentTimeOutOfRange . getContext

--------------------------------------------------------------------------------

data BadTxInfo
    = BadTxInfo'BadInputs BadInputs
    | BadTxInfo'BadReferenceInputs BadReferenceInputs
    | BadTxInfo'BadValidRange BadValidRange

validateTxInfo :: Validator BadTxInfo (TxInfo `InContext` (LedgerState st, LedgerConfig cfg))
validateTxInfo =
    mconcat
        [ contramapAndMapErr (first txInfoInputs) BadTxInfo'BadInputs validateInputs
        , contramapAndMapErr (bimap txInfoReferenceInputs fst) BadTxInfo'BadReferenceInputs validateReferenceInputs
        , contramapAndMapErr (bimap txInfoValidRange fst) BadTxInfo'BadValidRange validateValidRange
        ]
