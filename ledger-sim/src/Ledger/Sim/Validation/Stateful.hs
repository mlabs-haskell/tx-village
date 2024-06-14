module Ledger.Sim.Validation.Stateful (
  InvalidInputError (..),
  InvalidInputsError (..),
  InvalidReferenceInputsError (..),
  InvalidValidRangeError (..),
  InvalidTxInfoError (..),
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

data InvalidInputError
  = InvalidInputError'NotFoundOnLedger TxOutRef
  | InvalidInputError'ReferenceScriptNotAvailable ScriptHash

validateInputExist :: LedgerState st -> Validator InvalidInputError TxInInfo
validateInputExist state =
  contramap txInInfoOutRef $
    validateIf (`M.member` ls'utxos state) InvalidInputError'NotFoundOnLedger

validateInputReferenceScriptAvailable :: LedgerConfig ctx -> Validator InvalidInputError TxInInfo
validateInputReferenceScriptAvailable config =
  contramap (txOutReferenceScript . txInInfoResolved) $
    validateOptional $
      validateIf (`M.member` lc'scriptStorage config) InvalidInputError'ReferenceScriptNotAvailable

--------------------------------------------------------------------------------

data InvalidInputsError = InvalidInputsError'InvalidInput Int InvalidInputError

validateInputs ::
  LedgerConfig cfg -> LedgerState st -> Validator InvalidInputsError [TxInInfo]
validateInputs config state =
  validateListAndAnnotateErrWithIdx InvalidInputsError'InvalidInput $
    mconcat
      [ validateInputExist state
      , validateInputReferenceScriptAvailable config
      ]

--------------------------------------------------------------------------------

data InvalidReferenceInputsError = InvalidReferenceInputsError'InvalidInput Int InvalidInputError

validateReferenceInputs ::
  LedgerState st ->
  Validator
    InvalidReferenceInputsError
    [TxInInfo]
validateReferenceInputs =
  validateListAndAnnotateErrWithIdx InvalidReferenceInputsError'InvalidInput . validateInputExist

--------------------------------------------------------------------------------

newtype InvalidValidRangeError = InvalidValidRangeError'CurrentTimeOutOfRange POSIXTime

validateValidRange :: LedgerState st -> Validator InvalidValidRangeError POSIXTimeRange
validateValidRange state =
  let currentTime = ls'currentTime state
   in validateIf (IV.member currentTime) $ const $ InvalidValidRangeError'CurrentTimeOutOfRange currentTime

--------------------------------------------------------------------------------

data InvalidTxInfoError
  = InvalidTxInfoError'InvalidInputs InvalidInputsError
  | InvalidTxInfoError'InvalidReferenceInputs InvalidReferenceInputsError
  | InvalidTxInfoError'InvalidValidRange InvalidValidRangeError

validateTxInfo :: LedgerConfig cfg -> LedgerState st -> Validator InvalidTxInfoError TxInfo
validateTxInfo config state =
  mconcat
    [ contramapAndMapErr txInfoInputs InvalidTxInfoError'InvalidInputs $ validateInputs config state
    , contramapAndMapErr txInfoReferenceInputs InvalidTxInfoError'InvalidReferenceInputs $ validateReferenceInputs state
    , contramapAndMapErr txInfoValidRange InvalidTxInfoError'InvalidValidRange $ validateValidRange state
    ]
