module Ledger.Sim.Validation.Stateful (
  InvalidInputError (..),
  InvalidInputsError (..),
  InvalidReferenceInputsError (..),
  InvalidValidRangeError (..),
  ScriptPurpose (..),
  InvalidRedeemerError (..),
  InvalidRedeemersError (..),
  InvalidTxInfoError (..),
  validateTxInfo,
) where

import Control.Applicative (liftA3)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (Decidable (choose))
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Ledger.Sim.Types.LedgerConfig (LedgerConfig (lc'scriptStorage), ScriptMode (ScriptMode'AsReference, ScriptMode'AsWitness))
import Ledger.Sim.Types.LedgerState (LedgerState (ls'currentTime, ls'utxos))
import Ledger.Sim.Validation.Validator (
  Validator,
  contramapAndMapErr,
  validateFail,
  validateFoldable,
  validateIf,
  validateListAndAnnotateErrWithIdx,
  validateOptional,
  validateWith,
 )
import PlutusLedgerApi.V1.Interval qualified as IV
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Address (addressCredential),
  Credential (ScriptCredential),
  CurrencySymbol (unCurrencySymbol),
  POSIXTime,
  POSIXTimeRange,
  ScriptHash (ScriptHash),
  TxInInfo (
    txInInfoOutRef,
    txInInfoResolved
  ),
  TxInfo (txInfoInputs, txInfoMint, txInfoReferenceInputs, txInfoValidRange),
  TxOut (txOutAddress, txOutReferenceScript),
  TxOutRef,
  Value,
 )

--------------------------------------------------------------------------------

data InvalidInputError
  = InvalidInputError'NotFoundOnLedger TxOutRef
  | InvalidInputError'ReferenceScriptNotAvailable ScriptHash
  deriving stock (Show, Eq)
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
  deriving stock (Show, Eq)

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
  deriving stock (Show, Eq)

validateReferenceInputs ::
  LedgerState st ->
  Validator
    InvalidReferenceInputsError
    [TxInInfo]
validateReferenceInputs =
  validateListAndAnnotateErrWithIdx InvalidReferenceInputsError'InvalidInput . validateInputExist

--------------------------------------------------------------------------------

newtype InvalidValidRangeError = InvalidValidRangeError'CurrentTimeOutOfRange POSIXTime
  deriving stock (Show, Eq)

validateValidRange :: LedgerState st -> Validator InvalidValidRangeError POSIXTimeRange
validateValidRange state =
  let currentTime = ls'currentTime state
   in validateIf (IV.member currentTime) $ const $ InvalidValidRangeError'CurrentTimeOutOfRange currentTime

--------------------------------------------------------------------------------

data ScriptPurpose = ScriptPurpose'Spending | ScriptPurpose'Minting
  deriving stock (Show, Eq)

data InvalidRedeemerError
  = InvalidRedeemerError'MissingScriptInConfig
  | InvalidRedeemerError'MissingReferenceScript
  deriving stock (Show, Eq)

data InvalidRedeemersError = InvalidRedeemersError ScriptPurpose InvalidRedeemerError ScriptHash
  deriving stock (Show, Eq)

validateRedeemers :: LedgerConfig ctx -> Validator InvalidRedeemersError ([TxInInfo], [TxInInfo], Value)
validateRedeemers config =
  contramap
    ( \(referenceInputs, inputs, mintValue) ->
        let
          validatorHashes =
            mapMaybe
              ( \txInInfo ->
                  let txOut = txInInfoResolved txInInfo
                   in case addressCredential $ txOutAddress txOut of
                        ScriptCredential sh -> Just sh
                        _ -> Nothing
              )
              inputs
          mintingPolicyHashes =
            fmap (ScriptHash . unCurrencySymbol) $
              filter (/= Value.adaSymbol) $
                Value.symbols mintValue

          availableReferenceScripts =
            S.fromList $
              mapMaybe (txOutReferenceScript . txInInfoResolved) referenceInputs
         in
          (availableReferenceScripts, validatorHashes, mintingPolicyHashes)
    )
    $ mconcat
      [ contramap (\(availableReferenceScripts, validatorHashes, _) -> (availableReferenceScripts, validatorHashes)) $
          validateScriptHashes ScriptPurpose'Spending
      , contramap (\(availableReferenceScripts, _, mintingPolicyHashes) -> (availableReferenceScripts, mintingPolicyHashes)) $
          validateScriptHashes ScriptPurpose'Minting
      ]
  where
    validateScriptHashes :: ScriptPurpose -> Validator InvalidRedeemersError (S.Set ScriptHash, [ScriptHash])
    validateScriptHashes k = validateWith $ \(availableReferenceScripts, _) ->
      contramap snd $
        validateFoldable $
          choose
            ( \scriptHash ->
                case M.lookup scriptHash (lc'scriptStorage config) of
                  Nothing -> Left scriptHash
                  Just (mode, _) -> Right (availableReferenceScripts, mode, scriptHash)
            )
            (validateWith $ validateFail . InvalidRedeemersError k InvalidRedeemerError'MissingScriptInConfig)
            (validateScriptHash k)

    validateScriptHash :: ScriptPurpose -> Validator InvalidRedeemersError (S.Set ScriptHash, ScriptMode, ScriptHash)
    validateScriptHash k = validateWith $ \(availableReferenceScripts, scriptMode, scriptHash) ->
      contramap (const scriptHash) $
        validateIf
          ( case scriptMode of
              ScriptMode'AsWitness -> const True
              ScriptMode'AsReference -> flip S.member availableReferenceScripts
          )
          ( InvalidRedeemersError
              k
              InvalidRedeemerError'MissingReferenceScript
          )

--------------------------------------------------------------------------------

data InvalidTxInfoError
  = InvalidTxInfoError'InvalidInputs InvalidInputsError
  | InvalidTxInfoError'InvalidReferenceInputs InvalidReferenceInputsError
  | InvalidTxInfoError'InvalidValidRange InvalidValidRangeError
  | InvalidTxInfoError'InvalidRedeemers InvalidRedeemersError
  deriving stock (Show, Eq)

validateTxInfo :: LedgerConfig cfg -> LedgerState st -> Validator InvalidTxInfoError TxInfo
validateTxInfo config state =
  mconcat
    [ contramapAndMapErr txInfoInputs InvalidTxInfoError'InvalidInputs $ validateInputs config state
    , contramapAndMapErr txInfoReferenceInputs InvalidTxInfoError'InvalidReferenceInputs $ validateReferenceInputs state
    , contramapAndMapErr txInfoValidRange InvalidTxInfoError'InvalidValidRange $ validateValidRange state
    , contramapAndMapErr (liftA3 (,,) txInfoReferenceInputs txInfoInputs txInfoMint) InvalidTxInfoError'InvalidRedeemers $
        validateRedeemers config
    ]
