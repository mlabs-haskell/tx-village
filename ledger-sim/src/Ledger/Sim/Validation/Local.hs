module Ledger.Sim.Validation.Local (
  UnspendableInputError (..),
  InvalidInputsError (..),
  InvalidOutputError (..),
  InvalidOutputsError (..),
  InvalidRedeemersError (..),
  BalancingError (..),
  InvalidTxInfoError (..),
  validateTxInfo,
) where

import Control.Category ((>>>))
import Control.Monad (join)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (Decidable (choose))
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Ledger.Sim.Validation.Validator (
  Validator,
  contramapAndMapErr,
  mapErr,
  validateBool,
  validateFail,
  validateFoldable,
  validateIf,
  validateListAndAnnotateErrWithIdx,
  validatePass,
  validateWith,
 )
import PlutusLedgerApi.V2 (
  Address (addressCredential),
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol,
  DatumHash,
  OutputDatum (NoOutputDatum, OutputDatumHash),
  PubKeyHash,
  Redeemer,
  ScriptHash,
  ScriptPurpose (Minting, Spending),
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (
    txInfoData,
    txInfoFee,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoRedeemers,
    txInfoSignatories
  ),
  TxOut (txOutAddress, txOutDatum, txOutValue),
  TxOutRef,
  Value (getValue),
  adaSymbol,
 )
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Numeric qualified as PlutusTx

data UnspendableInputError
  = UnspendableInputError'ScriptInput'NoDatum
  | UnspendableInputError'ScriptInput'DatumWitnessMissing DatumHash
  | UnspendableInputError'PubKeyInput'NotAuthorized PubKeyHash

validateInputSpendable :: TxInfo -> Validator UnspendableInputError TxOut
validateInputSpendable txInfo =
  choose
    ( \txOut ->
        case addressCredential $ txOutAddress txOut of
          PubKeyCredential pkh -> Left pkh
          ScriptCredential _ -> Right $ txOutDatum txOut
    )
    validatePubKeyHashInSignatories
    validateScriptOutputDatum
  where
    validatePubKeyHashInSignatories :: Validator UnspendableInputError PubKeyHash
    validatePubKeyHashInSignatories =
      validateIf
        (`L.elem` txInfoSignatories txInfo)
        UnspendableInputError'PubKeyInput'NotAuthorized

    validateScriptOutputDatum :: Validator UnspendableInputError OutputDatum
    validateScriptOutputDatum =
      choose
        ( \case
            OutputDatumHash h -> Left h
            d -> Right d
        )
        ( validateIf
            (`AssocMap.member` txInfoData txInfo)
            UnspendableInputError'ScriptInput'DatumWitnessMissing
        )
        $ choose
          ( \case
              NoOutputDatum -> Left ()
              _ -> Right ()
          )
          (validateFail UnspendableInputError'ScriptInput'NoDatum)
          validatePass

--------------------------------------------------------------------------------

data InvalidInputsError
  = InvalidInputsError'UnspendableInput Int UnspendableInputError

validateInputs :: TxInfo -> Validator InvalidInputsError [TxInInfo]
validateInputs =
  validateListAndAnnotateErrWithIdx InvalidInputsError'UnspendableInput
    . contramap txInInfoResolved
    . validateInputSpendable

--------------------------------------------------------------------------------

data InvalidOutputError = InvalidOutputError'ScriptOutputNoDatum

validateOutput :: TxInfo -> Validator InvalidOutputError TxOut
validateOutput _ =
  validateIf
    ( txOutDatum >>> \case
        NoOutputDatum -> False
        _ -> True
    )
    $ const InvalidOutputError'ScriptOutputNoDatum

--------------------------------------------------------------------------------

data InvalidOutputsError = InvalidOutputsError'InvalidOutput Int InvalidOutputError

validateOutputs :: TxInfo -> Validator InvalidOutputsError [TxOut]
validateOutputs =
  validateListAndAnnotateErrWithIdx InvalidOutputsError'InvalidOutput . validateOutput

--------------------------------------------------------------------------------

data InvalidRedeemersError
  = BadRedeemers'ValidatorNotRun ScriptHash TxOutRef
  | BadRedeemers'MintingPolicyNotRan CurrencySymbol
  | BadRedeemers'UnexpectedExcessEntries [ScriptPurpose]

validateRedeemers ::
  TxInfo ->
  Validator
    InvalidRedeemersError
    (AssocMap.Map ScriptPurpose Redeemer)
validateRedeemers txInfo = validateWith $ \redeemers ->
  let isPurposeInMap = flip AssocMap.member redeemers

      validatorsToRun =
        mapMaybe
          ( join $
              txInInfoResolved >>> txOutAddress >>> addressCredential >>> \case
                ScriptCredential sh -> Just . (sh,) . txInInfoOutRef
                _ -> const Nothing
          )
          $ txInfoInputs txInfo

      mintingPoliciesToRun =
        filter (/= adaSymbol) $
          AssocMap.keys $
            getValue $
              txInfoMint txInfo

      excessEntries =
        AssocMap.keys $
          foldr
            AssocMap.delete
            redeemers
            (fmap (Spending . snd) validatorsToRun <> fmap Minting mintingPoliciesToRun)
   in mconcat
        [ contramap (const validatorsToRun) $
            validateFoldable $
              validateIf
                (isPurposeInMap . Spending . snd)
                (uncurry BadRedeemers'ValidatorNotRun)
        , contramap (const mintingPoliciesToRun) $
            validateFoldable $
              validateIf (isPurposeInMap . Minting) BadRedeemers'MintingPolicyNotRan
        , contramap (const excessEntries) $
            validateIf null BadRedeemers'UnexpectedExcessEntries
        ]

--------------------------------------------------------------------------------

data BalancingError
  = BalancingError
      Value -- Total input
      Value -- Total minted
      Value -- Fee
      Value -- Total output
      Value -- Delta, input + minted - fee - output

validateBalancing :: Validator BalancingError TxInfo
validateBalancing = validateWith $ \txInfo ->
  let
    inputValue = foldMap (txOutValue . txInInfoResolved) $ txInfoInputs txInfo
    minted = txInfoMint txInfo
    fee = txInfoFee txInfo
    outputValue = foldMap txOutValue $ txInfoOutputs txInfo
    delta = inputValue <> minted <> PlutusTx.negate (fee <> outputValue)
   in
    contramap
      (const $ AssocMap.all (AssocMap.all (== 0)) $ getValue delta)
      $ validateBool
      $ BalancingError inputValue minted fee outputValue delta

--------------------------------------------------------------------------------

data InvalidTxInfoError
  = InvalidTxInfoError'InvalidInputs InvalidInputsError
  | InvalidTxInfoError'InvalidOutputs InvalidOutputsError
  | InvalidTxInfoError'InvalidRedeemers InvalidRedeemersError
  | InvalidTxInfoError'NotBalanced BalancingError

validateTxInfo :: Validator InvalidTxInfoError TxInfo
validateTxInfo =
  mconcat
    [ validateWith $ contramapAndMapErr txInfoInputs InvalidTxInfoError'InvalidInputs . validateInputs
    , validateWith $ contramapAndMapErr txInfoOutputs InvalidTxInfoError'InvalidOutputs . validateOutputs
    , validateWith $ contramapAndMapErr txInfoRedeemers InvalidTxInfoError'InvalidRedeemers . validateRedeemers
    , mapErr InvalidTxInfoError'NotBalanced validateBalancing
    ]
