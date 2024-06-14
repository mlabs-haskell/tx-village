module Ledger.Sim.Validation.Local (
    BadUnspendableInput (..),
    BadInputs (..),
    BadOutputs (..),
    BadRedeemers (..),
    BadBalancing (..),
    BadTxInfo (..),
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

data BadUnspendableInput
    = BadUnspendableInput'ScriptInput'NoDatum
    | BadUnspendableInput'ScriptInput'DatumWitnessMissing DatumHash
    | BadUnspendableInput'PubKeyInput'NotAuthorized PubKeyHash

validateInputSpendable :: TxInfo -> Validator BadUnspendableInput TxOut
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
    validatePubKeyHashInSignatories :: Validator BadUnspendableInput PubKeyHash
    validatePubKeyHashInSignatories =
        validateIf
            (`L.elem` txInfoSignatories txInfo)
            BadUnspendableInput'PubKeyInput'NotAuthorized

    validateScriptOutputDatum :: Validator BadUnspendableInput OutputDatum
    validateScriptOutputDatum =
        choose
            ( \case
                OutputDatumHash h -> Left h
                d -> Right d
            )
            ( validateIf
                (`AssocMap.member` txInfoData txInfo)
                BadUnspendableInput'ScriptInput'DatumWitnessMissing
            )
            $ choose
                ( \case
                    NoOutputDatum -> Left ()
                    _ -> Right ()
                )
                (validateFail BadUnspendableInput'ScriptInput'NoDatum)
                validatePass

--------------------------------------------------------------------------------

data BadInputs
    = BadInputs'BadUnspendableInput Int BadUnspendableInput

validateInputs :: TxInfo -> Validator BadInputs [TxInInfo]
validateInputs =
    validateListAndAnnotateErrWithIdx BadInputs'BadUnspendableInput
        . contramap txInInfoResolved
        . validateInputSpendable

--------------------------------------------------------------------------------

data BadOutput = BadOutput'ScriptOutputNoDatum

validateOutput :: TxInfo -> Validator BadOutput TxOut
validateOutput _ =
    validateIf
        ( txOutDatum >>> \case
            NoOutputDatum -> False
            _ -> True
        )
        $ const BadOutput'ScriptOutputNoDatum

--------------------------------------------------------------------------------

data BadOutputs = BadOutputs'BadOutput Int BadOutput

validateOutputs :: TxInfo -> Validator BadOutputs [TxOut]
validateOutputs =
    validateListAndAnnotateErrWithIdx BadOutputs'BadOutput . validateOutput

--------------------------------------------------------------------------------

data BadRedeemers
    = BadRedeemers'ValidatorNotRun ScriptHash TxOutRef
    | BadRedeemers'MintingPolicyNotRan CurrencySymbol
    | BadRedeemers'UnexpectedExcessEntries [ScriptPurpose]

validateRedeemers ::
    TxInfo ->
    Validator
        BadRedeemers
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

data BadBalancing
    = BadBalancing'NotBalanced
        Value -- Total input
        Value -- Total minted
        Value -- Fee
        Value -- Total output
        Value -- Delta, input + minted - fee - output

validateBalancing :: Validator BadBalancing TxInfo
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
            $ BadBalancing'NotBalanced inputValue minted fee outputValue delta

--------------------------------------------------------------------------------

data BadTxInfo
    = BadTxInfo'BadInputs BadInputs
    | BadTxInfo'BadOutputs BadOutputs
    | BadTxInfo'BadRedeemers BadRedeemers
    | BadTxInfo'BadBalaning BadBalancing

validateTxInfo :: Validator BadTxInfo TxInfo
validateTxInfo =
    mconcat
        [ validateWith $ contramapAndMapErr txInfoInputs BadTxInfo'BadInputs . validateInputs
        , validateWith $ contramapAndMapErr txInfoOutputs BadTxInfo'BadOutputs . validateOutputs
        , validateWith $ contramapAndMapErr txInfoRedeemers BadTxInfo'BadRedeemers . validateRedeemers
        , mapErr BadTxInfo'BadBalaning validateBalancing
        ]
