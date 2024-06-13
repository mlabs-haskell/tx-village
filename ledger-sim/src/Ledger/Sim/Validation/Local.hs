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
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (Decidable (choose))
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Ledger.Sim.Validation.Validator (
    InContext (InContext, getContext, getSubject),
    Validator,
    contramapAndMapErr,
    itemsInContext,
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

validateInputSpendable :: Validator BadUnspendableInput (TxOut `InContext` TxInfo)
validateInputSpendable =
    choose
        ( join $
            getSubject
                >>> txOutAddress
                >>> addressCredential
                >>> \case
                    PubKeyCredential pkh -> Left . (pkh,) . txInfoSignatories . getContext
                    ScriptCredential _ -> Right . bimap txOutDatum txInfoData
        )
        ( validateIf
            (uncurry L.elem)
            (BadUnspendableInput'PubKeyInput'NotAuthorized . fst)
        )
        $ choose
            ( join $
                getSubject >>> \case
                    OutputDatumHash h -> Left . (h,) . getContext
                    d -> Right . const d
            )
            ( validateIf
                (uncurry AssocMap.member)
                (fst >>> BadUnspendableInput'ScriptInput'DatumWitnessMissing)
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

validateInputs :: Validator BadInputs ([TxInInfo] `InContext` TxInfo)
validateInputs =
    contramap (itemsInContext . first (fmap txInInfoResolved)) $
        validateListAndAnnotateErrWithIdx BadInputs'BadUnspendableInput validateInputSpendable

--------------------------------------------------------------------------------

data BadOutput = BadOutput'ScriptOutputNoDatum

validateOutput :: Validator BadOutput (TxOut `InContext` TxInfo)
validateOutput =
    choose
        ( getSubject >>> txOutDatum >>> \case
            NoOutputDatum -> Left ()
            _ -> Right ()
        )
        (validateFail BadOutput'ScriptOutputNoDatum)
        validatePass

--------------------------------------------------------------------------------

data BadOutputs = BadOutputs'BadOutput Int BadOutput

validateOutputs :: Validator BadOutputs ([TxOut] `InContext` TxInfo)
validateOutputs =
    contramap itemsInContext $
        validateListAndAnnotateErrWithIdx BadOutputs'BadOutput validateOutput

--------------------------------------------------------------------------------

data BadRedeemers
    = BadRedeemers'ValidatorNotRun ScriptHash TxOutRef
    | BadRedeemers'MintingPolicyNotRan CurrencySymbol
    | BadRedeemers'UnexpectedExcessEntries [ScriptPurpose]

validateRedeemers ::
    Validator
        BadRedeemers
        (AssocMap.Map ScriptPurpose Redeemer `InContext` TxInfo)
validateRedeemers = validateWith $ \c ->
    let isPurposeInMap = flip AssocMap.member $ getSubject c

        validatorsToRun =
            mapMaybe
                ( join $
                    txInInfoResolved >>> txOutAddress >>> addressCredential >>> \case
                        ScriptCredential sh -> Just . (sh,) . txInInfoOutRef
                        _ -> const Nothing
                )
                $ txInfoInputs
                $ getContext c

        mintingPoliciesToRun =
            filter (/= adaSymbol) $
                AssocMap.keys $
                    getValue $
                        txInfoMint $
                            getContext c

        excessEntries =
            AssocMap.keys $
                foldr
                    AssocMap.delete
                    (getSubject c)
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
        [ contramapAndMapErr (InContext =<< txInfoInputs) BadTxInfo'BadInputs validateInputs
        , contramapAndMapErr (InContext =<< txInfoOutputs) BadTxInfo'BadOutputs validateOutputs
        , contramapAndMapErr (InContext =<< txInfoRedeemers) BadTxInfo'BadRedeemers validateRedeemers
        , mapErr BadTxInfo'BadBalaning validateBalancing
        ]
