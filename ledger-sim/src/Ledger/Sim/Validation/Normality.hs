module Ledger.Sim.Validation.Normality (
    BadPOSIXTime (..),
    BadLedgerBytes'TooLong (..),
    BadLedgerBytes'UnexpectedLength (..),
    BadTokenName (..),
    BadCurrencySymbol (..),
    BadPubKeyHash (..),
    BadScriptHash (..),
    BadDatumHash (..),
    BadTxId (..),
    BadTxOutRef (..),
    BadScriptPurpose (..),
    BadCredential (..),
    BadStakingCredential (..),
    BadAddress (..),
    BadOrderedAssocMap (..),
    BadValue (..),
    BadValueType (..),
    BadAsset (..),
    BadAdaToken (..),
    BadNativeToken (..),
    BadTxOut (..),
    BadTxInInfo (..),
    BadInputList (..),
    BadInputs (..),
    BadReferenceInputs (..),
    BadOutputs (..),
    BadFee (..),
    BadMint,
    BadValidRange (..),
    BadSignatories (..),
    BadRedeemers (..),
    BadData (..),
    BadTxInfo (..),
    validateTxInfo,
) where

import Control.Monad ((>=>))
import Data.ByteString qualified as BS
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Functor.Contravariant.Divisible (Decidable (choose))
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S
import Ledger.Sim.Validation.Validator (
    Validator,
    contramapAndMapErr,
    contramapAndMapErrWithSubject,
    mapErr,
    mapErrWithSubject,
    validateBool,
    validateFail,
    validateFoldable,
    validateIf,
    validateListAndAnnotateErrWithIdx,
    validateOptional,
    validatePass,
    validateRoundtrip,
    validateWith,
 )
import PlutusLedgerApi.V2 (
    Address (addressCredential, addressStakingCredential),
    Credential (PubKeyCredential, ScriptCredential),
    CurrencySymbol (unCurrencySymbol),
    Datum,
    DatumHash (DatumHash),
    Extended (Finite),
    Interval (ivFrom, ivTo),
    LowerBound (LowerBound),
    OutputDatum (OutputDatumHash),
    POSIXTime,
    POSIXTimeRange,
    PubKeyHash (getPubKeyHash),
    Redeemer,
    ScriptHash (getScriptHash),
    ScriptPurpose (Minting, Spending),
    StakingCredential (StakingHash),
    TokenName (unTokenName),
    TxId (getTxId),
    TxInInfo (txInInfoOutRef, txInInfoResolved),
    TxInfo (
        txInfoData,
        txInfoFee,
        txInfoId,
        txInfoInputs,
        txInfoMint,
        txInfoOutputs,
        txInfoRedeemers,
        txInfoReferenceInputs,
        txInfoSignatories,
        txInfoValidRange
    ),
    TxOut (txOutAddress, txOutDatum, txOutReferenceScript, txOutValue),
    TxOutRef (txOutRefId, txOutRefIdx),
    UpperBound (UpperBound),
    Value (getValue),
    adaSymbol,
    adaToken,
 )
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as PlutusTx

--------------------------------------------------------------------------------

newtype BadPOSIXTime = BadPOSIXTime'NegativeInteger POSIXTime

validatePOSIXTime :: Validator BadPOSIXTime POSIXTime
validatePOSIXTime = validateIf (>= 0) BadPOSIXTime'NegativeInteger

--------------------------------------------------------------------------------

validateLedgerBytesLength ::
    (Int -> Bool) ->
    (Int -> err) ->
    Validator err PlutusTx.BuiltinByteString
validateLedgerBytesLength checkLength mkErr =
    contramap (BS.length . PlutusTx.fromBuiltin) $
        validateIf checkLength mkErr

data BadLedgerBytes'TooLong
    = BadLedgerBytes'TooLong Int Int

validateLedgerBytesLengthAtMost ::
    Int ->
    Validator BadLedgerBytes'TooLong PlutusTx.BuiltinByteString
validateLedgerBytesLengthAtMost limit =
    validateLedgerBytesLength
        (<= limit)
        (BadLedgerBytes'TooLong limit)

data BadLedgerBytes'UnexpectedLength
    = BadLedgerBytes'UnexpectedLength Int Int

validateLedgerBytesLengthExactly ::
    Int ->
    Validator BadLedgerBytes'UnexpectedLength PlutusTx.BuiltinByteString
validateLedgerBytesLengthExactly expected =
    validateLedgerBytesLength
        (== expected)
        (BadLedgerBytes'UnexpectedLength expected)

validateBlake2b224Hash ::
    Validator
        BadLedgerBytes'UnexpectedLength
        PlutusTx.BuiltinByteString
validateBlake2b224Hash = validateLedgerBytesLengthExactly 28

validateBlake2b256Hash ::
    Validator
        BadLedgerBytes'UnexpectedLength
        PlutusTx.BuiltinByteString
validateBlake2b256Hash = validateLedgerBytesLengthExactly 32

--------------------------------------------------------------------------------

data BadTokenName
    = BadTokenName'TooLong TokenName BadLedgerBytes'TooLong

validateTokenName :: Validator BadTokenName TokenName
validateTokenName =
    mapErrWithSubject BadTokenName'TooLong $
        contramap unTokenName $
            validateLedgerBytesLengthAtMost 32

--------------------------------------------------------------------------------

data BadCurrencySymbol
    = BadCurrencySymbol'UnexpectedLength CurrencySymbol BadLedgerBytes'UnexpectedLength

validateCurrencySymbol :: Validator BadCurrencySymbol CurrencySymbol
validateCurrencySymbol =
    mapErrWithSubject BadCurrencySymbol'UnexpectedLength $
        choose
            ( \cs ->
                if cs == adaSymbol then Left () else Right cs
            )
            validatePass
            (contramap unCurrencySymbol validateBlake2b224Hash)

--------------------------------------------------------------------------------

data BadPubKeyHash
    = BadPubKeyHash'UnexpectedLength PubKeyHash BadLedgerBytes'UnexpectedLength

validatePubKeyHash :: Validator BadPubKeyHash PubKeyHash
validatePubKeyHash =
    contramapAndMapErrWithSubject
        getPubKeyHash
        BadPubKeyHash'UnexpectedLength
        validateBlake2b224Hash

--------------------------------------------------------------------------------

data BadScriptHash
    = BadScriptHash'UnexpectedLength ScriptHash BadLedgerBytes'UnexpectedLength

validateScriptHash :: Validator BadScriptHash ScriptHash
validateScriptHash =
    contramapAndMapErrWithSubject
        getScriptHash
        BadScriptHash'UnexpectedLength
        validateBlake2b224Hash

--------------------------------------------------------------------------------

data BadDatumHash
    = BadDatumHash'UnexpectedLength DatumHash BadLedgerBytes'UnexpectedLength

validateDatumHash :: Validator BadDatumHash DatumHash
validateDatumHash =
    contramapAndMapErrWithSubject
        (\(DatumHash bs) -> bs)
        BadDatumHash'UnexpectedLength
        validateBlake2b256Hash

--------------------------------------------------------------------------------

data BadTxId = BadTxId'UnexpectedLength TxId BadLedgerBytes'UnexpectedLength

validateTxId :: Validator BadTxId TxId
validateTxId =
    contramapAndMapErrWithSubject
        getTxId
        BadTxId'UnexpectedLength
        validateBlake2b256Hash

--------------------------------------------------------------------------------

data BadTxOutRef
    = BadTxOutRef'BadTxId BadTxId
    | BadTxOutRef'NegativeIndex Integer

validateTxOutRef :: Validator BadTxOutRef TxOutRef
validateTxOutRef =
    mconcat
        [ contramapAndMapErr txOutRefId BadTxOutRef'BadTxId validateTxId
        , contramap txOutRefIdx $ validateIf (>= 0) BadTxOutRef'NegativeIndex
        ]

--------------------------------------------------------------------------------

data BadScriptPurpose
    = BadScriptPurpose'BadMinting BadCurrencySymbol
    | BadScriptPurpose'BadSpending BadTxOutRef
    | BadScriptPurpose'UnsupportedScriptPurpose ScriptPurpose

validateScriptPurpose :: Validator BadScriptPurpose ScriptPurpose
validateScriptPurpose =
    choose
        ( \case
            Minting cs -> Left cs
            s -> Right s
        )
        (mapErr BadScriptPurpose'BadMinting validateCurrencySymbol)
        $ choose
            ( \case
                Spending o -> Left o
                s -> Right s
            )
            (mapErr BadScriptPurpose'BadSpending validateTxOutRef)
            (validateWith $ validateFail . BadScriptPurpose'UnsupportedScriptPurpose)

--------------------------------------------------------------------------------

data BadCredential
    = BadCredential'BadPubKeyCredential BadPubKeyHash
    | BadCredential'BadScriptCredential BadScriptHash

validateCredential :: Validator BadCredential Credential
validateCredential =
    choose
        ( \case
            PubKeyCredential c -> Left c
            ScriptCredential c -> Right c
        )
        (mapErr BadCredential'BadPubKeyCredential validatePubKeyHash)
        (mapErr BadCredential'BadScriptCredential validateScriptHash)

--------------------------------------------------------------------------------

data BadStakingCredential
    = BadStakingCredential'BadCredential BadCredential
    | BadStakingCredential'StakingPtrUnsupported StakingCredential

validateStakingCredential :: Validator BadStakingCredential StakingCredential
validateStakingCredential =
    choose
        ( \case
            StakingHash h -> Left h
            c -> Right c
        )
        (mapErr BadStakingCredential'BadCredential validateCredential)
        ( mapErrWithSubject
            (const . BadStakingCredential'StakingPtrUnsupported)
            (validateFail ())
        )

--------------------------------------------------------------------------------

data BadAddress
    = BadAddress'BadCredential BadCredential
    | BadAddress'BadStakingCredential BadStakingCredential

validateAddress :: Validator BadAddress Address
validateAddress =
    mconcat
        [ contramapAndMapErr addressCredential BadAddress'BadCredential validateCredential
        , contramapAndMapErr addressStakingCredential BadAddress'BadStakingCredential $
            validateOptional validateStakingCredential
        ]

--------------------------------------------------------------------------------

data BadOrderedAssocMap k v e
    = BadOrderedAssocMap'NotStrictlyOrdered [k] [k]
    | BadOrderedAssocMap'BadKeyValuePair k v e

validateOrderedAssocMap ::
    (Ord k) =>
    Validator e (k, v) ->
    Validator (BadOrderedAssocMap k v e) (AssocMap.Map k v)
validateOrderedAssocMap validateEach =
    mconcat
        [ contramap AssocMap.keys $
            validateRoundtrip (S.toAscList . S.fromList) BadOrderedAssocMap'NotStrictlyOrdered
        , contramap AssocMap.toList $
            validateFoldable $
                mapErrWithSubject
                    (uncurry BadOrderedAssocMap'BadKeyValuePair)
                    validateEach
        ]

--------------------------------------------------------------------------------

data BadValue = BadValue Value BadValueType

data BadValueType
    = BadValueType'BadMap
        ( BadOrderedAssocMap
            CurrencySymbol
            (AssocMap.Map TokenName Integer)
            BadAsset
        )
    | BadValueType'AdaMissing
    | BadValueType'AdaOnly

data BadAsset
    = BadAsset'BadCurrencySymbol BadCurrencySymbol
    | BadAsset'BadAda (BadOrderedAssocMap TokenName Integer BadAdaToken)
    | BadAsset'BadNativeToken (BadOrderedAssocMap TokenName Integer BadNativeToken)

data BadAdaToken
    = BadAdaToken'TokenNameNotEmpty TokenName
    | BadAdaToken'NegativeAmount Integer
    | BadAdaToken'NonZeroAmount Integer

data BadNativeToken
    = BadNativeToken'BadTokenName BadTokenName
    | BadNativeToken'ZeroAmount
    | BadNativeToken'ZeroOrNegativeAmount Integer

data ValuePurpose
    = ValuePurpose'Output
    | ValuePurpose'Mint
    | ValuePurpose'Fee
    | ValuePurpose'Other

validateValue ::
    ValuePurpose ->
    Validator BadValue Value
validateValue purpose =
    mapErrWithSubject BadValue $
        contramap getValue $
            mconcat
                [ mapErr BadValueType'BadMap $
                    validateOrderedAssocMap $
                        choose
                            ( \(cs, tnMap) ->
                                if cs == adaSymbol
                                    then Left tnMap
                                    else Right (cs, tnMap)
                            )
                            (mapErr BadAsset'BadAda $ validateAdaTokens purpose)
                            ( mconcat
                                [ contramap fst $
                                    mapErr BadAsset'BadCurrencySymbol validateCurrencySymbol
                                , contramap snd $
                                    mapErr BadAsset'BadNativeToken $
                                        validateNativeTokenMap purpose
                                ]
                            )
                , mapErr (const BadValueType'AdaMissing) $ case purpose of
                    ValuePurpose'Other -> mempty
                    _ ->
                        validateIf
                            (isJust . (AssocMap.lookup adaSymbol >=> AssocMap.lookup adaToken))
                            $ const ()
                , mapErr (const BadValueType'AdaOnly) $ case purpose of
                    ValuePurpose'Fee ->
                        validateIf (([adaSymbol] ==) . AssocMap.keys) $ const ()
                    _ -> mempty
                ]

validateAdaTokens ::
    ValuePurpose ->
    Validator
        (BadOrderedAssocMap TokenName Integer BadAdaToken)
        (AssocMap.Map TokenName Integer)
validateAdaTokens purpose =
    validateOrderedAssocMap $
        mconcat
            [ contramap fst $ validateIf (== adaToken) BadAdaToken'TokenNameNotEmpty
            , contramap snd $
                mconcat
                    [ validateIf (>= 0) BadAdaToken'NegativeAmount
                    , case purpose of
                        ValuePurpose'Mint -> validateIf (== 0) BadAdaToken'NonZeroAmount
                        _ -> mempty
                    ]
            ]

validateNativeTokenMap ::
    ValuePurpose ->
    Validator
        (BadOrderedAssocMap TokenName Integer BadNativeToken)
        (AssocMap.Map TokenName Integer)
validateNativeTokenMap purpose =
    validateOrderedAssocMap $
        mconcat
            [ contramapAndMapErr fst BadNativeToken'BadTokenName validateTokenName
            , contramap snd $ case purpose of
                ValuePurpose'Output -> validateIf (> 0) BadNativeToken'ZeroOrNegativeAmount
                _ -> validateIf (/= 0) $ const BadNativeToken'ZeroAmount
            ]

--------------------------------------------------------------------------------

data BadTxOut
    = BadTxOut'BadAddress BadAddress
    | BadTxOut'BadValue BadValue
    | BadTxOut'BadDatumHash BadDatumHash
    | BadTxOut'BadReferenceScriptHash BadScriptHash

validateTxOut :: Validator BadTxOut TxOut
validateTxOut =
    mconcat
        [ contramapAndMapErr txOutAddress BadTxOut'BadAddress validateAddress
        , contramapAndMapErr txOutValue BadTxOut'BadValue $
            validateValue ValuePurpose'Output
        , contramapAndMapErr txOutDatum BadTxOut'BadDatumHash validateOutputDatum
        , contramapAndMapErr txOutReferenceScript BadTxOut'BadReferenceScriptHash $
            validateOptional validateScriptHash
        ]
  where
    validateOutputDatum :: Validator BadDatumHash OutputDatum
    validateOutputDatum =
        contramap
            ( \case
                OutputDatumHash h -> Just h
                _ -> Nothing
            )
            $ validateOptional validateDatumHash

--------------------------------------------------------------------------------

data BadTxInInfo
    = BadInput'BadTxOutRef BadTxOutRef
    | BadInput'BadTxOut BadTxOut

validateTxInInfo :: Validator BadTxInInfo TxInInfo
validateTxInInfo =
    mconcat
        [ contramapAndMapErr txInInfoOutRef BadInput'BadTxOutRef validateTxOutRef
        , contramapAndMapErr txInInfoResolved BadInput'BadTxOut validateTxOut
        ]

--------------------------------------------------------------------------------

data BadInputList
    = BadInputList'NotStrictlyOrdered
        [TxInInfo] -- Expected
        [TxInInfo] -- Actual
    | BadInputList'BadTxInInfo
        Int -- Index
        BadTxInInfo

validateInputList :: Validator BadInputList [TxInInfo]
validateInputList =
    mconcat
        [ validateRoundtrip
            ( fmap snd
                . M.toAscList
                . M.fromList
                . fmap (liftA2 (,) txInInfoOutRef id)
            )
            BadInputList'NotStrictlyOrdered
        , validateListAndAnnotateErrWithIdx BadInputList'BadTxInInfo validateTxInInfo
        ]

--------------------------------------------------------------------------------

data BadInputs
    = BadInputs'BadInputList BadInputList
    | BadInputs'NoInput

validateInputs :: Validator BadInputs [TxInInfo]
validateInputs =
    mconcat
        [ mapErr BadInputs'BadInputList validateInputList
        , validateIf (not . null) $ const BadInputs'NoInput
        ]

--------------------------------------------------------------------------------

newtype BadReferenceInputs = BadReferenceInputs'BadInputList BadInputList

validateReferenceInputs :: Validator BadReferenceInputs [TxInInfo]
validateReferenceInputs =
    mapErr BadReferenceInputs'BadInputList validateInputList

--------------------------------------------------------------------------------

data BadOutputs
    = BadOutputs'BadTxOut
        Int -- Index
        BadTxOut
    | BadOutputs'NoOutput

validateOutputs :: Validator BadOutputs [TxOut]
validateOutputs =
    mconcat
        [ validateListAndAnnotateErrWithIdx BadOutputs'BadTxOut validateTxOut
        , validateIf (not . null) $ const BadOutputs'NoOutput
        ]

--------------------------------------------------------------------------------

newtype BadFee = BadFee'BadValue BadValue

validateFee :: Validator BadFee Value
validateFee = mapErr BadFee'BadValue $ validateValue ValuePurpose'Fee

--------------------------------------------------------------------------------

newtype BadMint = BadMint'BadValue BadValue

validateMint :: Validator BadMint Value
validateMint = mapErr BadMint'BadValue $ validateValue ValuePurpose'Mint

--------------------------------------------------------------------------------

data BadValidRange
    = BadValidRange'BadLowerBound'BadPOSIXTime BadPOSIXTime
    | BadValidRange'BadUpperBound'BadPOSIXTime BadPOSIXTime
    | BadValidRange'BadUpperBound'CannotBeInclusive

validateValidRange :: Validator BadValidRange POSIXTimeRange
validateValidRange =
    mconcat
        [ mapErr BadValidRange'BadLowerBound'BadPOSIXTime $
            contramap
                ((\(LowerBound e _) -> e) . ivFrom)
                validateExtended
        , contramap ((\(UpperBound e c) -> (e, c)) . ivTo) $
            mconcat
                [ contramap (not . snd) $
                    validateBool BadValidRange'BadUpperBound'CannotBeInclusive
                , mapErr BadValidRange'BadUpperBound'BadPOSIXTime $
                    contramap fst validateExtended
                ]
        ]
  where
    validateExtended :: Validator BadPOSIXTime (Extended POSIXTime)
    validateExtended =
        contramap
            ( \case
                Finite p -> Just p
                _ -> Nothing
            )
            $ validateOptional validatePOSIXTime

--------------------------------------------------------------------------------

data BadSignatories
    = BadSignatories'NotStrictlyOrdered
        [PubKeyHash]
        [PubKeyHash]
    | BadSignatories'BadPubKeyHash Int BadPubKeyHash
    | BadSignatories'NoSignature

validateSignatories :: Validator BadSignatories [PubKeyHash]
validateSignatories =
    mconcat
        [ validateRoundtrip (S.toAscList . S.fromList) BadSignatories'NotStrictlyOrdered
        , validateListAndAnnotateErrWithIdx BadSignatories'BadPubKeyHash validatePubKeyHash
        , validateIf (not . null) $
            const BadSignatories'NoSignature
        ]

--------------------------------------------------------------------------------

data BadRedeemers
    = BadRedeemers'BadScriptPurpose BadScriptPurpose
    | BadRedeemers'HasDuplicateEntries
        (AssocMap.Map ScriptPurpose Redeemer)
        (AssocMap.Map ScriptPurpose Redeemer)

validateRedeemers :: Validator BadRedeemers (AssocMap.Map ScriptPurpose Redeemer)
validateRedeemers =
    mconcat
        [ contramap AssocMap.toList $
            validateFoldable $
                contramap fst $
                    mapErr BadRedeemers'BadScriptPurpose validateScriptPurpose
        , validateRoundtrip
            (AssocMap.fromListSafe . AssocMap.toList)
            BadRedeemers'HasDuplicateEntries
        ]

--------------------------------------------------------------------------------

newtype BadData
    = BadData'BadMap (BadOrderedAssocMap DatumHash Datum BadDatumHash)

validateData :: Validator BadData (AssocMap.Map DatumHash Datum)
validateData =
    mapErr BadData'BadMap $
        validateOrderedAssocMap $
            contramap fst validateDatumHash

--------------------------------------------------------------------------------

data BadTxInfo
    = BadTxInfo'BadInputs BadInputs
    | BadTxInfo'BadReferenceInputs BadReferenceInputs
    | BadTxInfo'BadOutputs BadOutputs
    | BadTxInfo'BadFee BadFee
    | BadTxInfo'BadMint BadMint
    | BadTxInfo'BadValidRange BadValidRange
    | BadTxInfo'BadSignatories BadSignatories
    | BadTxInfo'BadRedeemers BadRedeemers
    | BadTxInfo'BadData BadData
    | BadTxInfo'BadId BadTxId

validateTxInfo :: Validator BadTxInfo TxInfo
validateTxInfo =
    mconcat
        [ contramapAndMapErr txInfoInputs BadTxInfo'BadInputs validateInputs
        , contramapAndMapErr txInfoReferenceInputs BadTxInfo'BadReferenceInputs validateReferenceInputs
        , contramapAndMapErr txInfoOutputs BadTxInfo'BadOutputs validateOutputs
        , contramapAndMapErr txInfoFee BadTxInfo'BadFee validateFee
        , contramapAndMapErr txInfoMint BadTxInfo'BadMint validateMint
        , contramapAndMapErr txInfoSignatories BadTxInfo'BadSignatories validateSignatories
        , contramapAndMapErr txInfoValidRange BadTxInfo'BadValidRange validateValidRange
        , contramapAndMapErr txInfoRedeemers BadTxInfo'BadRedeemers validateRedeemers
        , contramapAndMapErr txInfoData BadTxInfo'BadData validateData
        , contramapAndMapErr txInfoId BadTxInfo'BadId validateTxId
        ]
