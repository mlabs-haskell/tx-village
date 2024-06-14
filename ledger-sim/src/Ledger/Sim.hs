module Ledger.Sim (
  LedgerSim,
  LedgerState (..),
  LedgerConfig (..),
  LedgerValidatorError (..),
  ledgerStateWithUtxos,
  emptyLedgerState,
  runLedgerSim,
  submitTx,
  incrementSlot,
  getCurrentSlot,
  lookupUTxO,
  utxosAtAddress,
  getsLedgerState,
  getLedgerState,
  asksLedgerCtx,
  askLedgerCtx,
  throwLedgerError,
) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.State.Strict (StateT (runStateT), get, gets, modify')
import Data.Bifunctor (second)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (find, fold, for_)
import Data.Functor (void)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as S
import Data.String (fromString)
import Data.Traversable (for)

import Codec.Serialise (serialise)
import Crypto.Hash (Blake2b_224 (Blake2b_224), hashWith)

import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.Common.Versions qualified as Plutus
import PlutusLedgerApi.V1.Address (toPubKeyHash, toScriptHash)
import PlutusLedgerApi.V1.Interval qualified as IV
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Address,
  CurrencySymbol (CurrencySymbol),
  Datum,
  DatumHash,
  EvaluationError,
  ExBudget,
  LogOutput,
  OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
  POSIXTime (getPOSIXTime),
  POSIXTimeRange,
  PubKeyHash,
  Redeemer,
  ScriptContext (ScriptContext),
  ScriptForEvaluation,
  ScriptHash (ScriptHash),
  ScriptPurpose (Minting, Spending),
  TokenName,
  TxId (TxId),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (
    TxInfo,
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
  TxOut (TxOut, txOutAddress, txOutDatum, txOutReferenceScript, txOutValue),
  TxOutRef (TxOutRef),
  Value,
  adaSymbol,
  adaToken,
 )
import PlutusLedgerApi.V2.Tx (isPubKeyOut)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PlutusMap
import PlutusTx.Builtins qualified as PlutusTx

import Ledger.Sim.Types.Config (
  LedgerConfig (lc'evaluationContext, lc'scriptStorage, lc'userCtx),
 )

data LedgerState st = LedgerState
  { ls'currentTime :: !POSIXTime
  , ls'utxos :: !(Map TxOutRef TxOut)
  , ls'userState :: !st
  }

ledgerStateWithUtxos :: Map TxOutRef TxOut -> st -> LedgerState st
ledgerStateWithUtxos existingUtxos userState =
  LedgerState
    { ls'currentTime = 1
    , ls'utxos = existingUtxos
    , ls'userState = userState
    }

emptyLedgerState :: st -> LedgerState st
emptyLedgerState = ledgerStateWithUtxos mempty

type LedgerSim ctx st e = StateT (LedgerState st) (LedgerValidator ctx e)

type LedgerValidator ctx e = ReaderT (LedgerConfig ctx) (Except (LedgerValidatorError e))

data LedgerValidatorError e
  = -- | The transaction is outside its validity range.
    TxInvalidRange {lve'txTime :: !POSIXTime, lve'txValidityRange :: POSIXTimeRange}
  | -- | The specified input in the transaction does not exist in the ledger utxo set.
    TxNonExistentInput {lve'txOutRef :: !TxOutRef}
  | -- | The owner of a utxo being spent has not signed this transaction.
    TxMissingOwnerSignature {lve'pubKeyHash :: !PubKeyHash}
  | -- | Transaction is not balanced.
    TxUnbalanced {lve'txInputValue :: !Value, lve'txMint :: !Value, lve'txOutputValue :: !Value}
  | -- | Spending an utxo that is missing from the inputs.
    TxMissingInputSpent {lve'txOutRef :: !TxOutRef}
  | -- | Datum for an input being spent is missing in witnesses.
    TxMissingDatum {lve'txDatumHash :: !DatumHash}
  | -- | The policy for a token in the mint field was not invoked.
    TxMissingPolicyInvocation {lve'policyId :: !CurrencySymbol}
  | -- | The validator for an utxo being spent was not invoked.
    TxMissingValidatorInvocation {lve'utxoRef :: !TxOutRef}
  | -- | A script in a reference input was not present in the script storage.
    TxMissingReferenceScript {lve'scriptHash :: !ScriptHash}
  | -- | A script being invoked was not present in the script storage.
    TxMissingScript {lve'scriptHash :: !ScriptHash}
  | -- | The input is at a script address but no datum has been provided during its creation.
    TxMissingDatumInScriptInput {lve'utxoRef :: !TxOutRef}
  | -- | A script output was not supplied with a datum.
    TxMissingDatumInScriptOutput {lve'utxo :: !TxOut}
  | -- | Tried to spend a script input located at a pub key address.
    TxNotScriptInput {lve'input :: TxInInfo}
  | TxScriptFailure {lve'scriptLogs :: LogOutput, lve'evalError :: EvaluationError}
  | -- | Stake/Reward based script purposes are not currently supported.
    TxUnsupportedScriptPurpose
  | TxNonNormalInputs {lve'expectedInputs :: ![TxInInfo], lve'actualInputs :: ![TxInInfo]}
  | TxNonNormalReferenceInputs {lve'expectedInputs :: ![TxInInfo], lve'actualInputs :: ![TxInInfo]}
  | TxNonNormalOutputValue
      { lve'expectedOutputValue :: ![(CurrencySymbol, [(TokenName, Integer)])]
      , lve'actualOutputValue :: ![(CurrencySymbol, [(TokenName, Integer)])]
      , lve'actualOutput :: !TxOut
      }
  | TxNonNormalMint
      { lve'expectedMint :: ![(CurrencySymbol, [(TokenName, Integer)])]
      , lve'actualMint :: ![(CurrencySymbol, [(TokenName, Integer)])]
      }
  | TxNonNormalFee
      { lve'expectedFee :: ![(CurrencySymbol, [(TokenName, Integer)])]
      , lve'actualFee :: ![(CurrencySymbol, [(TokenName, Integer)])]
      }
  | TxNonNormalSignatories
      { lve'expectedSignatories :: ![PubKeyHash]
      , lve'actualSignatories :: ![PubKeyHash]
      }
  | TxNonNormalRedeemers
      { lve'expectedRedeemers :: ![(ScriptPurpose, Redeemer)]
      , lve'actualRedeemers :: ![(ScriptPurpose, Redeemer)]
      }
  | TxNonNormalData
      { lve'expectedData :: ![(DatumHash, Datum)]
      , lve'actualData :: ![(DatumHash, Datum)]
      }
  | -- | Custom Application Errors.
    TxApplicationError {lve'appError :: !e}
  deriving stock (Eq, Show)

runLedgerSim :: LedgerConfig ctx -> LedgerState st -> LedgerSim ctx st e a -> Either (LedgerValidatorError e) a
runLedgerSim ledgerCfg ledgerState =
  fmap fst
    . runIdentity
    . runExceptT
    . flip runReaderT ledgerCfg
    . flip runStateT ledgerState

lookupUTxO :: TxOutRef -> LedgerSim ctx st e (Maybe TxOut)
lookupUTxO ref = M.lookup ref <$> gets ls'utxos

utxosAtAddress :: Address -> LedgerSim ctx st e [TxInInfo]
utxosAtAddress addr =
  mapMaybe
    ( \(ref, txOut) ->
        if txOutAddress txOut == addr
          then Just $ TxInInfo ref txOut
          else Nothing
    )
    . M.assocs
    <$> gets ls'utxos

{- Known shortcomings:
- Tx id must be generated and is not generated by hashing the tx body (how it works on the chain).
  This is because we don't have a real Cardano.Api TxBody available at hand and creating one from PLA would
  require a lot of effort.
  As a stopgap, Tx id is generated from current POSIX time. This may violate the assumptions of any scripts that
  use hashing inside them. ex: script that checks `txId txInfo != blake2b_224 time` where time is some time from validity range.
- See: 'checkTx'
-}
submitTx :: TxInfo -> LedgerSim ctx st e TxId
submitTx txInfoRaw = do
  currentTime <- gets ls'currentTime
  let txInfo = txInfoRaw {txInfoId = genTxId currentTime}
  ledgerState <- get
  -- TODO(chase): Find a way to utilize tx script budget.
  _txBudget <- lift $ checkTx ledgerState txInfo
  updateUtxos txInfo
  incrementSlot
  pure $ txInfoId txInfo

checkNormality :: TxInfo -> LedgerValidator ctx e ()
checkNormality
  TxInfo
    { txInfoReferenceInputs
    , txInfoRedeemers
    , txInfoSignatories
    , txInfoOutputs
    , txInfoMint
    , txInfoInputs
    , txInfoFee
    , txInfoData
    } = do
    unlessThrow TxNonNormalInputs (normalizeInputs txInfoInputs) txInfoInputs
    unlessThrow TxNonNormalInputs (normalizeInputs txInfoReferenceInputs) txInfoReferenceInputs
    unlessThrow
      TxNonNormalRedeemers
      (PlutusMap.toList . PlutusMap.fromListSafe $ PlutusMap.toList txInfoRedeemers)
      $ PlutusMap.toList txInfoRedeemers
    unlessThrow TxNonNormalSignatories (normalizeList txInfoSignatories) txInfoSignatories
    for_ txInfoOutputs $ \txOut@TxOut {txOutValue} -> do
      let expected = normalizeValue txOutValue
          actual = deconstructValue txOutValue
      unless (expected == actual)
        . throwLVE
        $ TxNonNormalOutputValue expected actual txOut
    unlessThrow TxNonNormalMint (normalizeValue txInfoMint) $ deconstructValue txInfoMint
    unlessThrow
      TxNonNormalFee
      [(adaSymbol, [(adaToken, Value.getLovelace $ Value.lovelaceValueOf txInfoFee)])]
      $ deconstructValue txInfoFee
    unlessThrow
      TxNonNormalData
      (normalizeMap txInfoData)
      $ PlutusMap.toList txInfoData
    where
      unlessThrow err expected actual =
        unless (expected == actual)
          . throwLVE
          $ err expected actual
      normalizeInputs x = (\inp -> inp {txInInfoResolved = normalizeTxOut $ txInInfoResolved inp}) <$> sortOn txInInfoOutRef x
      normalizeTxOut x =
        x
          { txOutValue =
              Value.Value
                . PlutusMap.fromList
                . fmap (second PlutusMap.fromList)
                . normalizeValue
                $ txOutValue x
          }
      normalizeList = S.toAscList . S.fromList
      normalizeMap :: (Ord k, Ord v) => PlutusMap.Map k v -> [(k, v)]
      normalizeMap = sort . PlutusMap.toList
      normalizeValue = padAda . normalizeMap . PlutusMap.mapWithKey (const normalizeMap) . Value.getValue
      deconstructValue = PlutusMap.toList . PlutusMap.mapWithKey (const PlutusMap.toList) . Value.getValue
      padAda ((cs, [(tk, amt)]) : rest)
        | cs == adaSymbol && tk == adaToken = (cs, [(tk, amt)]) : rest
      padAda rest = (adaSymbol, [(adaToken, 0)]) : rest

{- Note on missing validations - Chase

- TX size (size of cbor serialized txbody)
- Budget and size within limits
-}
checkTx :: LedgerState st -> TxInfo -> LedgerValidator ctx e ExBudget
checkTx
  LedgerState
    { ls'currentTime
    , ls'utxos
    }
  txInfo@TxInfo
    { txInfoValidRange
    , txInfoReferenceInputs
    , txInfoRedeemers
    , txInfoOutputs
    , txInfoMint
    , txInfoInputs
    , txInfoData
    , txInfoSignatories
    } = do
    checkNormality txInfo

    checkValidity
    checkUtxosExist
    checkReferenceScriptsExist
    checkSpendable
    checkScriptOutputsDatum
    checkBalance
    checkScriptsInvoked

    fmap fold . traverse evalScript $ PlutusMap.toList txInfoRedeemers
    where
      datumMap = M.fromList $ PlutusMap.toList txInfoData

      checkValidity :: LedgerValidator ctx e ()
      checkValidity = do
        unless (ls'currentTime `IV.member` txInfoValidRange)
          . throwLVE
          $ TxInvalidRange ls'currentTime txInfoValidRange

      checkUtxosExist :: LedgerValidator ctx e ()
      checkUtxosExist = do
        let inps = txInfoInputs <> txInfoReferenceInputs
        unlessExists TxNonExistentInput (M.keysSet ls'utxos) $ txInInfoOutRef <$> inps

      checkReferenceScriptsExist :: LedgerValidator ctx e ()
      checkReferenceScriptsExist = do
        scriptStorage <- asks lc'scriptStorage
        unlessExists
          TxMissingReferenceScript
          (M.keysSet scriptStorage)
          $ mapMaybe (txOutReferenceScript . txInInfoResolved) txInfoReferenceInputs

      -- \| Newly created outputs at script address should contain a datum.
      -- Though this is not formally checked by the real ledger.
      checkScriptOutputsDatum :: LedgerValidator ctx e ()
      checkScriptOutputsDatum = do
        void . for txInfoOutputs $ \txOut ->
          unless (isPubKeyOut txOut || txOutDatum txOut /= NoOutputDatum)
            . throwLVE
            $ TxMissingDatumInScriptOutput txOut

      checkSpendable :: LedgerValidator ctx e ()
      checkSpendable = do
        let keys = S.fromList txInfoSignatories
        unlessExists TxMissingOwnerSignature keys $ mapMaybe (toPubKeyHash . txOutAddress . txInInfoResolved) txInfoInputs

      checkBalance :: LedgerValidator ctx e ()
      checkBalance = do
        let inpValue = foldMap (txOutValue . txInInfoResolved) txInfoInputs
            outValue = foldMap txOutValue txInfoOutputs
        unless (inpValue <> txInfoMint == outValue)
          . throwLVE
          $ TxUnbalanced inpValue txInfoMint outValue

      checkScriptsInvoked :: LedgerValidator ctx e ()
      checkScriptsInvoked = do
        let invokedScripts = PlutusMap.keys txInfoRedeemers
            invokedPolicies = S.fromList . flip mapMaybe invokedScripts $ \case
              Minting cs -> Just cs
              _ -> Nothing
            invokedValidatorRefs = S.fromList . flip mapMaybe invokedScripts $ \case
              Spending ref -> Just ref
              _ -> Nothing
        unlessExists TxMissingPolicyInvocation invokedPolicies . filter (/= fromString "") $ Value.symbols txInfoMint
        unlessExists TxMissingValidatorInvocation invokedValidatorRefs . map txInInfoOutRef $
          filter (isJust . toScriptHash . txOutAddress . txInInfoResolved) txInfoInputs

      getScript :: ScriptPurpose -> LedgerValidator ctx e (ScriptForEvaluation, Maybe Datum)
      getScript (Minting (CurrencySymbol (ScriptHash -> sh))) = do
        script <- lookupScript sh
        pure (script, Nothing)
      getScript (Spending ref) = do
        (txInInfoResolved -> utxo) <-
          maybe (throwLVE $ TxMissingInputSpent ref) pure $
            find (\TxInInfo {txInInfoOutRef} -> txInInfoOutRef == ref) txInfoInputs
        sh <-
          maybe
            (throwLVE $ TxNotScriptInput {lve'input = TxInInfo ref utxo})
            pure
            . toScriptHash
            $ txOutAddress utxo
        script <- lookupScript sh
        datm <- case txOutDatum utxo of
          OutputDatumHash dh -> maybe (throwLVE $ TxMissingDatum dh) pure $ dh `M.lookup` datumMap
          OutputDatum datm -> pure datm
          NoOutputDatum -> throwLVE $ TxMissingDatumInScriptInput ref
        pure (script, Just datm)
      getScript _ = throwLVE TxUnsupportedScriptPurpose

      evalScript :: (ScriptPurpose, Redeemer) -> LedgerValidator ctx e ExBudget
      evalScript (purpose, rdmr) = do
        (script, mDatm) <- getScript purpose
        evalCtx <- asks lc'evaluationContext
        let (logs, res) =
              Plutus.evaluateScriptCounting
                Plutus.PlutusV2
                Plutus.vasilPV
                Plutus.Verbose
                evalCtx
                script
                $ maybeToList (PlutusTx.toData <$> mDatm)
                  <> [PlutusTx.toData rdmr, PlutusTx.toData $ ScriptContext txInfo purpose]
        case res of
          Left err -> throwLVE $ TxScriptFailure logs err
          Right budget -> pure budget

      lookupScript :: ScriptHash -> LedgerValidator ctx e ScriptForEvaluation
      lookupScript sh = do
        scriptStorage <- asks lc'scriptStorage
        maybe (throwLVE $ TxMissingScript sh) pure $ sh `M.lookup` scriptStorage

updateUtxos :: TxInfo -> LedgerSim ctx st e ()
updateUtxos TxInfo {txInfoId, txInfoInputs, txInfoOutputs} = do
  -- Remove spent utxos.
  modify' $ \st -> st {ls'utxos = M.withoutKeys (ls'utxos st) . S.fromList $ map txInInfoOutRef txInfoInputs}
  -- Add newly created utxos.
  modify' $ \st ->
    st
      { ls'utxos =
          M.fromList (zipWith (\ix utxo -> (TxOutRef txInfoId ix, utxo)) [0 ..] txInfoOutputs) <> ls'utxos st
      }

incrementSlot :: LedgerSim ctx st e ()
incrementSlot = do
  modify' $ \st -> st {ls'currentTime = ls'currentTime st + 1}

getCurrentSlot :: LedgerSim ctx st e POSIXTime
getCurrentSlot = gets ls'currentTime

-- | Make sure every `k` in `f k` exists in the given set.
unlessExists :: (Traversable f, Ord k) => (k -> LedgerValidatorError e) -> Set k -> f k -> LedgerValidator ctx e ()
unlessExists errF m l = void . for l $ \k -> unless (k `S.member` m) . throwLVE $ errF k

throwLVE :: LedgerValidatorError e -> LedgerValidator ctx e a
throwLVE = lift . throwE

-- | Get a specific component of the user state from the ledger, using given projection function.
getsLedgerState :: (st -> a) -> LedgerSim ctx st e a
getsLedgerState f = gets $ f . ls'userState

-- | Get the user state from the ledger.
getLedgerState :: LedgerSim ctx st e st
getLedgerState = getsLedgerState id

-- | Get a specific component of the user state from the ledger, using given projection function.
asksLedgerCtx :: (ctx -> a) -> LedgerSim ctx st e a
asksLedgerCtx f = lift . asks $ f . lc'userCtx

-- | Get the user state from the ledger.
askLedgerCtx :: LedgerSim ctx st e ctx
askLedgerCtx = asksLedgerCtx id

-- | Throw custom application error.
throwLedgerError :: e -> LedgerSim ctx st e a
throwLedgerError = lift . lift . throwE . TxApplicationError

{- | Generate tx id from time. NOTE: This is simply a stopgap measure used in the simulator. In reality,
tx ids are hashes of a transaction body.
-}
genTxId :: POSIXTime -> TxId
genTxId =
  TxId
    . PlutusTx.toBuiltin
    . convert @_ @ByteString
    . hashWith Blake2b_224
    . LBS.toStrict
    . serialise
    . getPOSIXTime
