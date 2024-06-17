import Control.Exception (throwIO)
import Control.Monad.State (gets)
import Crypto.Hash (
  Blake2b_224 (Blake2b_224),
  Blake2b_256 (Blake2b_256),
  HashAlgorithm,
  hashWith,
 )
import Data.Bifunctor (first)
import Data.Binary qualified as B
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Functor (void)
import Data.Map.Strict qualified as M
import Data.String (fromString)
import Ledger.Sim (
  LedgerSimError (LedgerSimError'SubmissionError),
  getCurrentSlot,
  lookupUTxO,
  submitTx,
  throwLedgerError,
 )
import Ledger.Sim.Submission (SubmissionError (SubmissionError'ValidationFailure))
import Ledger.Sim.Test (
  ledgerFailsBy,
  ledgerSucceeds,
  ledgerTestCase,
  ledgerTestGroup,
 )
import Ledger.Sim.Types.Config (
  LedgerConfig,
  PlutusCostModel (
    PlutusCostModel
  ),
  mkLedgerConfig,
 )
import Ledger.Sim.Types.State (
  LedgerState (ls'currentTime),
  ledgerStateWithUtxos,
 )
import Ledger.Sim.Utils.Hashing (hashDatum, hashScriptV2)
import Ledger.Sim.Validation (
  InvalidTxInfoError (
    InvalidTxInfoError'Local,
    InvalidTxInfoError'Normality,
    InvalidTxInfoError'Stateful
  ),
 )
import Ledger.Sim.Validation.Local qualified as Local
import Ledger.Sim.Validation.Normality qualified as Normality
import Ledger.Sim.Validation.Stateful qualified as Stateful
import PlutusLedgerApi.Common.Versions (vasilPV)
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Interval qualified as IV
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 (
  Address (Address),
  BuiltinByteString,
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol (CurrencySymbol),
  Datum (Datum),
  OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash),
  POSIXTime,
  POSIXTimeRange,
  PubKeyHash (PubKeyHash),
  Redeemer (Redeemer),
  ScriptHash (ScriptHash, getScriptHash),
  ScriptPurpose (Minting, Spending),
  StakingCredential (StakingHash),
  ToData (toBuiltinData),
  TokenName (TokenName),
  TxId (TxId),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (TxInfo),
  TxOut (TxOut, txOutAddress, txOutDatum, txOutReferenceScript, txOutValue),
  TxOutRef (TxOutRef),
  deserialiseScript,
  toBuiltin,
 )
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusTx.AssocMap qualified as AssocMap
import Test.Tasty (TestTree, defaultMain)

main :: IO ()
main = do
  script <- either error pure . first show . deserialiseScript vasilPV $ SBS.toShort alwaysSucceedsCbor
  let sh = hashScriptV2 script
  ledgerCfg <- either throwIO pure $ mkLedgerConfig (M.fromList [(sh, script)]) testCostModel ()
  defaultMain $ tests sh ledgerCfg

tests :: ScriptHash -> LedgerConfig () -> TestTree
tests dummyScriptHash ledgerCfg =
  ledgerTestGroup
    ledgerCfg
    (ledgerStateWithUtxos (liftA2 M.singleton txInInfoOutRef txInInfoResolved dummyInput) ())
    "Tests"
    [ ledgerTestCase "Simple Minting Tx" $
        ledgerSucceeds @() $ do
          let cs = Value.CurrencySymbol $ getScriptHash dummyScriptHash
              mintVal =
                Value.assetClassValue
                  (Value.AssetClass (cs, fromString "A"))
                  1
          currentTime <- gets ls'currentTime
          void . submitTx $
            TxInfo
              [dummyInput]
              mempty
              [ TxOut
                  { txOutValue = Value.lovelaceValue 0 <> mintVal
                  , txOutReferenceScript = Nothing
                  , txOutDatum = NoOutputDatum
                  , txOutAddress = ownAddress
                  }
              ]
              (Value.lovelaceValue 0)
              (Value.lovelaceValue 0 <> mintVal)
              mempty
              AssocMap.empty
              (intervalValidRange currentTime (currentTime + 1))
              [ownPubKeyHash]
              (AssocMap.fromList [(Minting cs, Redeemer (toBuiltinData @Integer 1234))])
              AssocMap.empty
              dummyTxId
    , ledgerTestCase "Simple spending tx" $
        ledgerSucceeds $ do
          let datum = Datum $ toBuiltinData ()
              datumHash = hashDatum . Datum $ toBuiltinData ()
          currentTime <- getCurrentSlot
          txId <-
            submitTx $
              TxInfo
                [dummyInput]
                mempty
                [ TxOut
                    { txOutValue = Value.lovelaceValue 0
                    , txOutReferenceScript = Nothing
                    , txOutDatum = OutputDatumHash datumHash
                    , txOutAddress = scriptHashAddress dummyScriptHash
                    }
                ]
                (Value.lovelaceValue 0)
                (Value.lovelaceValue 0)
                mempty
                AssocMap.empty
                (intervalValidRange currentTime (currentTime + 1))
                [ownPubKeyHash]
                AssocMap.empty
                AssocMap.empty
                dummyTxId
          let newUTxORef = TxOutRef txId 0
          currentTime' <- getCurrentSlot
          newUTxO <-
            lookupUTxO newUTxORef >>= \case
              Just x -> pure x
              Nothing -> throwLedgerError "Newly created utxo absent from ledger state"
          void . submitTx $
            TxInfo
              [TxInInfo newUTxORef newUTxO]
              mempty
              [ TxOut
                  { txOutValue = Value.lovelaceValue 0
                  , txOutReferenceScript = Nothing
                  , txOutDatum = NoOutputDatum
                  , txOutAddress = ownAddress
                  }
              ]
              (Value.lovelaceValue 0)
              (Value.lovelaceValue 0)
              mempty
              AssocMap.empty
              (intervalValidRange currentTime (currentTime' + 1))
              [ownPubKeyHash]
              (AssocMap.fromList [(Spending newUTxORef, Redeemer $ toBuiltinData ())])
              (AssocMap.fromList [(datumHash, datum)])
              dummyTxId
    , ledgerTestCase "Invalid range"
        $ ledgerFailsBy @()
          ( \case
              LedgerSimError'SubmissionError
                ( SubmissionError'ValidationFailure
                    [ InvalidTxInfoError'Stateful
                        ( Stateful.InvalidTxInfoError'InvalidValidRange
                            (Stateful.InvalidValidRangeError'CurrentTimeOutOfRange _)
                          )
                      ]
                  ) -> True
              _ -> False
          )
        $ do
          currentTime <- gets ls'currentTime
          void . submitTx $
            TxInfo
              [dummyInput]
              mempty
              [ TxOut
                  { txOutValue = Value.lovelaceValue 0
                  , txOutReferenceScript = Nothing
                  , txOutDatum = NoOutputDatum
                  , txOutAddress = ownAddress
                  }
              ]
              (Value.lovelaceValue 0)
              (Value.lovelaceValue 0)
              mempty
              AssocMap.empty
              (intervalValidRange (currentTime + 42) (currentTime + 4242))
              [ownPubKeyHash]
              AssocMap.empty
              AssocMap.empty
              dummyTxId
    , ledgerTestCase "Non existent input" $
        let nonexistentInputRef = TxOutRef dummyTxId' 1
         in ledgerFailsBy @()
              ( \case
                  LedgerSimError'SubmissionError
                    ( SubmissionError'ValidationFailure
                        [ InvalidTxInfoError'Stateful
                            ( Stateful.InvalidTxInfoError'InvalidInputs
                                ( Stateful.InvalidInputsError'InvalidInput
                                    0
                                    (Stateful.InvalidInputError'NotFoundOnLedger txOutRef)
                                  )
                              )
                          ]
                      ) -> nonexistentInputRef == txOutRef
                  _ -> False
              )
              $ void . submitTx
              $ TxInfo
                [ TxInInfo
                    (TxOutRef dummyTxId' 1)
                    $ TxOut
                      { txOutValue = Value.lovelaceValue 0
                      , txOutReferenceScript = Nothing
                      , txOutDatum = NoOutputDatum
                      , txOutAddress = ownAddress
                      }
                ]
                mempty
                [ TxOut
                    { txOutValue = Value.lovelaceValue 0
                    , txOutReferenceScript = Nothing
                    , txOutDatum = NoOutputDatum
                    , txOutAddress = ownAddress
                    }
                ]
                (Value.lovelaceValue 0)
                (Value.lovelaceValue 0)
                mempty
                AssocMap.empty
                IV.always
                [ownPubKeyHash]
                AssocMap.empty
                AssocMap.empty
                dummyTxId
    , ledgerTestCase "Missing signature"
        $ ledgerFailsBy
          ( \case
              LedgerSimError'SubmissionError
                ( SubmissionError'ValidationFailure
                    [ InvalidTxInfoError'Normality
                        ( Normality.InvalidTxInfoError'InvalidSignatories
                            Normality.InvalidSignatoriesError'NoSignature
                          )
                      , InvalidTxInfoError'Local
                          ( Local.InvalidTxInfoError'InvalidInputs
                              ( Local.InvalidInputsError'UnspendableInput
                                  0
                                  (Local.UnspendableInputError'PubKeyInput'NotAuthorized pkh)
                                )
                            )
                      ]
                  ) -> pkh == ownPubKeyHash
              _ -> False
          )
        $ do
          let
            cs = Value.CurrencySymbol $ getScriptHash dummyScriptHash
            mintVal =
              Value.assetClassValue
                (Value.AssetClass (cs, fromString "A"))
                1
          txId <-
            submitTx $
              TxInfo
                [dummyInput]
                mempty
                [ TxOut
                    { txOutValue = Value.lovelaceValue 0 <> mintVal
                    , txOutReferenceScript = Nothing
                    , txOutDatum = NoOutputDatum
                    , txOutAddress = ownAddress
                    }
                ]
                (Value.lovelaceValue 0)
                (Value.lovelaceValue 0 <> mintVal)
                mempty
                AssocMap.empty
                IV.always
                [ownPubKeyHash]
                (AssocMap.fromList [(Minting cs, Redeemer (toBuiltinData @Integer 1234))])
                AssocMap.empty
                dummyTxId
          let newUTxORef = TxOutRef txId 0
          newUTxO <-
            lookupUTxO newUTxORef >>= \case
              Just x -> pure x
              Nothing -> throwLedgerError "Newly created utxo absent from ledger state"
          void . submitTx $
            TxInfo
              [TxInInfo newUTxORef newUTxO]
              mempty
              [ TxOut
                  { txOutValue = Value.lovelaceValue 0 <> mintVal
                  , txOutReferenceScript = Nothing
                  , txOutDatum = NoOutputDatum
                  , txOutAddress = ownAddress
                  }
              ]
              (Value.lovelaceValue 0)
              (Value.lovelaceValue 0)
              mempty
              AssocMap.empty
              IV.always
              mempty
              AssocMap.empty
              AssocMap.empty
              dummyTxId
    , ledgerTestCase "Unbalanced"
        $ ledgerFailsBy @()
          ( \case
              LedgerSimError'SubmissionError
                ( SubmissionError'ValidationFailure
                    [ InvalidTxInfoError'Local
                        (Local.InvalidTxInfoError'NotBalanced _) -- TODO(chfanghr): Assert delta
                      ]
                  ) -> True
              _ -> False
          )
        $ let cs = Value.CurrencySymbol $ getScriptHash dummyScriptHash
              mintVal =
                Value.assetClassValue
                  (Value.AssetClass (cs, fromString "A"))
                  1
           in void . submitTx $
                TxInfo
                  [dummyInput]
                  mempty
                  [ TxOut
                      { txOutValue = Value.lovelaceValue 0 <> mintVal
                      , txOutReferenceScript = Nothing
                      , txOutDatum = NoOutputDatum
                      , txOutAddress = ownAddress
                      }
                  ]
                  (Value.lovelaceValue 0)
                  (Value.lovelaceValue 0)
                  mempty
                  AssocMap.empty
                  IV.always
                  [ownPubKeyHash]
                  AssocMap.empty
                  AssocMap.empty
                  dummyTxId
    , ledgerTestCase "Missing Input Spent" $
        let ref = TxOutRef dummyTxId' 1
         in ledgerFailsBy @()
              ( \case
                  LedgerSimError'SubmissionError
                    ( SubmissionError'ValidationFailure
                        [ InvalidTxInfoError'Local
                            ( Local.InvalidTxInfoError'InvalidRedeemers
                                ( Local.InvalidRedeemers'UnexpectedExcessEntries
                                    [Spending txOutRef]
                                  )
                              )
                          ]
                      ) -> txOutRef == ref
                  _ -> False
              )
              $ void . submitTx
              $ TxInfo
                [dummyInput]
                mempty
                [ TxOut
                    { txOutValue = Value.lovelaceValue 0
                    , txOutReferenceScript = Nothing
                    , txOutDatum = NoOutputDatum
                    , txOutAddress = ownAddress
                    }
                ]
                (Value.lovelaceValue 0)
                (Value.lovelaceValue 0)
                mempty
                AssocMap.empty
                IV.always
                [ownPubKeyHash]
                (AssocMap.fromList [(Spending ref, Redeemer $ toBuiltinData ())])
                AssocMap.empty
                dummyTxId
    , ledgerTestCase "Missing Datum Witness" $
        let dummyDatum = Datum $ toBuiltinData ()
            dummyDatumHash = hashDatum dummyDatum
         in ledgerFailsBy
              ( \case
                  LedgerSimError'SubmissionError
                    ( SubmissionError'ValidationFailure
                        [ InvalidTxInfoError'Local
                            ( Local.InvalidTxInfoError'InvalidInputs
                                ( Local.InvalidInputsError'UnspendableInput
                                    0
                                    (Local.UnspendableInputError'ScriptInput'DatumWitnessMissing datumHash)
                                  )
                              )
                          ]
                      ) -> datumHash == dummyDatumHash
                  _ -> False
              )
              $ do
                txId <-
                  submitTx $
                    TxInfo
                      [dummyInput]
                      mempty
                      [ TxOut
                          { txOutValue = Value.lovelaceValue 0
                          , txOutReferenceScript = Nothing
                          , txOutDatum = OutputDatumHash dummyDatumHash
                          , txOutAddress = scriptHashAddress dummyScriptHash
                          }
                      ]
                      (Value.lovelaceValue 0)
                      (Value.lovelaceValue 0)
                      mempty
                      AssocMap.empty
                      IV.always
                      [ownPubKeyHash]
                      AssocMap.empty
                      AssocMap.empty
                      dummyTxId
                let newUTxORef = TxOutRef txId 0
                newUTxO <-
                  lookupUTxO newUTxORef >>= \case
                    Just x -> pure x
                    Nothing -> throwLedgerError "Newly created utxo absent from ledger state"
                void . submitTx $
                  TxInfo
                    [TxInInfo newUTxORef newUTxO]
                    mempty
                    [ TxOut
                        { txOutValue = Value.lovelaceValue 0
                        , txOutReferenceScript = Nothing
                        , txOutDatum = NoOutputDatum
                        , txOutAddress = ownAddress
                        }
                    ]
                    (Value.lovelaceValue 0)
                    (Value.lovelaceValue 0)
                    mempty
                    AssocMap.empty
                    IV.always
                    [ownPubKeyHash]
                    (AssocMap.fromList [(Spending newUTxORef, Redeemer $ toBuiltinData ())])
                    AssocMap.empty
                    dummyTxId
    , ledgerTestCase "Missing Policy Invocation"
        $ ledgerFailsBy @()
          ( \case
              LedgerSimError'SubmissionError
                ( SubmissionError'ValidationFailure
                    [ InvalidTxInfoError'Local
                        ( Local.InvalidTxInfoError'InvalidRedeemers
                            (Local.InvalidRedeemers'MintingPolicyNotRan (CurrencySymbol scriptHash))
                          )
                      ]
                  ) -> getScriptHash dummyScriptHash == scriptHash
              _ -> False
          )
        $ do
          let cs = Value.CurrencySymbol $ getScriptHash dummyScriptHash
              mintVal =
                Value.assetClassValue
                  (Value.AssetClass (cs, fromString "A"))
                  1
          void . submitTx $
            TxInfo
              [dummyInput]
              mempty
              [ TxOut
                  { txOutValue = Value.lovelaceValue 0 <> mintVal
                  , txOutReferenceScript = Nothing
                  , txOutDatum = NoOutputDatum
                  , txOutAddress = ownAddress
                  }
              ]
              (Value.lovelaceValue 0)
              (Value.lovelaceValue 0 <> mintVal)
              mempty
              AssocMap.empty
              IV.always
              [ownPubKeyHash]
              AssocMap.empty
              AssocMap.empty
              dummyTxId
    , ledgerTestCase "Missing Validator Invocation"
        $ ledgerFailsBy
          ( \case
              LedgerSimError'SubmissionError
                ( SubmissionError'ValidationFailure
                    [ InvalidTxInfoError'Local
                        ( Local.InvalidTxInfoError'InvalidRedeemers
                            (Local.InvalidRedeemers'ValidatorNotRun scriptHash txOutRef)
                          )
                      ]
                  ) -> scriptHash == dummyScriptHash && txOutRef == TxOutRef dummyTxId 0
              _ -> False
          )
        $ do
          let datum = Datum $ toBuiltinData ()
              datumHash = hashDatum . Datum $ toBuiltinData ()
          txId <-
            submitTx $
              TxInfo
                [dummyInput]
                mempty
                [ TxOut
                    { txOutValue = Value.lovelaceValue 0
                    , txOutReferenceScript = Nothing
                    , txOutDatum = OutputDatumHash datumHash
                    , txOutAddress = scriptHashAddress dummyScriptHash
                    }
                ]
                (Value.lovelaceValue 0)
                (Value.lovelaceValue 0)
                mempty
                AssocMap.empty
                IV.always
                [ownPubKeyHash]
                AssocMap.empty
                AssocMap.empty
                dummyTxId
          let newUTxORef = TxOutRef txId 0
          newUTxO <-
            lookupUTxO newUTxORef >>= \case
              Just x -> pure x
              Nothing -> throwLedgerError "Newly created utxo absent from ledger state"
          void . submitTx $
            TxInfo
              [TxInInfo newUTxORef newUTxO]
              mempty
              [ TxOut
                  { txOutValue = Value.lovelaceValue 0
                  , txOutReferenceScript = Nothing
                  , txOutDatum = NoOutputDatum
                  , txOutAddress = ownAddress
                  }
              ]
              (Value.lovelaceValue 0)
              (Value.lovelaceValue 0)
              mempty
              AssocMap.empty
              IV.always
              [ownPubKeyHash]
              AssocMap.empty
              (AssocMap.fromList [(datumHash, datum)])
              dummyTxId
    , ledgerTestCase "Missing Datum in Script output"
        $ ledgerFailsBy @()
          ( \case
              LedgerSimError'SubmissionError
                ( SubmissionError'ValidationFailure
                    [ InvalidTxInfoError'Local
                        ( Local.InvalidTxInfoError'InvalidOutputs
                            ( Local.InvalidOutputsError'InvalidOutput
                                0
                                Local.InvalidOutputError'ScriptOutputNoDatum
                              )
                          )
                      ]
                  ) -> True
              _ -> False
          )
        $ do
          void . submitTx $
            TxInfo
              [dummyInput]
              mempty
              [ TxOut
                  { txOutValue = Value.lovelaceValue 0
                  , txOutReferenceScript = Nothing
                  , txOutDatum = NoOutputDatum
                  , txOutAddress = scriptHashAddress dummyScriptHash
                  }
              ]
              (Value.lovelaceValue 0)
              (Value.lovelaceValue 0)
              mempty
              AssocMap.empty
              IV.always
              [ownPubKeyHash]
              AssocMap.empty
              AssocMap.empty
              dummyTxId
    , ledgerTestCase "Not script input"
        $ ledgerFailsBy
          ( \case
              LedgerSimError'SubmissionError
                ( SubmissionError'ValidationFailure
                    [ InvalidTxInfoError'Local
                        ( Local.InvalidTxInfoError'InvalidRedeemers
                            ( Local.InvalidRedeemers'UnexpectedExcessEntries
                                [Spending txOutRef]
                              )
                          )
                      ]
                  ) -> txOutRef == TxOutRef dummyTxId 0
              _ -> False
          )
        $ do
          let
            cs = Value.CurrencySymbol $ getScriptHash dummyScriptHash
            mintVal =
              Value.assetClassValue
                (Value.AssetClass (cs, fromString "A"))
                1
          txId <-
            submitTx $
              TxInfo
                [dummyInput]
                mempty
                [ TxOut
                    { txOutValue = Value.lovelaceValue 0 <> mintVal
                    , txOutReferenceScript = Nothing
                    , txOutDatum = NoOutputDatum
                    , txOutAddress = ownAddress
                    }
                ]
                (Value.lovelaceValue 0)
                (Value.lovelaceValue 0 <> mintVal)
                mempty
                AssocMap.empty
                IV.always
                [ownPubKeyHash]
                (AssocMap.fromList [(Minting cs, Redeemer (toBuiltinData @Integer 1234))])
                AssocMap.empty
                dummyTxId
          let newUTxORef = TxOutRef txId 0
          newUTxO <-
            lookupUTxO newUTxORef >>= \case
              Just x -> pure x
              Nothing -> throwLedgerError "Newly created utxo absent from ledger state"
          void . submitTx $
            TxInfo
              [TxInInfo newUTxORef newUTxO]
              mempty
              [ TxOut
                  { txOutValue = Value.lovelaceValue 0 <> mintVal
                  , txOutReferenceScript = Nothing
                  , txOutDatum = NoOutputDatum
                  , txOutAddress = ownAddress
                  }
              ]
              (Value.lovelaceValue 0)
              (Value.lovelaceValue 0)
              mempty
              AssocMap.empty
              IV.always
              [ownPubKeyHash]
              (AssocMap.fromList [(Spending newUTxORef, Redeemer $ toBuiltinData ())])
              AssocMap.empty
              dummyTxId
    , ledgerTestCase "Non normal mint"
        $ ledgerFailsBy @()
          ( \case
              LedgerSimError'SubmissionError
                ( SubmissionError'ValidationFailure
                    [ InvalidTxInfoError'Normality
                        ( Normality.InvalidTxInfoError'InvalidMint
                            ( Normality.InvalidMintError'InvalidValue
                                ( Normality.InvalidValue
                                    _
                                    Normality.InvalidValueKind'AdaMissing
                                  )
                              )
                          )
                      ]
                  ) -> True
              _ -> False
          )
        $ do
          let
            cs = Value.CurrencySymbol $ getScriptHash dummyScriptHash
            mintVal =
              Value.assetClassValue
                (Value.AssetClass (cs, fromString "A"))
                1
          void . submitTx $
            TxInfo
              [dummyInput]
              mempty
              [ TxOut
                  { txOutValue = Value.lovelaceValue 0 <> mintVal
                  , txOutReferenceScript = Nothing
                  , txOutDatum = NoOutputDatum
                  , txOutAddress = ownAddress
                  }
              ]
              (Value.lovelaceValue 0)
              mintVal
              mempty
              AssocMap.empty
              IV.always
              [ownPubKeyHash]
              (AssocMap.fromList [(Minting cs, Redeemer (toBuiltinData @Integer 1234))])
              AssocMap.empty
              dummyTxId
    , ledgerTestCase "Non normal output value"
        $ ledgerFailsBy @()
          ( \case
              LedgerSimError'SubmissionError
                ( SubmissionError'ValidationFailure
                    [ InvalidTxInfoError'Normality
                        ( Normality.InvalidTxInfoError'InvalidOutputs
                            ( Normality.InvalidOutputsError'InvalidTxOut
                                0
                                ( Normality.InvalidTxOutError'InvalidValue
                                    ( Normality.InvalidValue
                                        _
                                        Normality.InvalidValueKind'AdaMissing
                                      )
                                  )
                              )
                          )
                      ]
                  ) -> True
              _ -> False
          )
        $ let
            cs = Value.CurrencySymbol $ getScriptHash dummyScriptHash
            mintVal =
              Value.assetClassValue
                (Value.AssetClass (cs, fromString "A"))
                1
           in
            void . submitTx $
              TxInfo
                [dummyInput]
                mempty
                [ TxOut
                    { txOutValue = mintVal
                    , txOutReferenceScript = Nothing
                    , txOutDatum = NoOutputDatum
                    , txOutAddress = ownAddress
                    }
                ]
                (Value.lovelaceValue 0)
                (Value.lovelaceValue 0 <> mintVal)
                mempty
                AssocMap.empty
                IV.always
                [ownPubKeyHash]
                (AssocMap.fromList [(Minting cs, Redeemer (toBuiltinData @Integer 1234))])
                AssocMap.empty
                dummyTxId
    , ledgerTestCase "Invalid currency symbol/token name/address" $
        let
          invalidCurrencySymbol = CurrencySymbol $ toBuiltin $ BS.replicate 1280 42
          invalidTokenName = TokenName $ toBuiltin $ BS.replicate 1281 42
          invalidPubKeyHash = PubKeyHash $ toBuiltin $ BS.replicate 1282 42
          invalidScriptHash = ScriptHash $ toBuiltin $ BS.replicate 1283 42
          invalidAddress =
            Address
              (PubKeyCredential invalidPubKeyHash)
              (Just $ StakingHash $ ScriptCredential invalidScriptHash)
          mintValue =
            Value.assetClassValue
              (Value.AssetClass (invalidCurrencySymbol, invalidTokenName))
              1
          outputValue = Value.lovelaceValue 0 <> mintValue
         in
          ledgerFailsBy @()
            ( \case
                LedgerSimError'SubmissionError
                  ( SubmissionError'ValidationFailure
                      [ InvalidTxInfoError'Normality
                          ( Normality.InvalidTxInfoError'InvalidOutputs
                              ( Normality.InvalidOutputsError'InvalidTxOut
                                  0
                                  ( Normality.InvalidTxOutError'InvalidAddress
                                      ( Normality.InvalidAddressError'InvalidCredential
                                          ( Normality.InvalidCredentialError'InvalidPubKeyCredential
                                              ( Normality.InvalidPubKeyHashError'UnexpectedLength
                                                  _
                                                  (Normality.InvalidLedgerBytesError'UnexpectedLength 28 1282)
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        , InvalidTxInfoError'Normality
                            ( Normality.InvalidTxInfoError'InvalidOutputs
                                ( Normality.InvalidOutputsError'InvalidTxOut
                                    0
                                    ( Normality.InvalidTxOutError'InvalidAddress
                                        ( Normality.InvalidAddressError'InvalidStakingCredential
                                            ( Normality.InvalidStakingCredentialError'InvalidCredential
                                                ( Normality.InvalidCredentialError'InvalidScriptCredential
                                                    ( Normality.InvalidScriptHashError'UnexpectedLength
                                                        _
                                                        (Normality.InvalidLedgerBytesError'UnexpectedLength 28 1283)
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                        , InvalidTxInfoError'Normality
                            ( Normality.InvalidTxInfoError'InvalidOutputs
                                ( Normality.InvalidOutputsError'InvalidTxOut
                                    0
                                    ( Normality.InvalidTxOutError'InvalidValue
                                        ( Normality.InvalidValue
                                            _
                                            ( Normality.InvalidValueKind'InvalidMap
                                                ( Normality.InvalidOrderedAssocMapError'InvalidKeyValuePair
                                                    _
                                                    _
                                                    ( Normality.InvalidAssetError'InvalidCurrencySymbol
                                                        ( Normality.InvalidCurrencySymbolError'UnexpectedLength
                                                            _
                                                            (Normality.InvalidLedgerBytesError'UnexpectedLength 28 1280)
                                                          )
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                        , InvalidTxInfoError'Normality
                            ( Normality.InvalidTxInfoError'InvalidOutputs
                                ( Normality.InvalidOutputsError'InvalidTxOut
                                    0
                                    ( Normality.InvalidTxOutError'InvalidValue
                                        ( Normality.InvalidValue
                                            _
                                            ( Normality.InvalidValueKind'InvalidMap
                                                ( Normality.InvalidOrderedAssocMapError'InvalidKeyValuePair
                                                    _
                                                    _
                                                    ( Normality.InvalidAssetError'InvalidNativeToken
                                                        ( Normality.InvalidOrderedAssocMapError'InvalidKeyValuePair
                                                            _
                                                            1
                                                            ( Normality.InvalidNativeTokenError'InvalidTokenName
                                                                ( Normality.InvalidTokenNameError'TooLong
                                                                    _
                                                                    (Normality.InvalidLedgerBytesError'TooLong 32 1281)
                                                                  )
                                                              )
                                                          )
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                        , InvalidTxInfoError'Normality
                            ( Normality.InvalidTxInfoError'InvalidMint
                                ( Normality.InvalidMintError'InvalidValue
                                    ( Normality.InvalidValue
                                        _
                                        ( Normality.InvalidValueKind'InvalidMap
                                            ( Normality.InvalidOrderedAssocMapError'InvalidKeyValuePair
                                                _
                                                _
                                                ( Normality.InvalidAssetError'InvalidCurrencySymbol
                                                    ( Normality.InvalidCurrencySymbolError'UnexpectedLength
                                                        _
                                                        (Normality.InvalidLedgerBytesError'UnexpectedLength 28 1280)
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                        , InvalidTxInfoError'Normality
                            ( Normality.InvalidTxInfoError'InvalidMint
                                ( Normality.InvalidMintError'InvalidValue
                                    ( Normality.InvalidValue
                                        _
                                        ( Normality.InvalidValueKind'InvalidMap
                                            ( Normality.InvalidOrderedAssocMapError'InvalidKeyValuePair
                                                _
                                                _
                                                ( Normality.InvalidAssetError'InvalidNativeToken
                                                    ( Normality.InvalidOrderedAssocMapError'InvalidKeyValuePair
                                                        _
                                                        1
                                                        ( Normality.InvalidNativeTokenError'InvalidTokenName
                                                            ( Normality.InvalidTokenNameError'TooLong
                                                                _
                                                                (Normality.InvalidLedgerBytesError'TooLong 32 1281)
                                                              )
                                                          )
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                        , InvalidTxInfoError'Normality
                            ( Normality.InvalidTxInfoError'InvalidRedeemers
                                ( Normality.InvalidRedeemersError'InvalidScriptPurpose
                                    ( Normality.InvalidScriptPurposeError'Minting
                                        ( Normality.InvalidCurrencySymbolError'UnexpectedLength
                                            _
                                            (Normality.InvalidLedgerBytesError'UnexpectedLength 28 1280)
                                          )
                                      )
                                  )
                              )
                        , InvalidTxInfoError'Stateful
                            ( Stateful.InvalidTxInfoError'InvalidRedeemers
                                (Stateful.InvalidRedeemers'ScriptRequiredForEvaluationNotAvailable _)
                              )
                        ]
                    ) -> True
                _ -> False
            )
            $ void . submitTx
            $ TxInfo
              [dummyInput]
              mempty
              [ TxOut
                  { txOutValue = outputValue
                  , txOutReferenceScript = Nothing
                  , txOutDatum = OutputDatum $ Datum $ toBuiltinData ()
                  , txOutAddress = invalidAddress
                  }
              ]
              (Value.lovelaceValue 0)
              (Value.lovelaceValue 0 <> mintValue)
              mempty
              AssocMap.empty
              IV.always
              [ownPubKeyHash]
              (AssocMap.fromList [(Minting invalidCurrencySymbol, Redeemer (toBuiltinData @Integer 1234))])
              AssocMap.empty
              dummyTxId
    ]

intervalValidRange :: POSIXTime -> POSIXTime -> POSIXTimeRange
intervalValidRange from to = IV.Interval (IV.lowerBound from) (IV.strictUpperBound to)

dummyInput :: TxInInfo
dummyInput =
  let txOut =
        TxOut
          { txOutValue = Value.lovelaceValue 0
          , txOutReferenceScript = Nothing
          , txOutDatum = NoOutputDatum
          , txOutAddress = ownAddress
          }
      outRef = TxOutRef (dummyTxIds !! 42) 69
   in TxInInfo outRef txOut

ownAddress :: Address
ownAddress = Address (PubKeyCredential ownPubKeyHash) Nothing

ownPubKeyHash :: PubKeyHash
ownPubKeyHash = head dummyPubKeyHashes

dummyTxId :: PlutusV2.TxId
dummyTxId = head dummyTxIds

dummyTxId' :: PlutusV2.TxId
dummyTxId' = dummyTxIds !! 1

mkDummyHashes :: (HashAlgorithm alg) => alg -> [BuiltinByteString]
mkDummyHashes alg =
  toBuiltin
    . BA.convert @_ @ByteString
    . hashWith alg
    . LBS.toStrict
    . B.encode @Integer
    <$> [0 ..]

dummyBlake2b_224Hashes :: [BuiltinByteString]
dummyBlake2b_224Hashes = mkDummyHashes Blake2b_224

dummyBlake2b_256Hashes :: [BuiltinByteString]
dummyBlake2b_256Hashes = mkDummyHashes Blake2b_256

dummyPubKeyHashes :: [PubKeyHash]
dummyPubKeyHashes = PubKeyHash <$> dummyBlake2b_224Hashes

dummyTxIds :: [TxId]
dummyTxIds = TxId <$> dummyBlake2b_256Hashes

-- | Minting Policy that always succeeds.
alwaysSucceedsCbor :: BS8.ByteString
alwaysSucceedsCbor = either error id . Base16.decode $ BS8.pack "4d01000033222220051200120011"

-- | Copied from: https://github.com/IntersectMBO/plutus/blob/774616b464c44dc934957dc0738098ca270ed9ee/plutus-benchmark/marlowe/src/PlutusBenchmark/Marlowe/BenchUtil.hs#L310
testCostModel :: PlutusCostModel
testCostModel =
  PlutusCostModel $
    M.fromList
      [ (PlutusV2.AddInteger'cpu'arguments'intercept, 205665)
      , (PlutusV2.AddInteger'cpu'arguments'slope, 812)
      , (PlutusV2.AddInteger'memory'arguments'intercept, 1)
      , (PlutusV2.AddInteger'memory'arguments'slope, 1)
      , (PlutusV2.AppendByteString'cpu'arguments'intercept, 1000)
      , (PlutusV2.AppendByteString'cpu'arguments'slope, 571)
      , (PlutusV2.AppendByteString'memory'arguments'intercept, 0)
      , (PlutusV2.AppendByteString'memory'arguments'slope, 1)
      , (PlutusV2.AppendString'cpu'arguments'intercept, 1000)
      , (PlutusV2.AppendString'cpu'arguments'slope, 24177)
      , (PlutusV2.AppendString'memory'arguments'intercept, 4)
      , (PlutusV2.AppendString'memory'arguments'slope, 1)
      , (PlutusV2.BData'cpu'arguments, 1000)
      , (PlutusV2.BData'memory'arguments, 32)
      , (PlutusV2.Blake2b_256'cpu'arguments'intercept, 117366)
      , (PlutusV2.Blake2b_256'cpu'arguments'slope, 10475)
      , (PlutusV2.Blake2b_256'memory'arguments, 4)
      , (PlutusV2.CekApplyCost'exBudgetCPU, 23000)
      , (PlutusV2.CekApplyCost'exBudgetMemory, 100)
      , (PlutusV2.CekBuiltinCost'exBudgetCPU, 23000)
      , (PlutusV2.CekBuiltinCost'exBudgetMemory, 100)
      , (PlutusV2.CekConstCost'exBudgetCPU, 23000)
      , (PlutusV2.CekConstCost'exBudgetMemory, 100)
      , (PlutusV2.CekDelayCost'exBudgetCPU, 23000)
      , (PlutusV2.CekDelayCost'exBudgetMemory, 100)
      , (PlutusV2.CekForceCost'exBudgetCPU, 23000)
      , (PlutusV2.CekForceCost'exBudgetMemory, 100)
      , (PlutusV2.CekLamCost'exBudgetCPU, 23000)
      , (PlutusV2.CekLamCost'exBudgetMemory, 100)
      , (PlutusV2.CekStartupCost'exBudgetCPU, 100)
      , (PlutusV2.CekStartupCost'exBudgetMemory, 100)
      , (PlutusV2.CekVarCost'exBudgetCPU, 23000)
      , (PlutusV2.CekVarCost'exBudgetMemory, 100)
      , (PlutusV2.ChooseData'cpu'arguments, 19537)
      , (PlutusV2.ChooseData'memory'arguments, 32)
      , (PlutusV2.ChooseList'cpu'arguments, 175354)
      , (PlutusV2.ChooseList'memory'arguments, 32)
      , (PlutusV2.ChooseUnit'cpu'arguments, 46417)
      , (PlutusV2.ChooseUnit'memory'arguments, 4)
      , (PlutusV2.ConsByteString'cpu'arguments'intercept, 221973)
      , (PlutusV2.ConsByteString'cpu'arguments'slope, 511)
      , (PlutusV2.ConsByteString'memory'arguments'intercept, 0)
      , (PlutusV2.ConsByteString'memory'arguments'slope, 1)
      , (PlutusV2.ConstrData'cpu'arguments, 89141)
      , (PlutusV2.ConstrData'memory'arguments, 32)
      , (PlutusV2.DecodeUtf8'cpu'arguments'intercept, 497525)
      , (PlutusV2.DecodeUtf8'cpu'arguments'slope, 14068)
      , (PlutusV2.DecodeUtf8'memory'arguments'intercept, 4)
      , (PlutusV2.DecodeUtf8'memory'arguments'slope, 2)
      , (PlutusV2.DivideInteger'cpu'arguments'constant, 196500)
      , (PlutusV2.DivideInteger'cpu'arguments'model'arguments'intercept, 453240)
      , (PlutusV2.DivideInteger'cpu'arguments'model'arguments'slope, 220)
      , (PlutusV2.DivideInteger'memory'arguments'intercept, 0)
      , (PlutusV2.DivideInteger'memory'arguments'minimum, 1)
      , (PlutusV2.DivideInteger'memory'arguments'slope, 1)
      , (PlutusV2.EncodeUtf8'cpu'arguments'intercept, 1000)
      , (PlutusV2.EncodeUtf8'cpu'arguments'slope, 28662)
      , (PlutusV2.EncodeUtf8'memory'arguments'intercept, 4)
      , (PlutusV2.EncodeUtf8'memory'arguments'slope, 2)
      , (PlutusV2.EqualsByteString'cpu'arguments'constant, 245000)
      , (PlutusV2.EqualsByteString'cpu'arguments'intercept, 216773)
      , (PlutusV2.EqualsByteString'cpu'arguments'slope, 62)
      , (PlutusV2.EqualsByteString'memory'arguments, 1)
      , (PlutusV2.EqualsData'cpu'arguments'intercept, 1060367)
      , (PlutusV2.EqualsData'cpu'arguments'slope, 12586)
      , (PlutusV2.EqualsData'memory'arguments, 1)
      , (PlutusV2.EqualsInteger'cpu'arguments'intercept, 208512)
      , (PlutusV2.EqualsInteger'cpu'arguments'slope, 421)
      , (PlutusV2.EqualsInteger'memory'arguments, 1)
      , (PlutusV2.EqualsString'cpu'arguments'constant, 187000)
      , (PlutusV2.EqualsString'cpu'arguments'intercept, 1000)
      , (PlutusV2.EqualsString'cpu'arguments'slope, 52998)
      , (PlutusV2.EqualsString'memory'arguments, 1)
      , (PlutusV2.FstPair'cpu'arguments, 80436)
      , (PlutusV2.FstPair'memory'arguments, 32)
      , (PlutusV2.HeadList'cpu'arguments, 43249)
      , (PlutusV2.HeadList'memory'arguments, 32)
      , (PlutusV2.IData'cpu'arguments, 1000)
      , (PlutusV2.IData'memory'arguments, 32)
      , (PlutusV2.IfThenElse'cpu'arguments, 80556)
      , (PlutusV2.IfThenElse'memory'arguments, 1)
      , (PlutusV2.IndexByteString'cpu'arguments, 57667)
      , (PlutusV2.IndexByteString'memory'arguments, 4)
      , (PlutusV2.LengthOfByteString'cpu'arguments, 1000)
      , (PlutusV2.LengthOfByteString'memory'arguments, 10)
      , (PlutusV2.LessThanByteString'cpu'arguments'intercept, 197145)
      , (PlutusV2.LessThanByteString'cpu'arguments'slope, 156)
      , (PlutusV2.LessThanByteString'memory'arguments, 1)
      , (PlutusV2.LessThanEqualsByteString'cpu'arguments'intercept, 197145)
      , (PlutusV2.LessThanEqualsByteString'cpu'arguments'slope, 156)
      , (PlutusV2.LessThanEqualsByteString'memory'arguments, 1)
      , (PlutusV2.LessThanEqualsInteger'cpu'arguments'intercept, 204924)
      , (PlutusV2.LessThanEqualsInteger'cpu'arguments'slope, 473)
      , (PlutusV2.LessThanEqualsInteger'memory'arguments, 1)
      , (PlutusV2.LessThanInteger'cpu'arguments'intercept, 208896)
      , (PlutusV2.LessThanInteger'cpu'arguments'slope, 511)
      , (PlutusV2.LessThanInteger'memory'arguments, 1)
      , (PlutusV2.ListData'cpu'arguments, 52467)
      , (PlutusV2.ListData'memory'arguments, 32)
      , (PlutusV2.MapData'cpu'arguments, 64832)
      , (PlutusV2.MapData'memory'arguments, 32)
      , (PlutusV2.MkCons'cpu'arguments, 65493)
      , (PlutusV2.MkCons'memory'arguments, 32)
      , (PlutusV2.MkNilData'cpu'arguments, 22558)
      , (PlutusV2.MkNilData'memory'arguments, 32)
      , (PlutusV2.MkNilPairData'cpu'arguments, 16563)
      , (PlutusV2.MkNilPairData'memory'arguments, 32)
      , (PlutusV2.MkPairData'cpu'arguments, 76511)
      , (PlutusV2.MkPairData'memory'arguments, 32)
      , (PlutusV2.ModInteger'cpu'arguments'constant, 196500)
      , (PlutusV2.ModInteger'cpu'arguments'model'arguments'intercept, 453240)
      , (PlutusV2.ModInteger'cpu'arguments'model'arguments'slope, 220)
      , (PlutusV2.ModInteger'memory'arguments'intercept, 0)
      , (PlutusV2.ModInteger'memory'arguments'minimum, 1)
      , (PlutusV2.ModInteger'memory'arguments'slope, 1)
      , (PlutusV2.MultiplyInteger'cpu'arguments'intercept, 69522)
      , (PlutusV2.MultiplyInteger'cpu'arguments'slope, 11687)
      , (PlutusV2.MultiplyInteger'memory'arguments'intercept, 0)
      , (PlutusV2.MultiplyInteger'memory'arguments'slope, 1)
      , (PlutusV2.NullList'cpu'arguments, 60091)
      , (PlutusV2.NullList'memory'arguments, 32)
      , (PlutusV2.QuotientInteger'cpu'arguments'constant, 196500)
      , (PlutusV2.QuotientInteger'cpu'arguments'model'arguments'intercept, 453240)
      , (PlutusV2.QuotientInteger'cpu'arguments'model'arguments'slope, 220)
      , (PlutusV2.QuotientInteger'memory'arguments'intercept, 0)
      , (PlutusV2.QuotientInteger'memory'arguments'minimum, 1)
      , (PlutusV2.QuotientInteger'memory'arguments'slope, 1)
      , (PlutusV2.RemainderInteger'cpu'arguments'constant, 196500)
      , (PlutusV2.RemainderInteger'cpu'arguments'model'arguments'intercept, 453240)
      , (PlutusV2.RemainderInteger'cpu'arguments'model'arguments'slope, 220)
      , (PlutusV2.RemainderInteger'memory'arguments'intercept, 0)
      , (PlutusV2.RemainderInteger'memory'arguments'minimum, 1)
      , (PlutusV2.RemainderInteger'memory'arguments'slope, 1)
      , (PlutusV2.SerialiseData'cpu'arguments'intercept, 1159724)
      , (PlutusV2.SerialiseData'cpu'arguments'slope, 392670)
      , (PlutusV2.SerialiseData'memory'arguments'intercept, 0)
      , (PlutusV2.SerialiseData'memory'arguments'slope, 2)
      , (PlutusV2.Sha2_256'cpu'arguments'intercept, 806990)
      , (PlutusV2.Sha2_256'cpu'arguments'slope, 30482)
      , (PlutusV2.Sha2_256'memory'arguments, 4)
      , (PlutusV2.Sha3_256'cpu'arguments'intercept, 1927926)
      , (PlutusV2.Sha3_256'cpu'arguments'slope, 82523)
      , (PlutusV2.Sha3_256'memory'arguments, 4)
      , (PlutusV2.SliceByteString'cpu'arguments'intercept, 265318)
      , (PlutusV2.SliceByteString'cpu'arguments'slope, 0)
      , (PlutusV2.SliceByteString'memory'arguments'intercept, 4)
      , (PlutusV2.SliceByteString'memory'arguments'slope, 0)
      , (PlutusV2.SndPair'cpu'arguments, 85931)
      , (PlutusV2.SndPair'memory'arguments, 32)
      , (PlutusV2.SubtractInteger'cpu'arguments'intercept, 205665)
      , (PlutusV2.SubtractInteger'cpu'arguments'slope, 812)
      , (PlutusV2.SubtractInteger'memory'arguments'intercept, 1)
      , (PlutusV2.SubtractInteger'memory'arguments'slope, 1)
      , (PlutusV2.TailList'cpu'arguments, 41182)
      , (PlutusV2.TailList'memory'arguments, 32)
      , (PlutusV2.Trace'cpu'arguments, 212342)
      , (PlutusV2.Trace'memory'arguments, 32)
      , (PlutusV2.UnBData'cpu'arguments, 31220)
      , (PlutusV2.UnBData'memory'arguments, 32)
      , (PlutusV2.UnConstrData'cpu'arguments, 32696)
      , (PlutusV2.UnConstrData'memory'arguments, 32)
      , (PlutusV2.UnIData'cpu'arguments, 43357)
      , (PlutusV2.UnIData'memory'arguments, 32)
      , (PlutusV2.UnListData'cpu'arguments, 32247)
      , (PlutusV2.UnListData'memory'arguments, 32)
      , (PlutusV2.UnMapData'cpu'arguments, 38314)
      , (PlutusV2.UnMapData'memory'arguments, 32)
      , (PlutusV2.VerifyEcdsaSecp256k1Signature'cpu'arguments, 35892428)
      , (PlutusV2.VerifyEcdsaSecp256k1Signature'memory'arguments, 10)
      , (PlutusV2.VerifyEd25519Signature'cpu'arguments'intercept, 9462713)
      , (PlutusV2.VerifyEd25519Signature'cpu'arguments'slope, 1021)
      , (PlutusV2.VerifyEd25519Signature'memory'arguments, 10)
      , (PlutusV2.VerifySchnorrSecp256k1Signature'cpu'arguments'intercept, 38887044)
      , (PlutusV2.VerifySchnorrSecp256k1Signature'cpu'arguments'slope, 32947)
      , (PlutusV2.VerifySchnorrSecp256k1Signature'memory'arguments, 10)
      ]
