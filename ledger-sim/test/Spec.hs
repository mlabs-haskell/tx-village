import Control.Exception (throwIO)
import Control.Monad.Trans.State.Strict (gets)
import Data.Bifunctor (first)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short qualified as SBS
import Data.Functor (void)
import Data.Map.Strict qualified as M
import Data.String (fromString)

import Test.Tasty (TestTree, defaultMain)

import PlutusLedgerApi.Common.Versions (vasilPV)
import PlutusLedgerApi.V1 qualified as Plutus
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V1.Value qualified as PlutusValue
import PlutusLedgerApi.V2 (
    Address (Address),
    Credential (PubKeyCredential),
    OutputDatum (NoOutputDatum),
    PubKeyHash,
    Redeemer (Redeemer),
    ScriptHash,
    ScriptPurpose (Minting, Spending),
    ToData (toBuiltinData),
    TxInInfo (TxInInfo),
    TxInfo (TxInfo),
    TxOut (TxOut, txOutAddress, txOutDatum, txOutReferenceScript, txOutValue),
    TxOutRef (TxOutRef),
    deserialiseScript,
 )
import PlutusLedgerApi.V2 qualified as PlutusV2
import PlutusTx.AssocMap qualified as PlutusMap

import Ledger.Sim (
    LedgerConfig,
    LedgerState (ls'currentTime),
    LedgerValidatorError (TxInvalidRange, TxMissingInputSpent, TxMissingOwnerSignature, TxNonExistentInput, TxUnbalanced),
    emptyLedgerState,
    getCurrentSlot,
    lookupUTxO,
    submitTx,
    throwLedgerError,
 )
import Ledger.Sim.Test (ledgerFailsBy, ledgerSucceeds, ledgerTestCase, ledgerTestGroup)
import Ledger.Sim.Types.Config (PlutusCostModel (PlutusCostModel), mkLedgerConfig)
import Ledger.Sim.Types.Script (hashScriptV2)

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
        (emptyLedgerState ())
        "Tests"
        [ ledgerTestCase "Simple Minting Tx" $
            ledgerSucceeds @() $ do
                let cs = PlutusValue.CurrencySymbol $ Plutus.getScriptHash dummyScriptHash
                    mintVal =
                        PlutusValue.assetClassValue
                            (PlutusValue.AssetClass (cs, fromString "A"))
                            1
                currentTime <- gets ls'currentTime
                void . submitTx $
                    TxInfo
                        mempty
                        mempty
                        [ TxOut
                            { txOutValue = PlutusValue.lovelaceValue 0 <> mintVal
                            , txOutReferenceScript = Nothing
                            , txOutDatum = NoOutputDatum
                            , txOutAddress = ownAddress
                            }
                        ]
                        (PlutusValue.lovelaceValue 0)
                        (PlutusValue.lovelaceValue 0 <> mintVal)
                        mempty
                        PlutusMap.empty
                        (interval currentTime (currentTime + 1))
                        [ownPubKeyHash]
                        (PlutusMap.fromList [(Minting cs, Redeemer (toBuiltinData @Integer 1234))])
                        PlutusMap.empty
                        dummyTxId
        , ledgerTestCase "Invalid range" $
            ledgerFailsBy @() (\case TxInvalidRange{} -> True; _ -> False) $ do
                currentTime <- gets ls'currentTime
                void . submitTx $
                    TxInfo
                        mempty
                        mempty
                        [ TxOut
                            { txOutValue = PlutusValue.lovelaceValue 0
                            , txOutReferenceScript = Nothing
                            , txOutDatum = NoOutputDatum
                            , txOutAddress = ownAddress
                            }
                        ]
                        (PlutusValue.lovelaceValue 0)
                        (PlutusValue.lovelaceValue 0)
                        mempty
                        PlutusMap.empty
                        (interval (currentTime - 2) (currentTime - 1))
                        [ownPubKeyHash]
                        PlutusMap.empty
                        PlutusMap.empty
                        dummyTxId
        , ledgerTestCase "Non existent input" $
            ledgerFailsBy @() (\case TxNonExistentInput{} -> True; _ -> False) $ do
                currentTime <- gets ls'currentTime
                void . submitTx $
                    TxInfo
                        [ TxInInfo
                            (TxOutRef dummyTxId' 1)
                            $ TxOut
                                { txOutValue = PlutusValue.lovelaceValue 0
                                , txOutReferenceScript = Nothing
                                , txOutDatum = NoOutputDatum
                                , txOutAddress = ownAddress
                                }
                        ]
                        mempty
                        [ TxOut
                            { txOutValue = PlutusValue.lovelaceValue 0
                            , txOutReferenceScript = Nothing
                            , txOutDatum = NoOutputDatum
                            , txOutAddress = ownAddress
                            }
                        ]
                        (PlutusValue.lovelaceValue 0)
                        (PlutusValue.lovelaceValue 0)
                        mempty
                        PlutusMap.empty
                        (interval currentTime (currentTime + 1))
                        [ownPubKeyHash]
                        PlutusMap.empty
                        PlutusMap.empty
                        dummyTxId
        , ledgerTestCase "Missing signature" $
            ledgerFailsBy @String (\case TxMissingOwnerSignature{} -> True; _ -> False) $ do
                let
                    cs = PlutusValue.CurrencySymbol $ Plutus.getScriptHash dummyScriptHash
                    mintVal =
                        PlutusValue.assetClassValue
                            (PlutusValue.AssetClass (cs, fromString "A"))
                            1
                currentTime <- getCurrentSlot
                txId <-
                    submitTx $
                        TxInfo
                            mempty
                            mempty
                            [ TxOut
                                { txOutValue = PlutusValue.lovelaceValue 0 <> mintVal
                                , txOutReferenceScript = Nothing
                                , txOutDatum = NoOutputDatum
                                , txOutAddress = ownAddress
                                }
                            ]
                            (PlutusValue.lovelaceValue 0)
                            (PlutusValue.lovelaceValue 0 <> mintVal)
                            mempty
                            PlutusMap.empty
                            (interval currentTime (currentTime + 1))
                            [ownPubKeyHash]
                            (PlutusMap.fromList [(Minting cs, Redeemer (toBuiltinData @Integer 1234))])
                            PlutusMap.empty
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
                            { txOutValue = PlutusValue.lovelaceValue 0
                            , txOutReferenceScript = Nothing
                            , txOutDatum = NoOutputDatum
                            , txOutAddress = ownAddress
                            }
                        ]
                        (PlutusValue.lovelaceValue 0)
                        (PlutusValue.lovelaceValue 0)
                        mempty
                        PlutusMap.empty
                        (interval currentTime (currentTime + 1))
                        mempty
                        PlutusMap.empty
                        PlutusMap.empty
                        dummyTxId
        , ledgerTestCase "Unbalanced" $
            ledgerFailsBy @() (\case TxUnbalanced{} -> True; _ -> False) $ do
                let cs = PlutusValue.CurrencySymbol $ Plutus.getScriptHash dummyScriptHash
                    mintVal =
                        PlutusValue.assetClassValue
                            (PlutusValue.AssetClass (cs, fromString "A"))
                            1
                currentTime <- getCurrentSlot
                void . submitTx $
                    TxInfo
                        mempty
                        mempty
                        [ TxOut
                            { txOutValue = PlutusValue.lovelaceValue 0 <> mintVal
                            , txOutReferenceScript = Nothing
                            , txOutDatum = NoOutputDatum
                            , txOutAddress = ownAddress
                            }
                        ]
                        (PlutusValue.lovelaceValue 0)
                        (PlutusValue.lovelaceValue 0)
                        mempty
                        PlutusMap.empty
                        (interval currentTime (currentTime + 1))
                        [ownPubKeyHash]
                        PlutusMap.empty
                        PlutusMap.empty
                        dummyTxId
        , ledgerTestCase "Missing Input Spent" $
            ledgerFailsBy @() (\case TxMissingInputSpent{} -> True; _ -> False) $ do
                let ref = TxOutRef dummyTxId' 1
                currentTime <- getCurrentSlot
                void . submitTx $
                    TxInfo
                        mempty
                        mempty
                        [ TxOut
                            { txOutValue = PlutusValue.lovelaceValue 0
                            , txOutReferenceScript = Nothing
                            , txOutDatum = NoOutputDatum
                            , txOutAddress = ownAddress
                            }
                        ]
                        (PlutusValue.lovelaceValue 0)
                        (PlutusValue.lovelaceValue 0)
                        mempty
                        PlutusMap.empty
                        (interval currentTime (currentTime + 1))
                        [ownPubKeyHash]
                        (PlutusMap.fromList [(Spending ref, Redeemer $ toBuiltinData ())])
                        PlutusMap.empty
                        dummyTxId
        ]

ownAddress :: Address
ownAddress = Address (PubKeyCredential ownPubKeyHash) Nothing

ownPubKeyHash :: PubKeyHash
ownPubKeyHash = fromString "e17c366f879d0f7ef28a405f3101e46ea5c2329a4bbca5f0276f8fba19"

dummyTxId :: PlutusV2.TxId
dummyTxId = fromString "847971d6db0576dcfeb0f041630a0ff111a11cb1921c7507c65c3b0dea58bc49"

dummyTxId' :: PlutusV2.TxId
dummyTxId' = fromString "135981d6db0576dcfeb0f021530a0ee111a11cb1921c7507c65c3b0dea35ab67"

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
