{-# OPTIONS_GHC -Wno-missing-import-lists #-}

import Control.Exception (throwIO)
import Control.Monad.Trans.State.Strict (gets)
import Data.Bifunctor (first)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short qualified as SBS
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
    ParamName (..),
    PubKeyHash,
    Redeemer (Redeemer),
    ScriptHash,
    ScriptPurpose (Minting),
    ToData (toBuiltinData),
    TxInfo (..),
    TxOut (TxOut, txOutAddress, txOutDatum, txOutReferenceScript, txOutValue),
    deserialiseScript,
 )
import PlutusTx.AssocMap qualified as PlutusMap

import Ledger.Sim (
    LedgerConfig,
    LedgerState (ls'currentTime),
    emptyLedgerState,
    submitTx,
 )
import Ledger.Sim.Test
import Ledger.Sim.Types.Config (PlutusCostModel (..), mkLedgerConfig)
import Ledger.Sim.Types.Script (hashScriptV2)

main :: IO ()
main = do
    script <- either error pure . first show . deserialiseScript vasilPV $ SBS.toShort alwaysSucceedsCbor
    let sh = hashScriptV2 script
    ledgerCfg <- either throwIO pure $ mkLedgerConfig (M.fromList [(sh, script)]) testCostModel
    defaultMain $ tests sh ledgerCfg

tests :: ScriptHash -> LedgerConfig -> TestTree
tests dummyScriptHash ledgerCfg =
    ledgerTestGroup
        ledgerCfg
        emptyLedgerState
        "Tests"
        [ ledgerTestCase "Simple Minting Tx" $ do
            ledgerSucceeds @() $ do
                let ownPubKeyHash :: PubKeyHash = fromString "e17c366f879d0f7ef28a405f3101e46ea5c2329a4bbca5f0276f8fba19"
                    cs = PlutusValue.CurrencySymbol $ Plutus.getScriptHash dummyScriptHash
                    mintVal =
                        PlutusValue.assetClassValue
                            (PlutusValue.AssetClass (cs, fromString "A"))
                            1
                    ownAddress = Address (PubKeyCredential ownPubKeyHash) Nothing
                currentTime <- gets ls'currentTime
                submitTx
                    $ TxInfo
                        mempty
                        mempty
                        [ TxOut
                            { txOutValue = mintVal
                            , txOutReferenceScript = Nothing
                            , txOutDatum = NoOutputDatum
                            , txOutAddress = ownAddress
                            }
                        ]
                        mempty
                        mintVal
                        mempty
                        PlutusMap.empty
                        (interval currentTime (currentTime + 1))
                        [ownPubKeyHash]
                        (PlutusMap.fromList [(Minting cs, Redeemer (toBuiltinData @Integer 1234))])
                        PlutusMap.empty
                    $ fromString "847971d6db0576dcfeb0f041630a0ff111a11cb1921c7507c65c3b0dea58bc49"
        ]

-- | Minting Policy that always succeeds.
alwaysSucceedsCbor :: BS8.ByteString
alwaysSucceedsCbor = either error id . Base16.decode $ BS8.pack "4d01000033222220051200120011"

-- | Copied from: https://github.com/IntersectMBO/plutus/blob/774616b464c44dc934957dc0738098ca270ed9ee/plutus-benchmark/marlowe/src/PlutusBenchmark/Marlowe/BenchUtil.hs#L310
testCostModel :: PlutusCostModel
testCostModel =
    PlutusCostModel $
        M.fromList
            [ (AddInteger'cpu'arguments'intercept, 205665)
            , (AddInteger'cpu'arguments'slope, 812)
            , (AddInteger'memory'arguments'intercept, 1)
            , (AddInteger'memory'arguments'slope, 1)
            , (AppendByteString'cpu'arguments'intercept, 1000)
            , (AppendByteString'cpu'arguments'slope, 571)
            , (AppendByteString'memory'arguments'intercept, 0)
            , (AppendByteString'memory'arguments'slope, 1)
            , (AppendString'cpu'arguments'intercept, 1000)
            , (AppendString'cpu'arguments'slope, 24177)
            , (AppendString'memory'arguments'intercept, 4)
            , (AppendString'memory'arguments'slope, 1)
            , (BData'cpu'arguments, 1000)
            , (BData'memory'arguments, 32)
            , (Blake2b_256'cpu'arguments'intercept, 117366)
            , (Blake2b_256'cpu'arguments'slope, 10475)
            , (Blake2b_256'memory'arguments, 4)
            , (CekApplyCost'exBudgetCPU, 23000)
            , (CekApplyCost'exBudgetMemory, 100)
            , (CekBuiltinCost'exBudgetCPU, 23000)
            , (CekBuiltinCost'exBudgetMemory, 100)
            , (CekConstCost'exBudgetCPU, 23000)
            , (CekConstCost'exBudgetMemory, 100)
            , (CekDelayCost'exBudgetCPU, 23000)
            , (CekDelayCost'exBudgetMemory, 100)
            , (CekForceCost'exBudgetCPU, 23000)
            , (CekForceCost'exBudgetMemory, 100)
            , (CekLamCost'exBudgetCPU, 23000)
            , (CekLamCost'exBudgetMemory, 100)
            , (CekStartupCost'exBudgetCPU, 100)
            , (CekStartupCost'exBudgetMemory, 100)
            , (CekVarCost'exBudgetCPU, 23000)
            , (CekVarCost'exBudgetMemory, 100)
            , (ChooseData'cpu'arguments, 19537)
            , (ChooseData'memory'arguments, 32)
            , (ChooseList'cpu'arguments, 175354)
            , (ChooseList'memory'arguments, 32)
            , (ChooseUnit'cpu'arguments, 46417)
            , (ChooseUnit'memory'arguments, 4)
            , (ConsByteString'cpu'arguments'intercept, 221973)
            , (ConsByteString'cpu'arguments'slope, 511)
            , (ConsByteString'memory'arguments'intercept, 0)
            , (ConsByteString'memory'arguments'slope, 1)
            , (ConstrData'cpu'arguments, 89141)
            , (ConstrData'memory'arguments, 32)
            , (DecodeUtf8'cpu'arguments'intercept, 497525)
            , (DecodeUtf8'cpu'arguments'slope, 14068)
            , (DecodeUtf8'memory'arguments'intercept, 4)
            , (DecodeUtf8'memory'arguments'slope, 2)
            , (DivideInteger'cpu'arguments'constant, 196500)
            , (DivideInteger'cpu'arguments'model'arguments'intercept, 453240)
            , (DivideInteger'cpu'arguments'model'arguments'slope, 220)
            , (DivideInteger'memory'arguments'intercept, 0)
            , (DivideInteger'memory'arguments'minimum, 1)
            , (DivideInteger'memory'arguments'slope, 1)
            , (EncodeUtf8'cpu'arguments'intercept, 1000)
            , (EncodeUtf8'cpu'arguments'slope, 28662)
            , (EncodeUtf8'memory'arguments'intercept, 4)
            , (EncodeUtf8'memory'arguments'slope, 2)
            , (EqualsByteString'cpu'arguments'constant, 245000)
            , (EqualsByteString'cpu'arguments'intercept, 216773)
            , (EqualsByteString'cpu'arguments'slope, 62)
            , (EqualsByteString'memory'arguments, 1)
            , (EqualsData'cpu'arguments'intercept, 1060367)
            , (EqualsData'cpu'arguments'slope, 12586)
            , (EqualsData'memory'arguments, 1)
            , (EqualsInteger'cpu'arguments'intercept, 208512)
            , (EqualsInteger'cpu'arguments'slope, 421)
            , (EqualsInteger'memory'arguments, 1)
            , (EqualsString'cpu'arguments'constant, 187000)
            , (EqualsString'cpu'arguments'intercept, 1000)
            , (EqualsString'cpu'arguments'slope, 52998)
            , (EqualsString'memory'arguments, 1)
            , (FstPair'cpu'arguments, 80436)
            , (FstPair'memory'arguments, 32)
            , (HeadList'cpu'arguments, 43249)
            , (HeadList'memory'arguments, 32)
            , (IData'cpu'arguments, 1000)
            , (IData'memory'arguments, 32)
            , (IfThenElse'cpu'arguments, 80556)
            , (IfThenElse'memory'arguments, 1)
            , (IndexByteString'cpu'arguments, 57667)
            , (IndexByteString'memory'arguments, 4)
            , (LengthOfByteString'cpu'arguments, 1000)
            , (LengthOfByteString'memory'arguments, 10)
            , (LessThanByteString'cpu'arguments'intercept, 197145)
            , (LessThanByteString'cpu'arguments'slope, 156)
            , (LessThanByteString'memory'arguments, 1)
            , (LessThanEqualsByteString'cpu'arguments'intercept, 197145)
            , (LessThanEqualsByteString'cpu'arguments'slope, 156)
            , (LessThanEqualsByteString'memory'arguments, 1)
            , (LessThanEqualsInteger'cpu'arguments'intercept, 204924)
            , (LessThanEqualsInteger'cpu'arguments'slope, 473)
            , (LessThanEqualsInteger'memory'arguments, 1)
            , (LessThanInteger'cpu'arguments'intercept, 208896)
            , (LessThanInteger'cpu'arguments'slope, 511)
            , (LessThanInteger'memory'arguments, 1)
            , (ListData'cpu'arguments, 52467)
            , (ListData'memory'arguments, 32)
            , (MapData'cpu'arguments, 64832)
            , (MapData'memory'arguments, 32)
            , (MkCons'cpu'arguments, 65493)
            , (MkCons'memory'arguments, 32)
            , (MkNilData'cpu'arguments, 22558)
            , (MkNilData'memory'arguments, 32)
            , (MkNilPairData'cpu'arguments, 16563)
            , (MkNilPairData'memory'arguments, 32)
            , (MkPairData'cpu'arguments, 76511)
            , (MkPairData'memory'arguments, 32)
            , (ModInteger'cpu'arguments'constant, 196500)
            , (ModInteger'cpu'arguments'model'arguments'intercept, 453240)
            , (ModInteger'cpu'arguments'model'arguments'slope, 220)
            , (ModInteger'memory'arguments'intercept, 0)
            , (ModInteger'memory'arguments'minimum, 1)
            , (ModInteger'memory'arguments'slope, 1)
            , (MultiplyInteger'cpu'arguments'intercept, 69522)
            , (MultiplyInteger'cpu'arguments'slope, 11687)
            , (MultiplyInteger'memory'arguments'intercept, 0)
            , (MultiplyInteger'memory'arguments'slope, 1)
            , (NullList'cpu'arguments, 60091)
            , (NullList'memory'arguments, 32)
            , (QuotientInteger'cpu'arguments'constant, 196500)
            , (QuotientInteger'cpu'arguments'model'arguments'intercept, 453240)
            , (QuotientInteger'cpu'arguments'model'arguments'slope, 220)
            , (QuotientInteger'memory'arguments'intercept, 0)
            , (QuotientInteger'memory'arguments'minimum, 1)
            , (QuotientInteger'memory'arguments'slope, 1)
            , (RemainderInteger'cpu'arguments'constant, 196500)
            , (RemainderInteger'cpu'arguments'model'arguments'intercept, 453240)
            , (RemainderInteger'cpu'arguments'model'arguments'slope, 220)
            , (RemainderInteger'memory'arguments'intercept, 0)
            , (RemainderInteger'memory'arguments'minimum, 1)
            , (RemainderInteger'memory'arguments'slope, 1)
            , (SerialiseData'cpu'arguments'intercept, 1159724)
            , (SerialiseData'cpu'arguments'slope, 392670)
            , (SerialiseData'memory'arguments'intercept, 0)
            , (SerialiseData'memory'arguments'slope, 2)
            , (Sha2_256'cpu'arguments'intercept, 806990)
            , (Sha2_256'cpu'arguments'slope, 30482)
            , (Sha2_256'memory'arguments, 4)
            , (Sha3_256'cpu'arguments'intercept, 1927926)
            , (Sha3_256'cpu'arguments'slope, 82523)
            , (Sha3_256'memory'arguments, 4)
            , (SliceByteString'cpu'arguments'intercept, 265318)
            , (SliceByteString'cpu'arguments'slope, 0)
            , (SliceByteString'memory'arguments'intercept, 4)
            , (SliceByteString'memory'arguments'slope, 0)
            , (SndPair'cpu'arguments, 85931)
            , (SndPair'memory'arguments, 32)
            , (SubtractInteger'cpu'arguments'intercept, 205665)
            , (SubtractInteger'cpu'arguments'slope, 812)
            , (SubtractInteger'memory'arguments'intercept, 1)
            , (SubtractInteger'memory'arguments'slope, 1)
            , (TailList'cpu'arguments, 41182)
            , (TailList'memory'arguments, 32)
            , (Trace'cpu'arguments, 212342)
            , (Trace'memory'arguments, 32)
            , (UnBData'cpu'arguments, 31220)
            , (UnBData'memory'arguments, 32)
            , (UnConstrData'cpu'arguments, 32696)
            , (UnConstrData'memory'arguments, 32)
            , (UnIData'cpu'arguments, 43357)
            , (UnIData'memory'arguments, 32)
            , (UnListData'cpu'arguments, 32247)
            , (UnListData'memory'arguments, 32)
            , (UnMapData'cpu'arguments, 38314)
            , (UnMapData'memory'arguments, 32)
            , (VerifyEcdsaSecp256k1Signature'cpu'arguments, 35892428)
            , (VerifyEcdsaSecp256k1Signature'memory'arguments, 10)
            , (VerifyEd25519Signature'cpu'arguments'intercept, 9462713)
            , (VerifyEd25519Signature'cpu'arguments'slope, 1021)
            , (VerifyEd25519Signature'memory'arguments, 10)
            , (VerifySchnorrSecp256k1Signature'cpu'arguments'intercept, 38887044)
            , (VerifySchnorrSecp256k1Signature'cpu'arguments'slope, 32947)
            , (VerifySchnorrSecp256k1Signature'memory'arguments, 10)
            ]
