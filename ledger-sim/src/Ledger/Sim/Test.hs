module Ledger.Sim.Test (
  LedgerExpectation,
  ledgerTestGroup,
  ledgerTestCase,
  ledgerSucceeds,
  ledgerSucceedsWith,
  ledgerFails,
  ledgerFailsBy,
  ledgerFailsBy',
  ledgerFailsWith,
  ledgerFailsWith',
  testCostModel,
) where

import Control.Monad.Reader (MonadReader (ask), Reader, runReader)
import Data.Map qualified as M
import Ledger.Sim (LedgerSim, LedgerSimError (LedgerSimError'Application), runLedgerSim)
import Ledger.Sim.Types.Config (LedgerConfig, PlutusCostModel (PlutusCostModel))
import Ledger.Sim.Types.State (LedgerState)
import PlutusLedgerApi.V2 qualified as PlutusV2
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

ledgerTestGroup :: LedgerConfig ctx -> LedgerState st -> TestName -> [Reader (LedgerConfig ctx, LedgerState st) TestTree] -> TestTree
ledgerTestGroup cfg st testName trees = testGroup testName $ flip runReader (cfg, st) <$> trees

ledgerTestCase :: TestName -> LedgerExpectation ctx st -> Reader (LedgerConfig ctx, LedgerState st) TestTree
ledgerTestCase testName test = do
  (cfg, st) <- ask
  pure . testCase testName $ toAssertion cfg st test

ledgerSucceeds :: (Eq e, Show e) => LedgerSim ctx st e () -> LedgerExpectation ctx st
ledgerSucceeds = ledgerSucceedsWith ()

ledgerSucceedsWith :: (Eq e, Show e, Eq a, Show a) => a -> LedgerSim ctx st e a -> LedgerExpectation ctx st
ledgerSucceedsWith = SucceedsWith

ledgerFails :: (Show e) => LedgerSim ctx st e () -> LedgerExpectation ctx st
ledgerFails = ledgerFailsBy $ const True

ledgerFailsBy :: (Show e) => (LedgerSimError e -> Bool) -> LedgerSim ctx st e () -> LedgerExpectation ctx st
ledgerFailsBy = FailsBy

ledgerFailsBy' :: (Show e) => (e -> Bool) -> LedgerSim ctx st e () -> LedgerExpectation ctx st
ledgerFailsBy' predicate = ledgerFailsBy $ \case LedgerSimError'Application e -> predicate e; _ -> False

ledgerFailsWith :: (Eq e, Show e, Eq a, Show a) => LedgerSimError e -> LedgerSim ctx st e a -> LedgerExpectation ctx st
ledgerFailsWith = FailsWith

ledgerFailsWith' :: (Eq e, Show e, Eq a, Show a) => e -> LedgerSim ctx st e a -> LedgerExpectation ctx st
ledgerFailsWith' = ledgerFailsWith . LedgerSimError'Application

data LedgerExpectation ctx st
  = forall e a. (Eq e, Show e, Eq a, Show a) => SucceedsWith a (LedgerSim ctx st e a)
  | forall e a. (Eq e, Show e, Eq a, Show a) => FailsWith (LedgerSimError e) (LedgerSim ctx st e a)
  | forall e a. (Show e) => FailsBy (LedgerSimError e -> Bool) (LedgerSim ctx st e a)

toAssertion :: LedgerConfig ctx -> LedgerState st -> LedgerExpectation ctx st -> Assertion
toAssertion cfg st (SucceedsWith x sim) = runLedgerSim cfg st sim @?= Right x
toAssertion cfg st (FailsWith x sim) = runLedgerSim cfg st sim @?= Left x
toAssertion cfg st (FailsBy predicate sim) =
  case runLedgerSim cfg st sim of
    Left err -> assertBool ("Expected an error that passes given predicate but it did not; error: " ++ show err) $ predicate err
    _ -> assertFailure "Expected contract to fail but it succeeded"

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
