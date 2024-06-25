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
) where

import Control.Monad.Reader (MonadReader (ask), Reader, runReader)
import Ledger.Sim (LedgerSim, LedgerSimError (LedgerSimError'ApplicationError), runLedgerSim)
import Ledger.Sim.Types.Config (LedgerConfig)
import Ledger.Sim.Types.State (LedgerState)
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
ledgerFailsBy' predicate = ledgerFailsBy $ \case LedgerSimError'ApplicationError e -> predicate e; _ -> False

ledgerFailsWith :: (Eq e, Show e, Eq a, Show a) => LedgerSimError e -> LedgerSim ctx st e a -> LedgerExpectation ctx st
ledgerFailsWith = FailsWith

ledgerFailsWith' :: (Eq e, Show e, Eq a, Show a) => e -> LedgerSim ctx st e a -> LedgerExpectation ctx st
ledgerFailsWith' = ledgerFailsWith . LedgerSimError'ApplicationError

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
