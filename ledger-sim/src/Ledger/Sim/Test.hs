module Ledger.Sim.Test (LedgerExpectation, ledgerTestGroup, ledgerTestCase, ledgerSucceeds, ledgerSucceedsWith, ledgerFails, ledgerFailsWith) where

import Control.Monad.Trans.Reader (Reader, ask, runReader)
import Data.Either (isLeft)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import Ledger.Sim (
    LedgerConfig,
    LedgerSim,
    LedgerState,
    LedgerValidatorError,
    runLedgerSim,
 )

ledgerTestGroup :: LedgerConfig -> LedgerState -> TestName -> [Reader (LedgerConfig, LedgerState) TestTree] -> TestTree
ledgerTestGroup cfg st testName trees = testGroup testName $ flip runReader (cfg, st) <$> trees

ledgerTestCase :: TestName -> LedgerExpectation -> Reader (LedgerConfig, LedgerState) TestTree
ledgerTestCase testName test = do
    (cfg, st) <- ask
    pure . testCase testName $ toAssertion cfg st test

ledgerSucceeds :: (Eq e, Show e) => LedgerSim e () -> LedgerExpectation
ledgerSucceeds = ledgerSucceedsWith ()

ledgerSucceedsWith :: (Eq e, Show e, Eq a, Show a) => a -> LedgerSim e a -> LedgerExpectation
ledgerSucceedsWith = SucceedsWith

ledgerFails :: LedgerSim e () -> LedgerExpectation
ledgerFails = Fails

ledgerFailsWith :: (Eq e, Show e, Eq a, Show a) => LedgerValidatorError e -> LedgerSim e a -> LedgerExpectation
ledgerFailsWith = FailsWith

data LedgerExpectation
    = forall e a. (Eq e, Show e, Eq a, Show a) => SucceedsWith a (LedgerSim e a)
    | forall e a. (Eq e, Show e, Eq a, Show a) => FailsWith (LedgerValidatorError e) (LedgerSim e a)
    | forall e a. Fails (LedgerSim e a)

toAssertion :: LedgerConfig -> LedgerState -> LedgerExpectation -> Assertion
toAssertion cfg st (SucceedsWith x sim) = runLedgerSim cfg st sim @?= Right x
toAssertion cfg st (FailsWith x sim) = runLedgerSim cfg st sim @?= Left x
toAssertion cfg st (Fails sim) =
    assertBool "Expected contract to fail" . isLeft $
        runLedgerSim cfg st sim
