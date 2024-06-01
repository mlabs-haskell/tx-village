module Ledger.Sim.Test (
    LedgerExpectation,
    ledgerTestGroup,
    ledgerTestCase,
    ledgerSucceeds,
    ledgerSucceedsWith,
    ledgerFails,
    ledgerFailsWith,
) where

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

ledgerFails :: LedgerSim ctx st e () -> LedgerExpectation ctx st
ledgerFails = Fails

ledgerFailsWith :: (Eq e, Show e, Eq a, Show a) => LedgerValidatorError e -> LedgerSim ctx st e a -> LedgerExpectation ctx st
ledgerFailsWith = FailsWith

data LedgerExpectation ctx st
    = forall e a. (Eq e, Show e, Eq a, Show a) => SucceedsWith a (LedgerSim ctx st e a)
    | forall e a. (Eq e, Show e, Eq a, Show a) => FailsWith (LedgerValidatorError e) (LedgerSim ctx st e a)
    | forall e a. Fails (LedgerSim ctx st e a)

toAssertion :: LedgerConfig ctx -> LedgerState st -> LedgerExpectation ctx st -> Assertion
toAssertion cfg st (SucceedsWith x sim) = runLedgerSim cfg st sim @?= Right x
toAssertion cfg st (FailsWith x sim) = runLedgerSim cfg st sim @?= Left x
toAssertion cfg st (Fails sim) =
    assertBool "Expected contract to fail" . isLeft $
        runLedgerSim cfg st sim
