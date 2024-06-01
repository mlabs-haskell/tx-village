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

import Control.Monad.Trans.Reader (Reader, ask, runReader)

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import Ledger.Sim (
    LedgerConfig,
    LedgerSim,
    LedgerState,
    LedgerValidatorError (TxApplicationError),
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
ledgerFails = ledgerFailsBy $ const True

ledgerFailsBy :: (LedgerValidatorError e -> Bool) -> LedgerSim ctx st e () -> LedgerExpectation ctx st
ledgerFailsBy = FailsBy

ledgerFailsBy' :: (e -> Bool) -> LedgerSim ctx st e () -> LedgerExpectation ctx st
ledgerFailsBy' predicate = ledgerFailsBy $ \case TxApplicationError e -> predicate e; _ -> False

ledgerFailsWith :: (Eq e, Show e, Eq a, Show a) => LedgerValidatorError e -> LedgerSim ctx st e a -> LedgerExpectation ctx st
ledgerFailsWith = FailsWith

ledgerFailsWith' :: (Eq e, Show e, Eq a, Show a) => e -> LedgerSim ctx st e a -> LedgerExpectation ctx st
ledgerFailsWith' = ledgerFailsWith . TxApplicationError

data LedgerExpectation ctx st
    = forall e a. (Eq e, Show e, Eq a, Show a) => SucceedsWith a (LedgerSim ctx st e a)
    | forall e a. (Eq e, Show e, Eq a, Show a) => FailsWith (LedgerValidatorError e) (LedgerSim ctx st e a)
    | forall e a. FailsBy (LedgerValidatorError e -> Bool) (LedgerSim ctx st e a)

toAssertion :: LedgerConfig ctx -> LedgerState st -> LedgerExpectation ctx st -> Assertion
toAssertion cfg st (SucceedsWith x sim) = runLedgerSim cfg st sim @?= Right x
toAssertion cfg st (FailsWith x sim) = runLedgerSim cfg st sim @?= Left x
toAssertion cfg st (FailsBy predicate sim) =
    assertBool "Expected contract to fail" . either predicate (const False) $
        runLedgerSim cfg st sim
