module Ledger.Sim.Types.Prices (Prices (..), ExBudgetRatio (..)) where

data Prices = Prices
  { prMem :: Rational
  , prStep :: Rational
  }
  deriving stock (Show, Eq)

data ExBudgetRatio = ExBudgetRatio
  { exBudgetRatio'Mem :: Rational
  , exBudgetRatio'Cpu :: Rational
  }
