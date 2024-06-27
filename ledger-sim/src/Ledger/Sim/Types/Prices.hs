module Ledger.Sim.Types.Prices (Prices (..)) where

data Prices = Prices
  { prMem :: Rational
  , prStep :: Rational
  }
  deriving stock (Show, Eq)
