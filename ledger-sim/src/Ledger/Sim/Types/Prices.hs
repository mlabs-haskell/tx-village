module Ledger.Sim.Types.Prices (Prices (..), calcScriptFee) where

import PlutusLedgerApi.Common (ExBudget (exBudgetCPU, exBudgetMemory))
import PlutusLedgerApi.V1 (ExCPU (ExCPU), ExMemory (ExMemory), SatInt, fromSatInt)

data Prices = Prices
  { prMem :: Rational
  , prStep :: Rational
  }
  deriving stock (Show)

unExCpu :: ExCPU -> SatInt
unExCpu (ExCPU n) = n

unExMem :: ExMemory -> SatInt
unExMem (ExMemory n) = n

calcScriptFee :: Prices -> ExBudget -> Integer
calcScriptFee price exBudget =
  ceiling $
    prStep price * fromSatInt (unExCpu $ exBudgetCPU exBudget)
      + prMem price * fromSatInt (unExMem $ exBudgetMemory exBudget)
