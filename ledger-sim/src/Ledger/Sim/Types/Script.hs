module Ledger.Sim.Types.Script (Plutus.ScriptForEvaluation, Plutus.ScriptHash, hashScriptV2) where

import Data.ByteString (ByteString)
import Data.String (fromString)

import Crypto.Hash (Blake2b_224 (Blake2b_224), hashWith)
import Data.ByteArray (convert)
import Data.ByteString.Short qualified as SBS
import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus

-- | Hash a Script with the given version prefix
hashScriptV2 :: Plutus.ScriptForEvaluation -> Plutus.ScriptHash
hashScriptV2 scr =
    Plutus.ScriptHash
        . Plutus.toBuiltin
        . convert @_ @ByteString
        . hashWith Blake2b_224
        $ fromString "\x02" <> SBS.fromShort (Plutus.serialisedScript scr)
