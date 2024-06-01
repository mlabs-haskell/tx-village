module Ledger.Sim.Utils.Hashing (hashScriptV2, hashDatum) where

import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.String (fromString)

import Codec.Serialise (serialise)
import Crypto.Hash (Blake2b_224 (Blake2b_224), Blake2b_256 (Blake2b_256), hashWith)

import PlutusLedgerApi.Common qualified as Plutus
import PlutusLedgerApi.V2 qualified as Plutus

-- | Hash a V2 Plutus Script.
hashScriptV2 :: Plutus.ScriptForEvaluation -> Plutus.ScriptHash
hashScriptV2 scr =
    Plutus.ScriptHash
        . Plutus.toBuiltin
        . convert @_ @ByteString
        . hashWith Blake2b_224
        $ fromString "\x02" <> SBS.fromShort (Plutus.serialisedScript scr)

-- | Hash a Datum.
hashDatum :: Plutus.Datum -> Plutus.DatumHash
hashDatum =
    Plutus.DatumHash
        . Plutus.toBuiltin
        . convert @_ @ByteString
        . hashWith Blake2b_256
        . LBS.toStrict
        . serialise
