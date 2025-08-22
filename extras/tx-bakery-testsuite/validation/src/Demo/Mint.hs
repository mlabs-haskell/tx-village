module Demo.Mint (mintingPolicy) where

import LambdaBuffers.Runtime.Plutarch ()
import Plutarch.LedgerApi.V1 (PRedeemer (PRedeemer))
import Plutarch.LedgerApi.V3 qualified as V3
import Plutarch.Prelude (
  ClosedTerm,
  PAsData,
  PEq ((#==)),
  PInteger,
  PUnit (PUnit),
  Term,
  pcon,
  perror,
  pfromData,
  pif,
  plam,
  pmatchC,
  ptraceInfo,
  ptryFromC,
  unTermCont,
  (:-->),
 )

{- | `eqValidator dat rdmr ctx` is a simple minting policy allowing to mint
      when the the redeemer matches the "secret"
      Note: please don't get inspired by this implementation, anyone can see the secret
-}
mintingPolicy :: ClosedTerm (V3.PScriptContext :--> PUnit)
mintingPolicy = plam $ \ctx' -> ptraceInfo "[EQ]" $ unTermCont $ do
  let
    secret :: Term s PInteger
    secret = 1234

  ctx <- pmatchC ctx'
  PRedeemer redeemer <- pmatchC $ V3.pscriptContext'redeemer ctx
  check <- pfromData . fst <$> ptryFromC @(PAsData PInteger) redeemer

  let validates = check #== secret

  pure $ pif validates (pcon PUnit) (ptraceInfo "[EQ] Validation failed" perror)
