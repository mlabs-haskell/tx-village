module Demo.Mint (mintingPolicy) where

import LambdaBuffers.Runtime.Plutarch ()
import Plutarch (ClosedTerm, perror, plam, popaque, unTermCont)
import Plutarch.Api.V2 qualified as V2
import Plutarch.Prelude (PAsData, PEq ((#==)), PInteger, POpaque, Term, pconstant, pfromData, pif, ptrace, (:-->))

-- | `eqValidator dat rdmr ctx` is a simple minting policy allowing to mint when the the redeemer matches the "secret"
mintingPolicy :: ClosedTerm (PAsData PInteger :--> (V2.PScriptContext :--> POpaque))
mintingPolicy = plam $ \redeemer _ctx -> ptrace "[EQ]" $ unTermCont $ do
  let
    secret :: Term s PInteger
    secret = 1234

  let validates = pfromData redeemer #== secret

  pure $ pif validates (popaque (pconstant ())) (ptrace "[EQ] Validation failed" perror)
