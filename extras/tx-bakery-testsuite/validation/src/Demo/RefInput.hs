module Demo.RefInput (refInputMintingPolicy) where

import LambdaBuffers.Demo.Plutus.Plutarch (EqDatum, RefInputRedeemer (RefInputRedeemer))
import LambdaBuffers.Runtime.Plutarch ()
import LambdaBuffers.Runtime.Plutarch.LamVal (pfromPlutusDataPTryFrom)
import Plutarch (ClosedTerm, perror, plam, popaque, unTermCont)
import Plutarch.Api.V2 qualified as V2
import Plutarch.Prelude (PBool (PFalse), PEq ((#==)), PMaybe (PJust), pcon, pconstant, pfield, pfind, pfromData, pif, pletC, pmatch, pmatchC, ptrace, (#))

{- | `eqValidator dat rdmr ctx` is a simple minting policy allowing to mint when the the redeemer
has a reference input to an EqValidator UTxO with a given inline datum
-}
refInputMintingPolicy :: ClosedTerm V2.PMintingPolicy
refInputMintingPolicy = plam $ \redeemer ctx -> ptrace "[RefMint]" $ unTermCont $ do
  RefInputRedeemer refEqDatum refTxOutRef <-
    pmatchC $ pfromData $ pfromPlutusDataPTryFrom @RefInputRedeemer # redeemer

  refInputs <- pletC $ pfield @"referenceInputs" # (pfield @"txInfo" # ctx)

  PJust refInput <-
    pmatchC $
      pfind
        # plam (\txInInfo -> (pfield @"outRef" # txInInfo) #== pfromData refTxOutRef)
        # pfromData refInputs

  let validates =
        pmatch
          (pfield @"datum" # (pfield @"resolved" # refInput))
          $ \case
            V2.POutputDatum datum -> unTermCont $ do
              V2.PDatum stored <- pmatchC $ pfield @"outputDatum" # datum
              storedEqDatum <- pletC $ pfromData $ pfromPlutusDataPTryFrom @EqDatum # stored
              pure $ pfromData refEqDatum #== storedEqDatum
            _ -> pcon PFalse

  pure $ pif validates (popaque (pconstant ())) (ptrace "[RefMint] Validation failed" perror)
