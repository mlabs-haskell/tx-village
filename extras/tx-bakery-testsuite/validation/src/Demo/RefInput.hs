module Demo.RefInput (refInputMintingPolicy) where

import LambdaBuffers.Demo.Plutus.Plutarch (
  EqDatum,
  RefInputRedeemer (RefInputRedeemer),
 )
import LambdaBuffers.Runtime.Plutarch ()
import LambdaBuffers.Runtime.Plutarch.LamVal (pfromPlutusDataPTryFrom)
import Plutarch.LedgerApi.V1 (PRedeemer (PRedeemer))
import Plutarch.LedgerApi.V3 qualified as V3
import Plutarch.Prelude (
  ClosedTerm,
  PBool (PFalse),
  PEq ((#==)),
  PMaybe (PJust),
  PUnit (PUnit),
  pcon,
  perror,
  pfind,
  pfromData,
  pif,
  plam,
  pletC,
  pmatch,
  pmatchC,
  ptraceInfo,
  unTermCont,
  (#),
  (:-->),
 )

{- | `eqValidator dat rdmr ctx` is a simple minting policy allowing to mint when
      the the redeemer
      has a reference input to an EqValidator UTxO with a given inline datum
-}
refInputMintingPolicy :: ClosedTerm (V3.PScriptContext :--> PUnit)
refInputMintingPolicy = plam $ \ctx' -> ptraceInfo "[RefMint]" $ unTermCont $ do
  ctx <- pmatchC ctx'
  info <- pmatchC $ V3.pscriptContext'txInfo ctx
  let refInputs = pfromData $ V3.ptxInfo'referenceInputs info

  PRedeemer redeemer <- pmatchC $ V3.pscriptContext'redeemer ctx

  RefInputRedeemer refEqDatum refTxOutRef <-
    pmatchC $ pfromData $ pfromPlutusDataPTryFrom @RefInputRedeemer # redeemer

  PJust refInput' <-
    pmatchC $
      pfind
        # plam
          ( \txInInfo' -> pmatch (pfromData txInInfo') $ \txInInfo ->
              V3.ptxInInfo'outRef txInInfo #== pfromData refTxOutRef
          )
        # refInputs

  let validates =
        pmatch (pfromData refInput') $ \refInput ->
          pmatch (V3.ptxInInfo'resolved refInput) $ \resolved ->
            pmatch (V3.ptxOut'datum resolved) $
              \case
                V3.POutputDatum datum' -> unTermCont $ do
                  V3.PDatum stored <- pmatchC datum'
                  storedEqDatum <-
                    pletC $
                      pfromData $
                        pfromPlutusDataPTryFrom @EqDatum # stored
                  pure $ pfromData refEqDatum #== storedEqDatum
                _ -> pcon PFalse

  pure $
    pif
      validates
      (pcon PUnit)
      (ptraceInfo "[RefMint] Validation failed" perror)
