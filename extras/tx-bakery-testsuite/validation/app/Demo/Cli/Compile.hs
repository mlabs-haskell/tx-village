module Demo.Cli.Compile (CompileOpts (..), CompileMode (..), compile) where

import Data.ByteString qualified
import Data.ByteString.Short (fromShort)
import Demo.Mint (mintingPolicy)
import Demo.RefInput (refInputMintingPolicy)
import Demo.Validation (eqValidator)
import LambdaBuffers.Demo.Config (Config (Config, config'eqValidator, config'mintingPolicy, config'refInputMintingPolicy), Script (Script))
import LambdaBuffers.Runtime.Prelude (toJsonBytes)
import Plutarch qualified (Config (Config), TracingMode (DoTracing, NoTracing), compile)
import Plutarch.Script (serialiseScript)

data CompileMode = COMPILE_PROD | COMPILE_DEBUG deriving stock (Show, Read, Eq)

data CompileOpts = CompileOpts
  { co'Mode :: CompileMode
  , co'File :: FilePath
  }
  deriving stock (Show, Eq)

compile :: CompileOpts -> IO ()
compile opts = do
  let cfg = case co'Mode opts of
        COMPILE_PROD -> Plutarch.Config Plutarch.NoTracing
        COMPILE_DEBUG -> Plutarch.Config Plutarch.DoTracing

  eqValidatorCompiled <- either (\err -> fail $ "Failed compiling eqValidator with " <> show err) pure (Plutarch.compile cfg eqValidator)
  mintingPolicyCompiled <- either (\err -> fail $ "Failed compiling mintingPolicy with " <> show err) pure (Plutarch.compile cfg mintingPolicy)
  refInputMintingPolicyCompiled <- either (\err -> fail $ "Failed compiling refInputMintingPolicy with " <> show err) pure (Plutarch.compile cfg refInputMintingPolicy)

  let config =
        toJsonBytes $
          Config
            { config'eqValidator = Script (fromShort . serialiseScript $ eqValidatorCompiled)
            , config'mintingPolicy = Script (fromShort . serialiseScript $ mintingPolicyCompiled)
            , config'refInputMintingPolicy = Script (fromShort . serialiseScript $ refInputMintingPolicyCompiled)
            }
  Data.ByteString.writeFile (co'File opts) config
