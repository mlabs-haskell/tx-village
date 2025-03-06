{ inputs, ... }:
{
  perSystem =
    { system, ... }:
    {
      packages = {
        lbf-tx-bakery-tests-plutus-api-plutarch = inputs.lbf.lib."${system}".lbfPlutarch {
          name = "lbf-tx-bakery-tests-plutus-api-plutarch";
          src = ./.;
          files = [ "Demo/Plutus.lbf" ];
        };

        lbf-tx-bakery-tests-plutus-api-rust = inputs.lbf.lib."${system}".lbfPlutusRust {
          name = "lbf-tx-bakery-tests-plutus-api";
          src = ./.;
          files = [ "Demo/Plutus.lbf" ];
        };

        lbf-tx-bakery-tests-config-api-haskell = inputs.lbf.lib."${system}".lbfPreludeHaskell {
          name = "lbf-tx-bakery-tests-config-api";
          src = ./.;
          files = [ "Demo/Config.lbf" ];
        };

        lbf-tx-bakery-tests-config-api-rust = inputs.lbf.lib."${system}".lbfPreludeRust {
          name = "lbf-tx-bakery-tests-config-api";
          src = ./.;
          files = [ "Demo/Config.lbf" ];
        };
      };
    };
}
