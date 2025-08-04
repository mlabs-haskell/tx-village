{ inputs, ... }:
{
  perSystem =
    {
      pkgs,
      config,
      system,
      inputs',
      self',
      ...
    }:

    let
      data = [
        {
          name = "tx-bakery-test-scripts-config.json";
          path = config.packages.tx-bakery-test-scripts-config;
        }
      ];
      dataDir = "data";

      rustFlake = inputs.flake-lang.lib."${system}".rustFlake {
        inherit data;
        src = ./.;
        crateName = "tx-bakery-testsuite";
        exportTests = true;

        extraSources = [
          config.packages.tx-bakery-rust-src
          config.packages.tx-bakery-ogmios-rust-src

          # LB base schema and runtime libs
          inputs'.lbf.packages.lbf-prelude-rust
          inputs'.lbf.packages.lbf-plutus-rust

          # Demo API
          config.packages.lbf-tx-bakery-tests-config-api-rust
          config.packages.lbf-tx-bakery-tests-plutus-api-rust
        ];

        devShellTools = [
          self'.packages.tx-bakery-tests
        ];

        devShellHook =
          config.settings.shell.hook
          + ''
            echo "TxBakery testsuite"
            echo ""
            echo "Run tx-bakery-tests to execute the testsuite."
            echo "or tx-bakery-tests up ogmios devnet to spin up an environment"
            echo ""
          '';

      };
    in
    {
      inherit (rustFlake) packages devShells;

      checks =
        let
          data-drv = pkgs.linkFarm "data" data;
        in
        {
          "tx-bakery-testsuite" = pkgs.stdenv.mkDerivation {
            name = "tx-bakery-testsuite";
            unpackPhase = ''
              echo "Linking data"
              ln -s ${data-drv} ./${dataDir}
              ln -s ${./wallets} ./wallets
            '';
            checkPhase = ''
              ${self'.packages.tx-bakery-tests-ci}/bin/tx-bakery-tests-ci --tui=false
            '';
            buildPhase = ''
              mkdir $out
            '';
            doCheck = true;
          };
        };
    };
}
