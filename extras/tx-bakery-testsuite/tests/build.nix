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
          self'.packages.pc-tx-bakery-tests
        ];

        devShellHook =
          config.settings.shell.hook
          + ''
            echo "TxBakery testsuite"
            echo ""
            echo "Run pc-tx-bakery-tests to execute the testsuite."
            echo "or pc-tx-bakery-tests up ogmios devnet to spin up an environment"
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
            name = "tx-bakery-testsuite-check";
            phases = [
              "unpackPhase"
              "checkPhase"
              "buildPhase"
            ];
            unpackPhase = ''
              echo "Linking data"
              ln -s ${data-drv} ./${dataDir}
              ln -s ${./wallets} ./wallets
            '';
            checkPhase = ''
              ${self'.packages.pc-tx-bakery-tests}/bin/pc-tx-bakery-tests --tui=false
            '';
            buildPhase = ''
              mkdir $out
            '';
            doCheck = true;
          };
        };

      process-compose.pc-tx-bakery-tests = {
        imports = [
          inputs.cardano-devnet.processComposeModule
        ];

        services = {
          cardano-devnet."devnet" = {
            inherit (inputs'.cardano-node.packages) cardano-node cardano-cli;
            enable = true;
            dataDir = ".devnet";
            initialFunds = {
              "a5587dc01541d4ad17d7a4416efee274d833f2fc894eef79976a3d06" = 9000000000;
            };
          };
        };

        settings.processes = {
          tests = {
            command = "
              ${self'.packages.tx-bakery-testsuite-rust-test}/bin/run_tests.sh
            ";
            depends_on = {
              devnet.condition = "process_healthy";
              ogmios.condition = "process_healthy";
            };
            availability = {
              exit_on_end = true;
              exit_on_skipped = true;
            };
          };

          ogmios = {
            command = ''
              ${inputs'.ogmios.packages."ogmios:exe:ogmios"}/bin/ogmios \
              --node-socket .devnet/node.socket \
              --node-config .devnet/config.json
            '';
            readiness_probe = {
              http_get = {
                host = "127.0.0.1";
                port = 1337;
                path = "/health";
              };
              initial_delay_seconds = 2;
              period_seconds = 2;
            };
            depends_on.devnet.condition = "process_healthy";
          };

        };
      };
    };
}
