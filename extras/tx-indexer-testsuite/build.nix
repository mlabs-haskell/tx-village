{ inputs, ... }:
{
  perSystem =
    {
      system,
      pkgs,
      config,
      inputs',
      self',
      ...
    }:
    let
      rustFlake = inputs.flake-lang.lib.${system}.rustFlake {
        src = ./.;
        crateName = "tx-indexer-testsuite";
        exportTests = true;
        extraSources = [
          config.packages.tx-bakery-rust-src
          config.packages.tx-bakery-ogmios-rust-src
          config.packages.tx-indexer-rust-src
          config.packages.diesel-derive-pg-rust-src
        ];
        buildInputs = [ pkgs.postgresql_16.lib ];

        devShellTools = [
          self'.packages.pc-tx-indexer-tests
        ];

        devShellHook =
          config.settings.shell.hook
          + ''
            ln -sf ${../../tx-indexer}/lib-migrations

            echo "TxIndexer testsuite"
            echo ""
            echo "Run pc-tx-indexer-tests to execute the testsuite."
            echo "or pc-tx-indexer-tests up db_migration ogmios devnet to spin up an environment"
            echo ""
          '';
      };
    in
    {
      inherit (rustFlake) devShells packages;

      checks = {
        "tx-indexer-testsuite" = pkgs.stdenv.mkDerivation {
          name = "tx-indexer-testsuite";
          phases = [
            "unpackPhase"
            "checkPhase"
            "buildPhase"
          ];
          unpackPhase = ''
            echo "Linking data"
            ln -s ${./wallets} ./wallets
            mkdir ./tests
            ln -s ${./tests/fixtures} ./tests/fixtures
          '';
          checkPhase = ''
            ${self'.packages.pc-tx-indexer-tests}/bin/pc-tx-indexer-tests --tui=false
          '';
          buildPhase = ''
            mkdir $out
          '';
          doCheck = true;
        };
      };

      process-compose.pc-tx-indexer-tests = {
        imports = [
          inputs.services-flake.processComposeModules.default
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
          postgres."db" = {
            enable = true;
            port = 5555;
            package = pkgs.postgresql_16;
            dataDir = ".pg";
            initialDatabases = [
              {
                name = "tx_indexer";
                schemas = [ ];
              }
            ];
          };
        };
        settings.processes = {
          tests = {
            command = "${self'.packages.tx-indexer-testsuite-rust-test}/bin/run_tests.sh";
            depends_on = {
              devnet.condition = "process_healthy";
              ogmios.condition = "process_healthy";
              db.condition = "process_healthy";
              db_migration.condition = "process_completed_successfully";
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

          db_migration = {
            command = ''
              ${pkgs.diesel-cli}/bin/diesel migration run \
                --database-url postgres://127.0.0.1:5555/tx_indexer \
                --migration-dir ${../../tx-indexer/lib-migrations}

              ${pkgs.diesel-cli}/bin/diesel migration run \
                --database-url postgres://127.0.0.1:5555/tx_indexer \
                --migration-dir ${./app-migrations}
            '';
            depends_on.db.condition = "process_healthy";
          };

        };
      };

    };

}
