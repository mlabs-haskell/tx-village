{ inputs, ... }:
{
  perSystem =
    {
      pkgs,
      lib,
      inputs',
      self',
      ...
    }:

    let
      environments = {
        "tx-indexer-tests" = {
          testsuite = "cargo test";

        };
        "tx-indexer-tests-ci" = {
          testsuite = "${self'.packages.tx-indexer-testsuite-rust-test}/bin/run_tests.sh";

        };
      };

      perEnvironment = envName: {
        ${envName} = {

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

          settings.processes =
            {
              tests = {
                command = environments.${envName}.testsuite;
                depends_on =
                  {
                    devnet.condition = "process_healthy";
                    ogmios.condition = "process_healthy";
                    db.condition = "process_healthy";
                    db_migration.condition = "process_completed_successfully";
                  }
                  // lib.optionalAttrs (envName == "tx-indexer-tests") {
                    build.condition = "process_completed";
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
            }
            // lib.optionalAttrs (envName == "tx-indexer-tests") {
              build = {
                command = "cargo build";
              };
            };
        };
      };

    in
    {
      process-compose = lib.mergeAttrsList (
        builtins.map perEnvironment (builtins.attrNames environments)
      );

    };
}
