{ inputs, ... }:
{
  perSystem =
    {
      lib,
      inputs',
      self',
      ...
    }:

    let
      environments = {
        "tx-bakery-tests" = {
          testsuite = "cargo test";

        };
        "tx-bakery-tests-ci" = {
          testsuite = "${self'.packages.tx-bakery-testsuite-rust-test}/bin/run_tests.sh";

        };
      };

      perEnvironment = envName: {
        ${envName} = {
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
              command = environments.${envName}.testsuite;
              depends_on = {
                devnet.condition = "process_healthy";
                ogmios.condition = "process_healthy";
              }
              // lib.optionalAttrs (envName == "tx-bakery-tests") {
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

          }
          // lib.optionalAttrs (envName == "tx-bakery-tests") {
            build = {
              command = "cargo build --tests";
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
