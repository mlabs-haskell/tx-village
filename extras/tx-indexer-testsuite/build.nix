{ inputs, ... }: {
  perSystem = { system, pkgs, config, inputs', self', ... }:
    let
      getDdls = path:
        builtins.filter builtins.pathExists
          (pkgs.lib.mapAttrsToList (name: _: ./${name}/up.sql) (builtins.readDir path));
      commands = import "${../..}/tx-indexer/commands.nix" {
        inherit pkgs;

        extraDDLs = getDdls "${./app-migrations}";
      };

      oura = pkgs.stdenv.mkDerivation {
        src = "${inputs.oura.outPath}";
        name = "oura-v1";
        unpackPhase = ''
          mkdir $out
          cp -r $src/* $out
          cd $out
        '';
      };

      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
          src = ./.;
          crateName = "tx-indexer-testsuite";
          extraSources = [
            config.packages.tx-bakery-rust-src
            config.packages.tx-bakery-ogmios-rust-src
            config.packages.tx-indexer-rust-src
            config.packages.diesel-derive-pg-rust-src
            oura
          ];
          devShellHook = config.settings.shell.hook + ''
            ln -sf ${../../tx-indexer}/lib-migrations
          '';

          buildInputs = [ pkgs.postgresql_16.lib ];

          inherit (commands) devShellTools;

          testTools = [
            pkgs.postgresql_16
            pkgs.diesel-cli
            pkgs.process-compose
            config.packages.cardano-devnet
            inputs'.ogmios.packages."ogmios:exe:ogmios"
            inputs'.cardano-node.packages.cardano-cli
            self'.packages.tx-indexer-tests
          ];

        };
    in
    {
      inherit (rustFlake) devShells packages;
      checks = {
        "tx-indexer-testsuite" = self'.packages.tx-indexer-tests;
      };

      cardano-devnet.initialFunds = {
        "60a5587dc01541d4ad17d7a4416efee274d833f2fc894eef79976a3d06" = 9000000000;
      };

      process-compose.tx-indexer-tests = {
        settings.processes = {
          tests = {
            command = "cargo test";
            availability.exit_on_end = true;
            depends_on = {
              cardano_devnet.condition = "process_healthy";
              ogmios.condition = "process_healthy";
              db.condition = "process_healthy";
              db_migration.condition = "process_completed_successfully";
            };
          };

          cardano_devnet = {
            command = "cardano-devnet";
            readiness_probe = {
              exec.command = "cardano-cli query tip --socket-path .devnet/node.socket --testnet-magic 42";
              initial_delay_seconds = 1;
              period_seconds = 1;
            };
          };

          ogmios = {
            command = "ogmios --node-socket .devnet/node.socket --node-config .devnet/config.json";
            readiness_probe = {
              http_get = {
                host = "127.0.0.1";
                port = 1337;
                path = "/health";
              };
              initial_delay_seconds = 2;
              period_seconds = 2;
            };
            depends_on.cardano_devnet.condition = "process_healthy";
          };

          db = {
            command = "init-empty-db";
            is_daemon = true;
            shutdown.command = "stop-db";
            readiness_probe = {
              exec.command = "pg_isready -p 5555 -h localhost";
              initial_delay_seconds = 2;
              period_seconds = 2;
            };
          };

          db_migration = {
            command = ''
              echo $PWD
              diesel migration run --database-url postgres://tx_indexer@127.0.0.1:5555 --migration-dir ./lib-migrations
              diesel migration run --database-url postgres://tx_indexer@127.0.0.1:5555 --migration-dir ./app-migrations
            '';
            depends_on.db.condition = "process_healthy";
          };

        };
      };

    };

}
