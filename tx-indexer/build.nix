{ inputs, ... }: {
  perSystem = { system, config, inputs', pkgs, ... }:
    let
      postgresConf =
        pkgs.writeText "postgresql.conf"
          ''
            # Add Custom Settings
            log_min_messages = warning
            log_min_error_statement = error
            log_min_duration_statement = 100  # ms
            log_connections = on
            log_disconnections = on
            log_duration = on
            #log_line_prefix = '[] '
            log_timezone = 'UTC'
            log_statement = 'all'
            log_directory = 'pg_log'
            log_filename = 'postgresql-%Y-%m-%d_%H%M%S.log'
            logging_collector = on
            log_min_error_statement = error
          '';


      init-db = pkgs.writeShellApplication {
        name = "init-db";
        runtimeInputs = [ pkgs.postgresql_16 ];
        runtimeEnv = {
          LC_CTYPE = "en_US.UTF-8";
          LC_ALL = "en_US.UTF-8";
          LANG = "en_US.UTF-8";
        };
        text = ''
          export PGDATA="$PWD/.pg"
          [ ! -d "$PGDATA" ] && pg_ctl initdb -o "-U postgres" && cat "${postgresConf}" >> "$PGDATA/postgresql.conf"
        '';
      };

      start-db = pkgs.writeShellApplication {
        name = "start-db";
        runtimeInputs = [ pkgs.postgresql_16 ];
        text = ''
          export PGDATA="$PWD/.pg"
          pg_ctl -o "-p 5555 -k $PGDATA" start
        '';
      };

      stop-db = pkgs.writeShellApplication {
        name = "stop-db";
        runtimeInputs = [ pkgs.postgresql_16 ];
        text = ''
          export PGDATA="$PWD/.pg"
          pg_ctl stop && exit
        '';
      };

      pg = pkgs.writeShellApplication {
        name = "pg";
        runtimeInputs = [ pkgs.postgresql_16 ];
        text = ''
          export PGHOST="$PWD/.pg"
          psql -p 5555 -U postgres
        '';
      };
      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
          src = ./.;
          crateName = "tx-indexer";
          extraSources = [
            inputs'.plutus-ledger-api-rust.packages.plutus-ledger-api-rust-src

            # LB base schema and runtime libs
            inputs'.lbf.packages.lbf-prelude-rust
            inputs'.lbf.packages.lbf-plutus-rust
            inputs'.lbf.packages.lbr-prelude-rust-src
            inputs'.lbf.packages.lbr-prelude-derive-rust-src

            config.packages.tx-bakery-rust-src
          ];

          devShellTools = [
            init-db
            start-db
            stop-db
            pg
          ];

          devShellHook = config.settings.shell.hook;

          testTools = with inputs'; [ plutip.packages."plutip-core:exe:local-cluster" ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
