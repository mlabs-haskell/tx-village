{
  pkgs,
  extraDDLs ? [ ],
  pgUser ? "tx_indexer",
  pgPort ? "5555",
  pgDir ? ".pg",
  postgresql ? pkgs.postgresql_16,
  extraPostgresConf ? "",
  schemaDumpIncludePlutus ? false,
}:
let
  postgresConf = pkgs.writeText "postgresql.conf" ''
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

    ${extraPostgresConf}
  '';

  ddls = [
    ./lib-migrations/00000000000001_plutus/up.sql
    ./lib-migrations/00000000000002_sync_progress/up.sql
  ]
  ++ extraDDLs;

  init-db = pkgs.writeShellApplication {
    name = "init-db";
    runtimeInputs = [ postgresql ];
    runtimeEnv = {
      LC_CTYPE = "en_US.UTF-8";
      LC_ALL = "en_US.UTF-8";
      LANG = "en_US.UTF-8";
    };
    text = "init-empty-db; " + pkgs.lib.concatMapStringsSep "\n" (ddl: "pg < ${ddl}") ddls;
  };

  init-empty-db = pkgs.writeShellApplication {
    name = "init-empty-db";
    runtimeInputs = [ postgresql ];
    runtimeEnv = {
      LC_CTYPE = "en_US.UTF-8";
      LC_ALL = "en_US.UTF-8";
      LANG = "en_US.UTF-8";
    };
    text = ''
      export PGDATA="$PWD/${pgDir}"
      export PGHOST="$PWD/${pgDir}"
      [ ! -d "$PGDATA" ] && pg_ctl initdb -o "-U ${pgUser}" && cat "${postgresConf}" >> "$PGDATA/postgresql.conf"

      start-db
      echo "SELECT 'CREATE DATABASE ${pgUser}' WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = '${pgUser}')\gexec" \
        | psql -p "${pgPort}" -U "${pgUser}" postgres

    '';
  };

  start-db = pkgs.writeShellApplication {
    name = "start-db";
    runtimeInputs = [ postgresql ];
    text = ''
      export PGDATA="$PWD/${pgDir}";
      pg_ctl -o "-p ${pgPort} -k $PGDATA" start
    '';
  };

  stop-db = pkgs.writeShellApplication {
    name = "stop-db";
    runtimeInputs = [ postgresql ];
    text = ''
      export PGDATA="$PWD/${pgDir}";
      pg_ctl stop && exit
    '';
  };

  pg = pkgs.writeShellApplication {
    name = "pg";
    runtimeInputs = [ postgresql ];
    text = ''
      export PGHOST="$PWD/${pgDir}";
      psql -p ${pgPort} -U ${pgUser}
    '';
  };

  dump-schema = pkgs.writeShellApplication {
    name = "dump-schema";
    runtimeInputs = [ postgresql ];
    text = ''
      pg_dump -sx \
        -U ${pgUser} \
        -h 127.0.0.1 \
        -p ${pgPort} \
        ${pkgs.lib.optionalString (!schemaDumpIncludePlutus) "-N plutus"}
    '';
  };

in
{
  inherit postgresConf;
  devShellTools = [
    init-db
    start-db
    stop-db
    pg
    init-empty-db
    dump-schema
  ];
}
