{ inputs, ... }: {
  perSystem = { system, config, inputs', pkgs, ... }:
    let
      commands = import ./commands.nix {
        inherit pkgs;
        extraDDLs = [ ./db/utxo-indexer.sql ];
      };

      # Using the latest alpha of sqlx from GitHub where this is fixed: https://github.com/launchbadge/sqlx/issues/1031
      sqlx =
        pkgs.stdenv.mkDerivation
          {
            src = inputs.sqlx.outPath;
            name = "sqlx-0.8.0";
            unpackPhase = ''
              mkdir $out
              cp -r $src/* $out
            '';
          };
      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
          src = ./.;
          crateName = "tx-indexer";
          extraSources = [
            # LB base schema and runtime libs
            inputs'.lbf.packages.lbf-prelude-rust
            inputs'.lbf.packages.lbf-plutus-rust

            sqlx

            config.packages.tx-bakery-rust-src
          ];

          inherit (commands) devShellTools;

          devShellHook = config.settings.shell.hook;

          testTools = with inputs'; [
            plutip.packages."plutip-core:exe:local-cluster"
            ogmios.packages."ogmios:exe:ogmios"
          ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
