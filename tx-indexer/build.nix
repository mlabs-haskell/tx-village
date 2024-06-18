{ inputs, ... }: {
  perSystem = { system, config, inputs', pkgs, ... }:
    let
      commands = import ./commands.nix {
        inherit pkgs;
        extraDDLs = [ ./db/tx-indexer.sql ];
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

          devShellTools = with commands;[
            init-db
            start-db
            stop-db
            pg
          ];

          devShellHook = config.settings.shell.hook;

          testTools = with inputs'; [
            plutip.packages."plutip-core:exe:local-cluster"
            inputs'.ogmios.packages."ogmios:exe:ogmios"
          ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
