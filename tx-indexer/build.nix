{ inputs, ... }: {
  perSystem = { system, config, inputs', pkgs, ... }:
    let
      commands = import ./commands.nix {
        inherit pkgs;
        extraDDLs = [ ./db/utxo-indexer.sql ];
      };

      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
          src = ./.;
          crateName = "tx-indexer";
          extraSources = [
            # LB base schema and runtime libs
            inputs'.lbf.packages.lbf-prelude-rust
            inputs'.lbf.packages.lbf-plutus-rust

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
