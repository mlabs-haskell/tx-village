{ inputs, ... }: {
  perSystem = { system, config, inputs', pkgs, ... }:
    let
      commands = import ./commands.nix {
        inherit pkgs;
        # extraDDLs = [ ./db/utxo-indexer.sql ./db/testdb.sql ];
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
            config.packages.tx-bakery-ogmios-rust-src
          ];

          inherit (commands) devShellTools;

          devShellHook = config.settings.shell.hook;

          buildInputs = [ pkgs.postgresql ];

          testTools = with inputs'; [
            ogmios.packages."ogmios:exe:ogmios"
            pkgs.diesel-cli
          ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
