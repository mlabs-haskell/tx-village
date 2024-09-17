{ inputs, ... }: {
  perSystem = { system, config, inputs', pkgs, ... }:
    let
      commands = import ./commands.nix {
        inherit pkgs;
        extraDDLs =
          builtins.filter builtins.pathExists
            (map ({ name, ... }: ./${name}/up.sql)
              (pkgs.lib.attrsToList (builtins.readDir ./app-migrations)));
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
            config.packages.diesel-derive-pg-rust-src
          ];

          inherit (commands) devShellTools;

          devShellHook = config.settings.shell.hook;

          buildInputs = [ pkgs.postgresql ];

          testTools = with inputs'; [
            ogmios.packages."ogmios:exe:ogmios"
            pkgs.diesel-cli
          ];

          cargoNextestExtraArgs = "-E 'not test(database)'";
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
