{ inputs, ... }: {
  perSystem = { system, pkgs, config, ... }:
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

          devShellTools = commands.devShellTools ++ [
            pkgs.diesel-cli
            pkgs.postgresql_16
            pkgs.process-compose
          ];
        };
    in
    {
      inherit (rustFlake) checks devShells packages;

    };

}
