{ inputs, ... }: {
  imports = [ inputs.cardano-devnet-flake.flakeModule ];
  perSystem = { system, pkgs, config, inputs', ... }:
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
          ];

        };
    in
    {
      inherit (rustFlake) checks devShells packages;

      cardano-devnet.initialFunds = {
        "60a5587dc01541d4ad17d7a4416efee274d833f2fc894eef79976a3d06" = 9000000000;
      };

    };

}
