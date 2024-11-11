{ inputs, ... }: {
  perSystem = { pkgs, config, system, inputs', ... }:

    let
      rustFlake = inputs.flake-lang.lib."${system}".rustFlake
        {
          src = ./.;
          crateName = "tx-bakery-tests";
          cargoNextestExtraArgs = "--no-capture";

          testTools = [
            config.packages.cardano-devnet
            inputs'.ogmios.packages."ogmios:exe:ogmios"
            inputs'.cardano-node.packages.cardano-cli
            pkgs.process-compose
          ];

          extraSources = [
            config.packages.tx-bakery-rust-src
            config.packages.tx-bakery-ogmios-rust-src

            # LB base schema and runtime libs
            inputs'.lbf.packages.lbf-prelude-rust
            inputs'.lbf.packages.lbf-plutus-rust

            # Demo API
            config.packages.lbf-tx-bakery-tests-config-api-rust
            config.packages.lbf-tx-bakery-tests-plutus-api-rust
          ];

          data = [
            {
              name = "tx-bakery-test-scripts-config.json";
              path = config.packages.tx-bakery-test-scripts-config;
            }
          ];

          devShellHook = config.settings.shell.hook;
        };
    in
    {
      inherit (rustFlake) packages checks devShells;

      cardano-devnet.initialFunds = {
        "60a5587dc01541d4ad17d7a4416efee274d833f2fc894eef79976a3d06" = 9000000000;
      };
    };
}
