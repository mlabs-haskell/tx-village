{ inputs, ... }: {
  perSystem = { config, system, inputs', ... }:

    let
      rustFlake = inputs.flake-lang.lib."${system}".rustFlake
        {
          src = ./.;
          crateName = "tx-bakery-tests";
          cargoNextestExtraArgs = "--no-capture";

          testTools = [
            inputs'.ogmios.packages."ogmios:exe:ogmios"
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
    };
}
