{ inputs, ... }: {
  perSystem = { system, config, inputs', ... }:
    let
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
          devShellHook = config.settings.shell.hook;
          testTools = with inputs'; [ plutip.packages."plutip-core:exe:local-cluster" ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
