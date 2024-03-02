{ inputs, ... }: {
  perSystem = { system, config, inputs', ... }:
    let
      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
          src = ./.;
          crateName = "infinity-query";
          extraSources = [
            config.packages.infinity-transactions-rust-src
            config.packages.plutip-rust-src
            config.packages.lbf-infinity-plutus-api-rust
            inputs'.lbf.packages.lbf-prelude-rust
            inputs'.lbf.packages.lbf-plutus-rust
            inputs'.lbf.packages.lbr-prelude-rust-src
            inputs'.lbf.packages.lbr-prelude-derive-rust-src
            inputs'.plutus-ledger-api-rust.packages.plutus-ledger-api-rust-src
          ];
          devShellHook = config.settings.shell.hook;
          devShellTools = with inputs'; [ plutip.packages."plutip-core:exe:local-cluster" ];
          testTools = with inputs'; [ plutip.packages."plutip-core:exe:local-cluster" ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
