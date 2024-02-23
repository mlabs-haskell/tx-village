{ inputs, ... }: {
  perSystem = { system, config, inputs', ... }:
    let
      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
          src = ./.;
          crateName = "infinity-query";
          extraSources = [
            config.packages.plutip-rust-src
            config.packages.infinity-transactions-rust-src
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
