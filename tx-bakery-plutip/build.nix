{ inputs, ... }: {
  perSystem = { config, system, ... }:

    let
      rustFlake = inputs.flake-lang.lib."${system}".rustFlake
        {
          src = ./.;
          crateName = "tx-bakery-plutip";
          cargoNextestExtraArgs = "--no-capture";

          extraSources = [
            config.packages.tx-bakery-rust-src
          ];

          devShellHook = config.settings.shell.hook;
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
