{ inputs, ... }:
{
  perSystem =
    { config, system, ... }:
    let
      hsFlake = inputs.flake-lang.lib.${system}.haskellPlutusFlake {
        src = ./.;
        name = "ledger-sim";
        inherit (config.settings.haskell) index-state compiler-nix-name;

        devShellTools = config.settings.shell.tools;

        devShellHook = config.settings.shell.hook;
      };
    in
    {
      devShells.dev-ledger-sim = hsFlake.devShells.default;

      packages = {
        # WARN(bladyjoker): We have to pick the hsFlake.packages like this otherwise flake-parts goes into `infinite recursion`.
        ledger-sim-lib = hsFlake.packages."ledger-sim:lib:ledger-sim";

        # TODO(chase): Enable this once we have a CLI
        # ledger-sim-cli = hsFlake.packages."ledger-sim:exe:ledger-sim-cli";
      };

      inherit (hsFlake) checks;
    };
}
