{ inputs, ... }:
{
  perSystem =
    {
      config,
      system,
      ...
    }:

    let
      rustFlake = inputs.flake-lang.lib."${system}".rustFlake {
        src = ./.;
        version = "v2";
        crateName = "tx-bakery";
        runTests = false;

        devShellHook = config.settings.shell.hook;
      };
    in
    {

      inherit (rustFlake) packages checks devShells;

    };
}
