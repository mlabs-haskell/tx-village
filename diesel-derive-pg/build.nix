{ inputs, ... }: {
  perSystem = { system, pkgs, config, ... }:
    let
      rustFlake =
        inputs.flake-lang.lib.${system}.rustFlake {
          src = ./.;
          crateName = "diesel-derive-pg";

          buildInputs = [ pkgs.postgresql.lib ];

          devShellHook = config.settings.shell.hook;
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
