{ inputs, ... }: {
  perSystem = { system, config, inputs', ... }:
    let
      rustFlake =
        inputs.lbf.lib."${system}".rustFlake {
          src = ./.;
          crane = inputs.crane;
          crateName = "infinity-query";
          devShellHook = config.settings.shell.hook;
          devShellTools = with inputs'; [ plutip.packages."plutip-core:exe:local-cluster" ];
          testTools = with inputs'; [ plutip.packages."plutip-core:exe:local-cluster" ];
        };
    in
    {
      inherit (rustFlake) packages checks devShells;
    };
}
