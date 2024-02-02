{ inputs, ... }: {
  perSystem = { system, config, inputs', ... }:
    let
      rustFlake =
        inputs.lbf.lib."${system}".rustFlake {
          src = ./.;
          inherit (inputs) crane;
          crateName = "infinity-query";
          extraSources = [
            {
              name = "plutip";
              path = config.packages.plutip-rust-src;
            }
            {
              name = "infinity-transactions";
              path = config.packages.infinity-transactions-rust-src;
            }
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
