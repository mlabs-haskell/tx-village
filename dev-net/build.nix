{
  flake-parts-lib,
  self,
  withSystem,
  lib,
  ...
}:
{
  perSystem =
    { pkgs, system, ... }:
    lib.mkIf (system == "x86_64-linux") {
      checks.dev-net-nixos-test = pkgs.nixosTest (
        import ./test.nix {
          inherit flake-parts-lib self withSystem;
        }
      );
    };

  flake = {
    nixosModules.dev-net = import ./cardano-dev-net.nix {
      inherit flake-parts-lib self withSystem;
    };
  };
}
