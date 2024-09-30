{ flake-parts-lib
, self
, withSystem
, ...
}: {
  perSystem = { pkgs, ... }: {
    checks.dev-net-nixos-test = pkgs.nixosTest (import ./test.nix {
      inherit flake-parts-lib self withSystem;
    });
  };

  flake = {
    nixosModules.dev-net = import ./cardano-dev-net.nix {
      inherit flake-parts-lib self withSystem;
    };
  };
}
