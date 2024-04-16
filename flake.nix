{
  description = "Cardano Open Oracle Protocol";

  inputs = {
    # LambdaBuffers as source of truth for many inputs
    lbf.url = "github:mlabs-haskell/lambda-buffers?ref=szg251/tx-info";

    # Flake monorepo toolkit
    flake-lang.follows = "lbf/flake-lang";

    # Nix
    nixpkgs.follows = "lbf/nixpkgs";
    flake-parts.follows = "lbf/flake-parts";

    ## Code quality automation
    pre-commit-hooks.follows = "lbf/pre-commit-hooks";
    hci-effects.follows = "lbf/hci-effects";

    # Cardano transaction library (transaction building)
    ctl.follows = "lbf/ctl";

    # Plutarch (Plutus validation scripts)
    plutarch.follows = "lbf/plutarch";

    # Plutip for spawning local Cardano networks
    plutip.url = "github:mlabs-haskell/plutip";

    # Light-weight wrapper around cardano-node
    ogmios.url = "github:mlabs-haskell/ogmios-nixos";

    # Plutus Ledger API types and utilities for Rust
    plutus-ledger-api-rust = {
      url = "github:mlabs-haskell/plutus-ledger-api-rust";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./pkgs.nix
        ./settings.nix
        ./pre-commit.nix
        ./hercules-ci.nix

        # Libraries
        ./tx-bakery/build.nix
        ./extras/tx-bakery-testsuite/api/build.nix

        # Extras
        ./extras/tx-bakery-testsuite/validation/build.nix
      ];
      debug = true;
      systems = [ "x86_64-linux" "x86_64-darwin" ];
    };
}
