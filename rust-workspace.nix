_: {
  perSystem = { pkgs, config, inputs', ... }: {
    packages.rust-workspace =
      let
        rustCrates = [
          config.packages.tx-bakery-rust-src
          config.packages.tx-indexer-rust-src
        ];

        rustCrateNames =
          pkgs.lib.concatMapStringsSep ", " (crate: ''"${crate.name}"'') rustCrates;

        devDeps = [
          inputs'.plutus-ledger-api-rust.packages.plutus-ledger-api-rust-src
          inputs'.lbf.packages.lbf-prelude-rust
          inputs'.lbf.packages.lbf-plutus-rust
          inputs'.lbf.packages.lbr-prelude-rust-src
          inputs'.lbf.packages.lbr-prelude-derive-rust-src

          config.packages.lbf-tx-bakery-tests-config-api-rust
          config.packages.lbf-tx-bakery-tests-plutus-api-rust
        ];

        sources = pkgs.linkFarm "sources"
          (builtins.map (drv: { inherit (drv) name; path = drv; }) (rustCrates ++ devDeps));

        rootCargoToml = pkgs.writeTextFile {
          name = "rootCargoToml";
          text = '' 
            [workspace]
            members = [${rustCrateNames}]
            resolver = "2"
          '';
        };
      in
      pkgs.stdenv.mkDerivation {
        src = ./.;
        name = "rust-workspace";
        unpackPhase = ''
          mkdir $out
          cp -Lr ${sources}/* $out
          cat ${rootCargoToml} > $out/Cargo.toml;
        '';
      };
  };
}
