{ inputs, ... }:
{
  perSystem =
    {
      system,
      pkgs,
      config,
      self',
      ...
    }:
    let
      rustFlake = inputs.flake-lang.lib.${system}.rustFlake {
        src = ./.;
        crateName = "tx-indexer-testsuite";
        exportTests = true;
        extraSources = [
          config.packages.tx-bakery-rust-src
          config.packages.tx-bakery-ogmios-rust-src
          config.packages.tx-indexer-rust-src
          config.packages.diesel-derive-pg-rust-src
        ];
        buildInputs = [ pkgs.postgresql_16.lib ];

        devShellTools = [
          self'.packages.tx-indexer-tests
        ];

        devShellHook =
          config.settings.shell.hook
          + ''
            ln -sf ${../../tx-indexer}/lib-migrations

            echo "TxIndexer testsuite"
            echo ""
            echo "Run tx-indexer-tests to execute the testsuite."
            echo "or tx-indexer-tests up db_migration ogmios devnet to spin up an environment"
            echo ""
          '';
      };
    in
    {
      inherit (rustFlake) devShells packages;

      checks = {
        "tx-indexer-testsuite" = pkgs.stdenv.mkDerivation {
          name = "tx-indexer-testsuite";
          unpackPhase = ''
            echo "Linking data"
            ln -s ${./wallets} ./wallets
            mkdir ./tests
            ln -s ${./tests/fixtures} ./tests/fixtures
          '';
          checkPhase = ''
            ${self'.packages.tx-indexer-tests-ci}/bin/tx-indexer-tests-ci --tui=false
          '';
          buildPhase = ''
            mkdir $out
          '';
          doCheck = true;
        };
      };

    };

}
