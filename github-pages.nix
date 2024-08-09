{
  hercules-ci.github-pages.branch = "connor/github-pages"; # FIXME

  perSystem = { config, pkgs, ... }:
    let
      github-pages-main-page = pkgs.writeText "README.md" ''
        ${builtins.readFile ./README.md}

        ## API References

        - [tx-bakery](./artifacts/tx-bakery/tx_bakery/index.html)
        - [tx-bakery-ogmios](./artifacts/tx-bakery-ogmios/tx_bakery_ogmios/index.html)
        - [tx-bakery-plutip](./artifacts/tx-bakery-plutip/tx_bakery_plutip/index.html)
      '';

      github-pages = pkgs.stdenv.mkDerivation {
        name = "tx-village-github-pages";
        src = ./.;
        buildPhase = ''
          mkdir $out

          cp -L -v ${github-pages-main-page} $out/README.md

          mkdir $out/artifacts

          cp -L -v -r ${config.packages.tx-bakery-rust-doc}/share/doc $out/artifacts/tx-bakery
          cp -L -v -r ${config.packages.tx-bakery-ogmios-rust-doc}/share/doc $out/artifacts/tx-bakery-ogmios
          cp -L -v -r ${config.packages.tx-bakery-plutip-rust-doc}/share/doc $out/artifacts/tx-bakery-plutip
        '';
      };
    in
    {
      packages = {
        inherit github-pages-main-page github-pages;
      };
      hercules-ci.github-pages.settings.contents = github-pages;
    };
}
