{ inputs, ... }:
{
  imports = [
    inputs.pre-commit-hooks.flakeModule
  ];
  perSystem =
    { config, ... }:
    {
      devShells.dev-pre-commit = config.pre-commit.devShell;
      devShells.default = config.pre-commit.devShell;

      pre-commit = {
        settings = {
          excludes = [
          ];

          hooks = {
            nixfmt-rfc-style.enable = true;
            deadnix.enable = true;
            statix.enable = true;
            cabal-fmt.enable = true;
            fourmolu.enable = true;
            hlint.enable = true;
            typos = {
              enable = true;
              settings = {
                ignored-words = [
                  "TypeLits"
                  "BA"
                  "numer"
                ];
                exclude = [
                  "fourmolu.yaml"
                  "genesis-*.json"
                ];
              };
            };
            markdownlint.enable = true;
            dhall-format.enable = true;
            purty.enable = true;
          };

          settings = {
            ormolu.cabalDefaultExtensions = true;
          };
        };
      };
    };
}
