# Repo-wide Nixpkgs with a ton of overlays
{ inputs, ... }:
{
  perSystem = { pkgs, system, ... }: {

    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        (final: _: {
          inherit (final) openssl;
        })
      ];
    };

  };
}
