{ inputs, withSystem, ... }: {
  imports = [
    inputs.hci-effects.flakeModule # Adds hercules-ci and herculesCI options
  ];

  hercules-ci.flake-update = {
    enable = true;
    updateBranch = "updated-flake-lock";
    # Next two parameters should always be set explicitly
    createPullRequest = true;
    autoMergeMethod = null;
    when = {
      # Perform update by Sundays at 12:45
      minute = 45;
      hour = 12;
      dayOfWeek = "Sun";
    };
  };

  herculesCI = herculesArgs: {
    onPush.default = {
      outputs.effects =
        let
          inherit (herculesArgs.config.repo) tag;
          tagRegEx = "v([0-9])+(\.[0-9]+)*(-[a-zA-Z]+)*";
        in
        {
          publish-tx-bakery =
            withSystem "x86_64-linux"

              ({ hci-effects, config, ... }:

                hci-effects.runIf
                  (tag != null && (builtins.match tagRegEx tag) != null)
                  (hci-effects.cargoPublish
                    {
                      src = config.packages.tx-bakery-rust-src;
                      secretName = "crates-io-token";
                      extraPublishArgs = [ "--dry-run" ];
                    })
              );
          publish-tx-indexer =
            withSystem "x86_64-linux"

              ({ hci-effects, config, ... }:

                hci-effects.runIf
                  (tag != null && (builtins.match tagRegEx tag) != null)
                  (hci-effects.cargoPublish
                    {
                      src = config.packages.tx-indexer-rust-src;
                      secretName = "crates-io-token";
                      extraPublishArgs = [ "--dry-run" ];
                    })
              );
        };
    };

    ciSystems = [ "x86_64-linux" "x86_64-darwin" ];
  };
}
