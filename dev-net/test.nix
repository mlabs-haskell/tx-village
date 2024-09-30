{ self, ... }: {
  name = "Test Cardano Dev Net";

  nodes =
    let
      # TODO(chfanghr): this *is* a nixosTest attribute in the new version of nixpkgs
      defaults = { config, pkgs, lib, ... }: {
        imports = [
          self.inputs.cardano-nix.nixosModules.default
          self.outputs.nixosModules.dev-net
        ];

        services = {
          cardano-dev-net.enable = true;
          ogmios = {
            enable = true;
            nodeSocketPath = config.services.cardano-dev-net.socketPath;
            nodeConfigPath = config.services.cardano-dev-net.dynamicConfigFile;
          };
        };

        systemd.services.ogmios =
          let
            waitForOgmios = pkgs.writeShellScript "wait-for-ogmios" ''
              function is-listening {
                ${lib.getExe' pkgs.iproute2 "ss"} \
                  --no-header \
                  --numeric \
                  --listening \
                  --tcp src = ${config.services.ogmios.host} and sport = inet:${toString config.services.ogmios.port} | grep LISTEN
              }

              until is-listening; do sleep 10; done
            '';
          in
          {
            after = [ "cardano-node-socket.service" ];
            serviceConfig.RestrictAddressFamilies = [ "AF_NETLINK" ];
            postStart = ''
              timeout 60 ${waitForOgmios}
            '';
          };
      };
    in
    {
      machineWithNetworkMagic42 = {
        imports = [ defaults ];
      };
      machineWithNetworkMagic1 = {
        imports = [ defaults ];
        services.cardano-dev-net.networkMagic = 1;
      };
    };

  testScript = ''
    start_all()

    machineWithNetworkMagic1.wait_for_unit("ogmios.service")
    machineWithNetworkMagic42.wait_for_unit("ogmios.service")
  '';
}
