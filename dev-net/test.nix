{ self, ... }: {
  name = "Test Cardano Dev Net";

  nodes.machine = { config, pkgs, lib, ... }: {
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

  testScript = ''
    start_all()

    machine.wait_for_unit("ogmios.service")
  '';
}
