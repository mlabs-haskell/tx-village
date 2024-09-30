{ self, ... }: {
  name = "Test Cardano Dev Net";

  nodes.machine = { config, ... }: {
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

    systemd.services.ogmios.after = [ "cardano-node-socket.service" ];
  };

  testScript = ''
    start_all()

    machine.wait_for_unit("ogmios.service")
  '';
}
