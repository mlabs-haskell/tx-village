{ self, ... }: {
  name = "Test Cardano Dev Net";

  nodes.machine = _: {
    imports = [
      self.inputs.cardano-nix.nixosModules.default
      self.outputs.nixosModules.dev-net
    ];

    services.cardano-dev-net.enable = true;
  };

  testScript = ''
    start_all()

    machine.wait_for_unit("cardano-node-socket.service")
  '';
}
