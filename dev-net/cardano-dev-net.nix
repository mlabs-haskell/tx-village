{ withSystem, ... }: { lib
                     , config
                     , pkgs
                     , ...
                     }:
let
  inherit (lib) mkOption types mkEnableOption mkIf;
  inherit (builtins) toString;

  cfg = config.services.cardano-dev-net;

  defaultDynamicConfigDir = "cardano-dev-net";

  inSystemdStateDir = dir: "/var/lib/${dir}";

  topology = (pkgs.formats.json { }).generate "cardano-dev-net-empty-topology.json" {
    Producers = [ ];
  };

  keyType = types.submodule {
    options = {
      keyFile = mkOption {
        type = types.path;
      };

      dynamicKeyPath = mkOption {
        type = types.path;
      };
    };
  };

  initialFundsConfig = (pkgs.formats.json { }).generate "initial-funds-config" cfg.initialFunds;
in
{
  options.services.cardano-dev-net = {
    enable = mkEnableOption "cardano dev net";

    socketPath = lib.mkOption {
      type = types.path;
      default = "/run/cardano-node/node.socket";
    };

    networkMagic = lib.mkOption {
      type = types.ints.unsigned;
      default = 42;
    };

    templateConfigDir = mkOption {
      type = types.path;
      default = ./fixtures;
    };

    templateConfigFile = mkOption {
      type = types.path;
      readOnly = true;
      default = "${cfg.templateConfigDir}/config.json";
    };

    dynamicConfigDir = mkOption {
      type = types.path;
      readOnly = true;
      default = inSystemdStateDir defaultDynamicConfigDir;
    };

    dynamicConfigFile = mkOption {
      type = types.path;
      readOnly = true;
      default = "${cfg.dynamicConfigDir}/config.json";
    };

    kesKey = mkOption {
      type = keyType;
      default = {
        keyFile = ./fixtures/kes.skey;
        dynamicKeyPath = "${inSystemdStateDir defaultDynamicConfigDir}/kes.skey";
      };
    };

    vrfKey = mkOption {
      type = keyType;
      default = {
        keyFile = ./fixtures/vrf.skey;
        dynamicKeyPath = "${inSystemdStateDir defaultDynamicConfigDir}/vrf.skey";
      };
    };

    operationalCertificate = mkOption {
      type = keyType;
      default = {
        keyFile = ./fixtures/opcert.cert;
        dynamicKeyPath = "${inSystemdStateDir defaultDynamicConfigDir}/opcert.cert";
      };
    };

    signingKey = mkOption {
      type = keyType;
      default = {
        keyFile = ./fixtures/byron-delegate.key;
        dynamicKeyPath = "${inSystemdStateDir defaultDynamicConfigDir}/byron-delegate.key";
      };
    };

    delegationCertificate = mkOption {
      type = keyType;
      default = {
        keyFile = ./fixtures/byron-delegation.cert;
        dynamicKeyPath = "${inSystemdStateDir defaultDynamicConfigDir}/byron-delegation.cert";
      };
    };

    initialFunds = mkOption {
      type = types.attrsOf types.ints.unsigned;
      default = { };
      example = {
        "609783be7d3c54f11377966dfabc9284cd6c32fca1cd42ef0a4f1cc45b" = 900000000000;
      };
    };
  };

  config = mkIf cfg.enable {
    services.cardano-node = {
      enable = true;
      package = withSystem pkgs.stdenv.system (
        { inputs', ... }:
        inputs'."cardano-node-9.1.0".packages.cardano-node
      );
      inherit topology;
      nodeConfigFile = cfg.dynamicConfigFile;
      inherit (cfg) socketPath;

      kesKey = cfg.kesKey.dynamicKeyPath;
      vrfKey = cfg.vrfKey.dynamicKeyPath;
      operationalCertificate = cfg.operationalCertificate.dynamicKeyPath;
      signingKey = cfg.signingKey.dynamicKeyPath;
      delegationCertificate = cfg.delegationCertificate.dynamicKeyPath;
    };

    systemd.services.prepare-cardano-dev-net = {
      wantedBy = [ "multi-user.target" ];
      before = [ "cardano-node.service" ];
      serviceConfig = {
        Type = "oneshot";
        Restart = "on-failure";
        RemainAfterExit = true;
        User = "cardano-node";
        Group = "cardano-node";
        StateDirectory = [
          defaultDynamicConfigDir
        ];
      };
      path = [
        pkgs.jq
      ];
      script = ''
        set -x
        if ! [ -f ${cfg.dynamicConfigDir}/done ]; then
          cd "${cfg.templateConfigDir}"

          BYRON_GENESIS_FILE_TEMPLATE="`jq -r .ByronGenesisFile ${cfg.templateConfigFile}`"
          SHELLY_GENESIS_FILE_TEMPLATE="`jq -r .ShelleyGenesisFile ${cfg.templateConfigFile}`"
          ALONZO_GENESIS_FILE_TEMPLATE="`jq -r .AlonzoGenesisFile ${cfg.templateConfigFile}`"
          CONWAY_GENESIS_FILE_TEMPLATE="`jq -r .ConwayGenesisFile ${cfg.templateConfigFile}`"

          BYRON_GENESIS_FILE="${cfg.dynamicConfigDir}/genesis-byron.json"
          SHELLY_GENESIS_FILE="${cfg.dynamicConfigDir}/genesis-shelly.json"
          ALONZO_GENESIS_FILE="${cfg.dynamicConfigDir}/genesis-alonzo.json"
          CONWAY_GENESIS_FILE="${cfg.dynamicConfigDir}/genesis-conway.json"

          jq -r .startTime=`date +%s` "$BYRON_GENESIS_FILE_TEMPLATE" > "$BYRON_GENESIS_FILE"
          jq -r ".systemStart=\"`date -u +%FT%TZ`\" | .initialFunds=`cat ${initialFundsConfig}` | .networkMagic=${toString cfg.networkMagic}" "$SHELLY_GENESIS_FILE_TEMPLATE" > "$SHELLY_GENESIS_FILE"
          cp "$ALONZO_GENESIS_FILE_TEMPLATE" "$ALONZO_GENESIS_FILE"
          cp "$CONWAY_GENESIS_FILE_TEMPLATE" "$CONWAY_GENESIS_FILE"

          jq -r ".ByronGenesisFile=\"$BYRON_GENESIS_FILE\" | .ShelleyGenesisFile=\"$SHELLY_GENESIS_FILE\" | .AlonzoGenesisFile=\"$ALONZO_GENESIS_FILE\" | .ConwayGenesisFile=\"$CONWAY_GENESIS_FILE\""\
            "${cfg.templateConfigFile}" > "${cfg.dynamicConfigFile}"

          install -m 400 -o cardano-node -g cardano-node ${cfg.kesKey.keyFile} ${cfg.kesKey.dynamicKeyPath}
          install -m 400 -o cardano-node -g cardano-node ${cfg.vrfKey.keyFile} ${cfg.vrfKey.dynamicKeyPath}
          install -m 400 -o cardano-node -g cardano-node ${cfg.operationalCertificate.keyFile} ${cfg.operationalCertificate.dynamicKeyPath}
          install -m 400 -o cardano-node -g cardano-node ${cfg.signingKey.keyFile} ${cfg.signingKey.dynamicKeyPath}
          install -m 400 -o cardano-node -g cardano-node ${cfg.delegationCertificate.keyFile} ${cfg.delegationCertificate.dynamicKeyPath}

          touch ${cfg.dynamicConfigDir}/done
        fi
      '';
    };

    systemd.services.cardano-node-socket = {
      description = "Wait for cardano-node socket to appear and set permissions to allow group read and write.";
      after = [ "cardano-node.service" ];
      requires = [ "cardano-node.service" ];
      bindsTo = [ "cardano-node.service" ];
      requiredBy = [ "cardano-node.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      # Using a path unit doesn't allow dependencies to be declared correctly, so poll.
      script = ''
        echo 'Waiting for ${cfg.socketPath} to appear...'
        /bin/sh -c 'until test -e ${cfg.socketPath}; do sleep 1; done'
        echo 'Changing permissions for ${cfg.socketPath}.'
        chmod g+rw ${cfg.socketPath}
      '';
    };

    environment.variables = {
      CARDANO_NODE_SOCKET_PATH = cfg.socketPath;
      CARDANO_NODE_NETWORK_MAGIC = cfg.networkMagic;
    };
  };
}
