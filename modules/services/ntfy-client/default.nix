{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-ntfy-client;

  ntfy-client = pkgs.writeShellScriptBin "ntfy-client" ''
    TOPIC=$(cat ${config.age.secrets.ntfyTopic.path})
    URL="https://ntfy.zweili.org"
    ${pkgs.curl}/bin/curl -d "$1" $URL/$TOPIC > /dev/null'';

  unit-status-ntfy = pkgs.writeShellScript "unit-status-ntfy" ''
    UNIT="$1"

    UNITSTATUS="$(systemctl status $UNIT)"
    ALERT="$(echo -e "\u26A0")"

    ${ntfy-client} "$ALERT Unit failed $UNIT $ALERT
    Status:
    $UNITSTATUS"'';
in
{
  options = {
    services.az-ntfy-client.enable = lib.mkEnableOption "Enable notificians via Ntfy";
  };

  config = lib.mkIf cfg.enable {
    age.secrets.ntfyTopic = {
      file = "${inputs.self}/scrts/ntfy-topic.age";
      mode = "644";
    };
    environment.systemPackages = [ ntfy-client ];
    systemd.services."unit-status-ntfy@" = {
      description = "Unit Status Ntfy Service";
      unitConfig = {
        After = "network-online.target";
      };
      serviceConfig = {
        Type = "simple";
        ExecStart = "${unit-status-ntfy} %I";
      };
    };
  };
}
