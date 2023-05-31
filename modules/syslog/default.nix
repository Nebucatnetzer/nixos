{ config, lib, ... }:
let
  cfg = config.services.az-syslog;
in
{
  options = {
    services.az-syslog.enable = lib.mkEnableOption "Enable syslog";
  };

  config = lib.mkIf cfg.enable {
    services.rsyslogd = {
      enable = true;
      defaultConfig = ''
        *.*  action(type="omfwd" target="10.7.89.108" port="514" protocol="udp"
                    action.resumeRetryCount="100"
                    queue.type="linkedList" queue.size="10000")
      '';
    };
    systemd.services.syslog.after = [ "network-online.target" ];
  };
}

