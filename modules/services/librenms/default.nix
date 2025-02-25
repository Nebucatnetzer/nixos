{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-librenms;
in
{
  options = {
    services.az-librenms.enable = lib.mkEnableOption "Enable the LibreNMS service.";
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      80
      514
    ];
    networking.firewall.allowedUDPPorts = [
      514
    ];
    services = {
      az-snmpd.enable = true;
      librenms = {
        enable = true;
        database = {
          createLocally = true;
          socket = "/run/mysqld/mysqld.sock";
        };
        settings = {
          enable_syslog = true;
          ignore_mount = [
            "/sys"
          ];
          ignore_mount_string = [
            "mnt"
            "msg.lock" # qnap
            "new_root" # qnap
            "NFSv=4" # qnap
            "store" # raspi
            "run" # linux
            "samba_third_party" # qnap
            "server-data" # servers
            "shm" # linux tmpfs
          ];
        };
      };
      syslog-ng = {
        enable = true;
        extraConfig = ''
          source s_net {
            tcp(port(514) flags(syslog-protocol));
            udp(port(514) flags(syslog-protocol));
          };

          destination d_librenms {
            program("${pkgs.librenms}/syslog.php" template ("''$HOST||''$FACILITY||''$PRIORITY||''$LEVEL||''$TAG||''$R_YEAR-''$R_MONTH-''$R_DAY ''$R_HOUR:''$R_MIN:''$R_SEC||''$MSG||''$PROGRAM\n") template-escape(yes));
          };

          log {
            source(s_net);
            source(s_all);
            destination(d_librenms);
          };
        '';
      };
    };
  };
}
