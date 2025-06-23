{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-librenms;
  libreNMSSyslogWrapper = pkgs.writeShellScriptBin "librenms-syslog" ''
    cd ${pkgs.librenms}
    sudo=exec
    if [[ "$USER" != ${config.services.librenms.user} ]]; then
      sudo='exec /run/wrappers/bin/sudo -u ${config.services.librenms.user}'
    fi
    $sudo ${pkgs.librenms.phpPackage}/bin/php ${pkgs.librenms}/syslog.php "$@"
  '';
in
{
  options = {
    services.az-librenms.enable = lib.mkEnableOption "Enable the LibreNMS service.";
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [
      514
    ];
    networking.firewall.allowedUDPPorts = [
      514
    ];
    services = {
      az-acme-base.enable = true;
      az-snmpd.enable = true;
      phpfpm.pools.librenms.phpOptions = ''
        "date.timezone" = ${config.time.timeZone}
        "opcache.enable" = 1
        "opcache.memory_consumption" = 256
        log_errors = "on"
        memory_limit = "${toString config.services.librenms.settings.php_memory_limit}M"
        post_max_size = "100M"
        upload_max_filesize = "100M"
        zend_extension = "opcache"
      '';
      librenms = {
        enable = true;
        database = {
          createLocally = true;
          socket = "/run/mysqld/mysqld.sock";
        };
        hostname = "librenms.zweili.org";
        nginx = {
          forceSSL = true;
          enableACME = true;
          # To set the LibreNMS virtualHost as the default virtualHost;
          default = true;
        };
        settings = {
          enable_syslog = true;
          ignore_mount = [
            "/sys"
            # defaults: https://github.com/librenms/librenms/blob/2cb8d9f042c9658531d85047c62f958cb9519ac7/misc/config_definitions.json#L4189
            "/compat/linux/proc"
            "/compat/linux/sys"
            "/dev"
            "/kern"
            "/mnt/cdrom"
            "/proc"
            "/sys/fs/cgroup"
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
            # defaults: https://github.com/librenms/librenms/blob/2cb8d9f042c9658531d85047c62f958cb9519ac7/misc/config_definitions.json#L4238
            "packages"
            "devfs"
            "procfs"
            "linprocfs"
            "linsysfs"
            "UMA"
            "MALLOC"
          ];
          syslog_purge = 30;
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
            program("${libreNMSSyslogWrapper}/bin/librenms-syslog" template ("''$HOST||''$FACILITY||''$PRIORITY||''$LEVEL||''$TAG||''$R_YEAR-''$R_MONTH-''$R_DAY ''$R_HOUR:''$R_MIN:''$R_SEC||''$MSG||''$PROGRAM\n") template-escape(yes));
          };

          log {
            source(s_net);
            destination(d_librenms);
          };
        '';
      };
    };
  };
}
