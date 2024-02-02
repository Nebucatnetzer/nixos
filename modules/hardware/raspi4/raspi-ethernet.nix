{ config, lib, ... }:
let
  cfg = config.hardware.az-raspi4-ethernet;
in
{
  options = {
    hardware.az-raspi4-ethernet = {
      enable = lib.mkEnableOption "Enable options required for Raspberry Pi 4.";
      hostname = lib.mkOption {
        type = lib.types.str;
        description = "The hostname of the system.";
      };
      ip = lib.mkOption {
        type = lib.types.str;
        description = "The IP of the system.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    boot.kernelParams = [
      "ip=${cfg.ip}::10.7.89.1:255.255.255.0:${cfg.hostname}:eth0" # required for ssh at initrd
    ];
    hardware.az-raspi4-base.enable = true;
    networking = {
      useDHCP = false;
      hostName = cfg.hostname;
      hosts = {
        "127.0.0.1" = [ "${cfg.hostname}.2li.local" ];
        ip = [ "${cfg.hostname}.2li.local" ];
      };
      defaultGateway = "10.7.89.1";
      nameservers = [ "10.7.89.1" ];
      interfaces.eth0.ipv4.addresses = [
        {
          address = cfg.ip;
          prefixLength = 24;
        }
      ];
    };

    services = {
      az-log2ram.enable = true;
      az-syslog.enable = true;
    };
  };
}
