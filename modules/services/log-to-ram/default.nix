{ config, lib, ... }:
let cfg = config.services.az-log2ram;
in {
  options = {
    services.az-log2ram.enable = lib.mkEnableOption "Enable log to RAM";
  };

  config = lib.mkIf cfg.enable {
    fileSystems."/var/log" = {
      device = "none";
      fsType = "tmpfs";
      options = [ "defaults" "size=512M" ];
    };
    services.journald.extraConfig = ''
      SystemMaxUse=300M
      Storage=volatile
    '';
  };
}
