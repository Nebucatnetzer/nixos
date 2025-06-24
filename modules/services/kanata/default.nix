{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-kanata;
in
{
  options = {
    services.az-kanata.enable = lib.mkEnableOption "Enable Kanata service.";
  };

  config = lib.mkIf cfg.enable {
    services.kanata = {
      enable = true;
      keyboards."notebook" = {
        config = builtins.readFile ./config.kbd;
        extraDefCfg = ''
          process-unmapped-keys yes
        '';
        devices = [ "/dev/input/by-path/platform-i8042-serio-0-event-kbd" ];
      };
    };
  };
}
