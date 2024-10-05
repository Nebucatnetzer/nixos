{ config, lib, ... }:
let
  cfg = config.services.az-kmonad;
in
{
  options = {
    services.az-kmonad.enable = lib.mkEnableOption "Enable KMonad";
    services.az-kmonad.device = lib.mkOption {
      type = lib.types.path;
      example = "/dev/input/by-id/some-dev";
      description = "Path to the keyboard's device file.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.kmonad = {
      enable = true;
      keyboards = {
        myKMonadOutput = {
          device = cfg.device;
          config = builtins.readFile ./config.kbd;
        };
      };
    };
  };
}
