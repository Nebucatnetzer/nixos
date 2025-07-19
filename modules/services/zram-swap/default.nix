{ config, lib, ... }:
let
  cfg = config.services.az-zram-swap;
in
{
  options = {
    services.az-zram-swap.enable = lib.mkEnableOption "Enable zram swap";
  };

  config = lib.mkIf cfg.enable {
    zramSwap = {
      enable = true;
      priority = 100;
    };
  };
}
