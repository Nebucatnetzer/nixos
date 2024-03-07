{
  config,
  lib,
  pkgs,
  ...
}:
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
    # Since we have "fast" swap, we can increase swappiness
    boot.kernel.sysctl = {
      "vm.swappiness" = 180;
    };
  };
}
