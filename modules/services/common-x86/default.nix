{ config, lib, ... }:
let cfg = config.services.az-x86;
in {
  options = {
    services.az-x86.enable =
      lib.mkEnableOption "Enable options for x86 systems";
  };

  config = lib.mkIf cfg.enable {
    # Enable boot splash screen
    boot.plymouth.enable = true;
    # Use the systemd-boot EFI boot loader.
    boot.loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    hardware = { cpu.intel.updateMicrocode = true; };
  };
}
