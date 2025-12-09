{ ... }:
{
  # Enable boot splash screen
  boot.plymouth.enable = true;
  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };
  hardware = {
    cpu.intel.updateMicrocode = true;
  };
  nixpkgs.hostPlatform = "x86_64-linux";
}
