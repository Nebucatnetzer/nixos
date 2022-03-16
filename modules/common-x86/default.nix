{ inputs, custom, pkgs, system, ... }:
{
  imports = [
    (import "${inputs.self}/modules/common" {inherit inputs custom pkgs system;})
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };
  hardware = {
    cpu.intel.updateMicrocode = true;
  };
}

