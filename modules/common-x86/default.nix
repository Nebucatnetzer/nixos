{ custom }: { ... }:
{
  imports = [
    (import "${custom.inputs.self}/modules/common" { inherit custom; })
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

