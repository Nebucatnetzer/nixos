{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.hardware.az-nvidia;
in
{
  options = {
    hardware.az-nvidia.enable = lib.mkEnableOption "Nvidia GPU Support";
  };

  config = lib.mkIf cfg.enable {
    environment = {
      variables = {
        XDG_DATA_HOME = "$HOME/.local/share";
      };
    };
    services.xserver.videoDrivers = lib.mkDefault [ "nvidia" ];
    hardware.graphics.extraPackages = [ pkgs.vaapiVdpau ];
    hardware.graphics.enable = true;
    hardware.nvidia.open = false;
    hardware.nvidia.prime = {
      # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
      intelBusId = "PCI:00:02:0";
      # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
      nvidiaBusId = "PCI:01:00:0";
      offload = {
        enable = true;
        enableOffloadCmd = true;
      };
    };
  };
}
