{ ... }:
{
  environment = {
    variables = {
      XDG_DATA_HOME = "$HOME/.local/share";
    };
  };

  hardware.nvidia.prime = {
    # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
    intelBusId = "PCI:00:02:0";

    # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
    nvidiaBusId = "PCI:01:00:0";
  };
}
