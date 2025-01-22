{ hostname }:
{
  ...
}:
{
  hardware = {
    az-raspi4-usb = {
      enable = true;
      hostname = hostname;
      ip = "10.213.0.1";
    };
  };

  profiles.az-server.enable = true;
  programs = {
    az-nix-direnv.enable = true;
  };
}
