{ hostname }:
{ inputs, ... }:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.150";
    };
  };

  zramSwap = {
    enable = true;
  };

  # Features
  profiles.az-server.enable = true;
  services = {
    az-data-share.enable = true;
    az-docker.enable = true;
    az-logs-share.enable = true;
    az-nextcloud-cli-client.enable = true;
    az-restic-client-server = {
      enable = true;
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
    };
  };
  # Enable dictionaries
  programs = {
    az-nix-direnv.enable = true;
    az-restic-management.enable = true;
  };
}
