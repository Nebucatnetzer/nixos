{ pkgs, username, ... }:
let
  repository = "/mnt/restic-server";
in
{
  environment.systemPackages = with pkgs; [
    restic
  ];

  fileSystems."/mnt/restic" = {
    device = "10.7.89.108:restic";
    fsType = "nfs";
  };
  fileSystems.${repository} = {
    device = "10.7.89.108:restic-server";
    fsType = "nfs";
  };
  services.restic.server = {
    enable = true;
    dataDir = repository;
    extraFlags = [ "--no-auth" ];
  };
  networking.firewall.allowedTCPPorts = [ 8000 ];
}
