{ username, ... }:
let
  repository = "/mnt/restic";
in
{
  fileSystems.${repository} = {
    device = "10.7.89.108:restic";
    fsType = "nfs";
  };
  services.restic.server = {
    enable = true;
    dataDir = repository;
    extraFlags = [ "--no-auth" ];
  };
}
