{ ... }:
{
  fileSystems."/mnt/data" = {
    device = "10.7.89.108:server_data";
    fsType = "nfs";
  };
}
