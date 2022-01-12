{ ... }:
{
  fileSystems."/mnt/data" = {
    device = "10.7.89.108:raspi_data";
    fsType = "nfs";
  };
}
