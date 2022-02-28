{ ... }:
{
  fileSystems."/mnt/downloads" = {
    device = "10.7.89.108:media/downloads";
    fsType = "nfs";
  };
}
