{ ... }:
{
  fileSystems."/mnt/restic" = {
    device = "10.7.89.108:restic";
    fsType = "nfs";
  };
}
