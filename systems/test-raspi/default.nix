{ hostname }:
{ inputs, pkgs, ... }:
{
  imports = [ inputs.disko.nixosModules.disko ];
  disko.devices = {
    disk = {
      main = {
        imageSize = "10G";
        device = "/dev/disk/by-id/some-disk-id";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              type = "EF00";
              size = "1G";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
            root = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "f2fs";
                mountpoint = "/";
                mountOptions = [
                  "atgc,gc_merge"
                  "compress_algorithm=lz4"
                  "compress_extension=*"
                  "compress_chksum"
                  "discard"
                  "lazytime"
                ];
              };
            };
          };
        };
      };
    };
  };
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.40";
    };
  };

  services.az-docker.enable = true;
  programs = {
    az-nix-direnv.enable = true;
  };
}
