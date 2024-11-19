{ hostname }:
{ inputs, pkgs, ... }:
let
  uefiFirmware = pkgs.fetchzip {
    url = "https://github.com/pftf/RPi4/releases/download/v1.38/RPi4_UEFI_Firmware_v1.38.zip";
    sha256 = "sha256-9tOr80jcmguFy2bSz+H3TfmG8BkKyBTFoUZkMy8x+0g=";
    stripRoot = false;
  };
in
{
  imports = [
    inputs.disko.nixosModules.disko
    "${inputs.nixos-hardware}/raspberry-pi/4"
  ];
  nixpkgs.hostPlatform = "aarch64-linux";
  disko.devices = {
    disk = {
      main = {
        imageSize = "10G";
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
                postMountHook = ''
                  cp -r ${uefiFirmware} /boot
                '';
                mountOptions = [ "umask=0077" ];
              };
            };
            root = {
              size = "100%";
              device = "/dev/disk/by-label/NIXOS_SD";
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

  networking = {
    useDHCP = false;
    hostName = hostname;
    hosts = {
      "127.0.0.1" = [ "${hostname}.2li.local" ];
      ip = [ "${hostname}.2li.local" ];
    };
    defaultGateway = "10.7.89.1";
    nameservers = [ "10.7.89.1" ];
    interfaces.eth0.ipv4.addresses = [
      {
        address = "10.7.89.40";
        prefixLength = 24;
      }
    ];
  };
}
