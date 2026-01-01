{
  config,
  inputs,
  pkgs,
  ...
}:
let
  azPkgs = import "${inputs.self}/pkgs" { inherit pkgs; };
  commonBtrfsOptions = import "${inputs.self}/modules/hardware/btrfs/common_options.nix";
in
{
  disko = {
    imageBuilder = {
      enableBinfmt = true;
      pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      kernelPackages = inputs.nixpkgs.legacyPackages.x86_64-linux.linuxPackages_latest;
    };
  };
  disko.devices = {
    disk = {
      main = {
        imageName = "${config.networking.hostName}-nixos-aarch64-linux-uefi-luks-btrfs";
        imageSize = "15G";
        type = "disk";
        device = "/dev/mmcblk1";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "1G";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
                postMountHook = builtins.toString (
                  pkgs.writeScript "postMountHook.sh" ''
                    cp --verbose -r ${azPkgs.raspi4Uefi}/* /mnt/boot/
                    mkdir -p /mnt/etc/secrets/initrd
                    cp /tmp/ssh_host_ed25519_key /mnt/etc/secrets/initrd/ssh_host_ed25519_key
                  ''
                );
              };
            };
            luks = {
              size = "100%";
              content = {
                type = "luks";
                name = "crypted";
                passwordFile = "/tmp/luks_password.key"; # Interactive
                settings = {
                  allowDiscards = true;
                };
                content = {
                  type = "btrfs";
                  extraArgs = [ "-f" ];
                  subvolumes = {
                    "/root" = {
                      mountpoint = "/";
                      mountOptions = commonBtrfsOptions;
                    };
                    "/home" = {
                      mountpoint = "/home";
                      mountOptions = commonBtrfsOptions;
                    };
                    "/nix" = {
                      mountpoint = "/nix";
                      mountOptions = commonBtrfsOptions;
                    };
                    "/swap" = {
                      mountpoint = "/swap";
                      swap.swapfile.size = "8G";
                    };
                  };
                };
              };

            };
          };
        };
      };
    };
  };
}
