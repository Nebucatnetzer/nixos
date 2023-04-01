{ custom, hostname }: { lib, pkgs, ... }:
let
  ip = "10.213.0.1";
in
{
  imports = [
    custom.inputs.nixos-hardware.nixosModules.raspberry-pi-4
    "${custom.inputs.self}/modules/log-to-ram"
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    (import "${custom.inputs.self}/modules/nix-direnv" { inherit custom; })
    "${custom.inputs.self}/modules/tmux"
  ];

  boot = {
    supportedFilesystems = lib.mkForce [ "f2fs" "ntfs" "cifs" "ext4" "vfat" "nfs" "nfs4" ];
    # kernelModules = [ "libcomposite" ];
  };
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  hardware.raspberry-pi."4".fkms-3d.enable = true;
  hardware.raspberry-pi."4".audio.enable = true;
  hardware.pulseaudio.enable = true;

  environment.systemPackages = with pkgs; [
    libraspberrypi
    raspberrypi-eeprom
  ];

  networking = {
    hostName = hostname;
    hosts = {
      "127.0.0.1" = [ "${hostname}.2li.local" ];
      ip = [ "${hostname}.2li.local" ];
    };
    interfaces.usb0.ipv4.addresses = [
      {
        address = ip;
        prefixLength = 24;
      }
    ];
  };

  boot.loader.raspberryPi.firmwareConfig = "dtoverlay=dwc2";
  networking.dhcpcd.denyInterfaces = [ "usb0" ];

  services.dhcpd4 = {
    enable = true;
    interfaces = [ "usb0" ];
    extraConfig = ''
      option domain-name "nixos";
      option domain-name-servers 8.8.8.8, 8.8.4.4;
      subnet 10.0.3.0 netmask 255.255.255.0 {
        range 10.0.3.100 10.0.3.200;
        option subnet-mask 255.255.255.0;
        option broadcast-address 10.0.3.255;
      }
    '';
  };

  environment.shellAliases = {
    raspi-cpu = ''
      sudo vcgencmd get_throttled && sudo vcgencmd measure_temp
    '';
    raspi-firmware-update = ''
      sudo mkdir -p /mnt && \
      sudo mount /dev/disk/by-label/FIRMWARE /mnt && \
      BOOTFS=/mnt FIRMWARE_RELEASE_STATUS=stable sudo -E rpi-eeprom-update -d -a && \
      sudo umount /mnt
    '';
  };
}
