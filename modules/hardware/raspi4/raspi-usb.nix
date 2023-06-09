{ config, lib, ... }:
let
  cfg = config.hardware.az-raspi4-usb;
in
{
  imports = [
    ./base.nix
  ];

  options = {
    hardware.az-raspi4-usb = {
      enable = lib.mkEnableOption "Enable options required for Raspberry Pi 4.";
      hostname = lib.mkOption {
        type = lib.types.str;
        description = "The hostname of the system.";
      };
      ip = lib.mkOption {
        type = lib.types.str;
        description = "The IP of the system.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    boot = {
      kernelModules = [ "libcomposite" ];
      loader.raspberryPi.firmwareConfig = "dtoverlay=dwc2";
    };
    networking = {
      hostName = cfg.hostname;
      hosts = {
        "127.0.0.1" = [ "${cfg.hostname}.2li.local" ];
        ip = [ "${cfg.hostname}.2li.local" ];
      };
      interfaces.usb0.ipv4.addresses = [
        {
          address = cfg.ip;
          prefixLength = 24;
        }
      ];
      wireless = {
        enable = true;
        userControlled.enable = true;
        interfaces = [ "wlan0" ];
        networks = {
          "Gröibschi" = {
            psk = "schottland";
          };
        };
      };
    };

    networking.dhcpcd.denyInterfaces = [ "usb0" ];
    services.dhcpd4 = {
      enable = true;
      interfaces = [ "usb0" ];
      extraConfig = ''
        option domain-name "2li.mobile";
        option subnet-mask 255.255.255.0;
        option broadcast-address 10.213.0.255;
        option domain-name-servers 84.200.69.80, 84.200.70.40;
        subnet 10.213.0.0 netmask 255.255.255.0 {
          option routers ${cfg.ip};
          range 10.213.0.100 10.213.0.200;
        }
      '';
    };

    systemd.services."usb-otg" = {
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      wantedBy = [ "default.target" ];
      script = ''
        mkdir -p /sys/kernel/config/usb_gadget/pi4
        cd /sys/kernel/config/usb_gadget/pi4
        echo 0x1d6b > idVendor # Linux Foundation
        echo 0x0104 > idProduct # Multifunction Composite Gadget
        echo 0x0100 > bcdDevice # v1.0.0
        echo 0x0200 > bcdUSB # USB2
        echo 0xEF > bDeviceClass
        echo 0x02 > bDeviceSubClass
        echo 0x01 > bDeviceProtocol
        mkdir -p /sys/kernel/config/usb_gadget/pi4/strings/0x409
        echo "fedcba9876543211" > strings/0x409/serialnumber
        echo "Nebucatnetzer" > strings/0x409/manufacturer
        echo "PI4 USB Device" > strings/0x409/product
        mkdir -p /sys/kernel/config/usb_gadget/pi4/configs/c.1/strings/0x409
        echo "Config 1: ECM network" > configs/c.1/strings/0x409/configuration
        echo 250 > configs/c.1/MaxPower
        # Add functions here
        # see gadget configurations below
        # End functions
        mkdir -p /sys/kernel/config/usb_gadget/pi4/functions/ecm.usb0
        HOST="00:dc:c8:f7:75:14" # "HostPC"
        SELF="00:dd:dc:eb:6d:a1" # "BadUSB"
        echo $HOST > functions/ecm.usb0/host_addr
        echo $SELF > functions/ecm.usb0/dev_addr
        ln -s functions/ecm.usb0 configs/c.1/
        udevadm settle -t 5 || :
        ls /sys/class/udc > UDC
      '';
    };
    systemd.services.dhcpd4.after = [ "usb-otg.service" ];
    systemd.services."network-addresses-usb0".after = [ "usb-otg.service" ];
  };
}

