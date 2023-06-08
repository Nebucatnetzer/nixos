# Raspberry Pi 4 Module

Do not import this module into the main `default.nix` file. Otherwise the other
systems aren't buildable anymore because some options from the `nixos-hardware`
input get applied immediately.

Just import it in any system you need with the following code where variant is
either `ethernet` or `usb`. `ethernet` is for a Pi which is intended as a
server and is connected with a fixed IP to the network. `usb` is for a
Raspberry Pi which can be connected via USB-C to another device, e.g. an iPad.
The ethernet port is configured to get its IP via DHCP and via uSB-C the Pi is
reachable via the IP provided in the configuration.

```nix
  imports = [
    "${inputs.self}/modules/hardware/raspi4-VARIANT"
  ];
  hardware = {
    az-raspi4-VARIANT = {
      enable = true;
      hostname = "some-hostname";
      ip = "10.7.89.150";
    };
  };
```
