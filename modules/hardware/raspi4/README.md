# Raspberry Pi 4 Module

Do not import this module into the main `default.nix` file. Otherwise the other
systems aren't buildable anymore because some options from the `nixos-hardware`
input get applied immediately.

Just import it in any system you need with the following code:

```nix
  imports = [
    "${inputs.self}/modules/hardware/raspi4"
  ];
  hardware = {
    az-raspi4 = {
      enable = true;
      hostname = "some-hostname";
      ip = "10.7.89.150";
    };
  };
```
