{ inputs, ... }:
{
  imports = [
    "${inputs.self}/home-manager/modules"
  ];
  systemd.user.startServices = "sd-switch";
}
