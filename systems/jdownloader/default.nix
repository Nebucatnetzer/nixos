{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "jdownloader";
      ip = "10.7.89.110";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "04:30"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/download-share"
    "${inputs.self}/modules/jdownloader"
  ];
}
