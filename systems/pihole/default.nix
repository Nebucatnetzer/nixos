{ custom, hostname, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.2";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "05:00"; inherit custom hostname inputs pkgs;
    })
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/pihole"
    "${inputs.self}/modules/unbound"
  ];
}
