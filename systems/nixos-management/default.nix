{ config, custom, inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      hostname = "nixos-management";
      ip = "10.7.89.150";
      inherit inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      time = "21:30"; inherit config custom inputs pkgs;
    })
    "${inputs.self}/modules/code-server"
    "${inputs.self}/modules/docker"
    "${inputs.self}/modules/nix-direnv"
    "${inputs.self}/modules/tmux"
  ];
  # Required to build aarch64 packages
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
}
