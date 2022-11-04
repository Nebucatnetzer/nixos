{ custom, hostname, inputs }: { pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.30";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/restic-server" { inherit inputs; })
  ];
}
