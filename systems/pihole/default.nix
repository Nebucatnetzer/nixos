{ custom, hostname }: { pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.2";
      inherit custom hostname;
    })
    (import "${custom.inputs.self}/modules/restic-server-client" {
      path = "/var/lib/pihole";
      tag = "pihole";
      time = "05:00"; inherit custom;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    (import "${custom.inputs.self}/modules/pihole" { inherit custom; })
    "${custom.inputs.self}/modules/unbound"
  ];
}
