{ custom, hostname, inputs }: { pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.2";
      inherit hostname inputs;
    })
    (import "${inputs.self}/modules/restic-server-client" {
      path = "/var/lib/pihole";
      tag = "pihole";
      time = "05:00"; inherit custom hostname inputs;
    })
    (import "${inputs.self}/modules/docker" { inherit custom; })
    (import "${inputs.self}/modules/pihole" { inherit inputs; })
    "${inputs.self}/modules/unbound"
  ];
}
