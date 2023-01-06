{ custom, hostname }: { pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.2";
      inherit custom hostname;
    })
    (import "${custom.inputs.self}/modules/restic-client-server" {
      path = "/var/lib/pihole";
      tag = "pihole";
      time = "02:00"; inherit custom;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    (import "${custom.inputs.self}/modules/pihole" { inherit custom; })
    "${custom.inputs.self}/modules/unbound"
  ];
}
