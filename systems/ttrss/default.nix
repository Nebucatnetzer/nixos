{ custom, hostname }: { pkgs, ... }:
let
  domain = "ttrss.2li.ch";
in
{
  imports = [
    (import "${custom.inputs.self}/systems/proxmox-vm" {
      ip = "10.7.89.115";
      inherit custom hostname;
    })
    (import "${custom.inputs.self}/modules/nginx-proxy" { inherit custom domain; })
    (import "${custom.inputs.self}/modules/ttrss-postgres" { inherit custom domain; })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
  ];
}
