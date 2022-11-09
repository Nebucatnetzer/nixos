{ custom, hostname }: { pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.123";
      inherit hostname custom;
    })
    (import "${custom.inputs.self}/modules/restic-server-client" {
      path = "/home/andreas";
      time = "01:00";
      inherit custom;
    })
    (import "${custom.inputs.self}/modules/nginx-proxy" {
      domain = "mail.zweili.org"; inherit custom;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    (import "${custom.inputs.self}/modules/docker-mailserver" { inherit custom; })
  ];
}

