{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.123";
      inherit hostname;
    })
    (import "${inputs.self}/modules/restic-client-server" {
      path = "/home/andreas";
      time = "01:00";
    })
  ];
  services = {
    az-mailserver.enable = true;
    az-nginx-proxy = {
      enable = true;
      domain = "mail.zweili.org";
    };
  };
}

