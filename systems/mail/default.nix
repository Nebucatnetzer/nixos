{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.123";
      inherit hostname;
    })
  ];
  services = {
    az-mailserver.enable = true;
    az-nginx-proxy = {
      enable = true;
      domain = "mail.zweili.org";
    };
    az-restic-client-server = {
      enable = true;
      path = "/home/andreas";
      time = "01:00";
    };
  };
}

