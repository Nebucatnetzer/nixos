{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.103";
      inherit hostname;
    })
    "${inputs.self}/modules/nginx-acme-base"
    (import "${inputs.self}/modules/restic-client-server-mysql" {
      path = "/home/andreas";
      time = "01:30";
    })
  ];

  services = {
    az-nextcloud = {
      enable = true;
      domain = "nextcloud.2li.ch";
    };
  };
}
