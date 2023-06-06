{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.103";
      inherit hostname;
    })
  ];

  services = {
    az-nextcloud = {
      enable = true;
      domain = "nextcloud.2li.ch";
    };
    az-restic-client-server-mysql = {
      enable = true;
      path = "/home/andreas";
      time = "01:30";
    };
  };
}
