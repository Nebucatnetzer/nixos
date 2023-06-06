{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.112";
      inherit hostname;
    })
  ];
  services = {
    az-media-share.enable = true;
    az-plex.enable = true;
    az-restic-client-server = {
      enable = true;
      path = "/var/lib/plex";
      tag = "plex";
      time = "02:30";
    };
  };
}
