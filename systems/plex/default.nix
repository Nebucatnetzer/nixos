{ hostname }: { inputs, pkgs, ... }:
{
  imports = [
    (import "${inputs.self}/systems/raspi4" {
      ip = "10.7.89.112";
      inherit hostname;
    })
    (import "${inputs.self}/modules/restic-client-server" {
      path = "/var/lib/plex";
      tag = "plex";
      time = "02:30";
    })
    "${inputs.self}/modules/plex"
  ];
  services = {
    az-media-share.enable = true;
  };
}
