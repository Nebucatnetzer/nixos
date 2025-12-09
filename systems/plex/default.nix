{ hostname }:
{ inputs, ... }:
let
  mediaShare = import "${inputs.self}/modules/services/media-share" { hard = true; };
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.112";
  };
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/plex"
    mediaShare
    raspiEthernet
  ];
  services.az-restic-client-server = {
    enable = true;
    path = "/var/lib/plex";
    tag = "plex";
    time = "01:30";
  };
}
