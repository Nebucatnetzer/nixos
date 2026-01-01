{ hostname }:
{ inputs, ... }:
let
  mediaShare = import "${inputs.self}/modules/services/media-share" { hard = true; };
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
  raspiEthernet = import "${inputs.self}/modules/hardware/raspi4/raspi-ethernet.nix" {
    inherit hostname;
    ip = "10.7.89.112";
  };
  resticClientServer = import "${inputs.self}/modules/services/restic-client-server";
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/plex"
    raspi4Configs.diskLayouts.singleSdCard
    mediaShare
    raspiEthernet
    (resticClientServer {
      path = "/var/lib/plex";
      tag = "plex";
      time = "01:30";
    })
  ];
}
