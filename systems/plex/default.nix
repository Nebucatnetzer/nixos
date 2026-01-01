{ hostname }:
{ inputs, ... }:
let
  mediaShare = import "${inputs.self}/modules/services/media-share";
  raspi4Configs = import "${inputs.self}/modules/hardware/raspi4";
  resticClientServer = import "${inputs.self}/modules/services/restic-client-server";
in
{
  imports = [
    "${inputs.self}/modules/profiles/server"
    "${inputs.self}/modules/services/plex"
    (mediaShare { hard = true; })
    raspi4Configs.diskLayouts.singleSdCard
    (raspi4Configs.ethernet {
      inherit hostname;
      ip = "10.7.89.112";
    })
    (resticClientServer {
      path = "/var/lib/plex";
      tag = "plex";
      time = "01:30";
    })
  ];
}
