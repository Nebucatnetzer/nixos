{ custom, hostname }: { pkgs, ... }:
{
  imports = [
    (import "${custom.inputs.self}/systems/raspi4" {
      ip = "10.7.89.112";
      inherit custom hostname;
    })
    (import "${custom.inputs.self}/modules/restic-server-client" {
      path = "/var/lib/plex";
      tag = "plex";
      time = "02:30";
      inherit custom;
    })
    (import "${custom.inputs.self}/modules/docker" { inherit custom; })
    "${custom.inputs.self}/modules/media-share"
    (import "${custom.inputs.self}/modules/plex" { inherit custom; })
  ];
}
