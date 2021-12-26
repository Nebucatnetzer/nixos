{ pkgs, ... }:
{
  xdg.desktopEntries = {
    droidcam = {
      name = "Droidcam";
      exec = "${pkgs.droidcam}/bin/droidcam";
      terminal = false;
      categories = [ "Video" "AudioVideo" ];
    };
  };
}
