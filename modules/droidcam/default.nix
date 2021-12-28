{ ... }:
{
  programs.droidcam.enable = true;
  # required for USB connection
  services.usbmuxd.enable = true;
  environment.shellAliases = {
    webcam = "droidcam-cli -size=1920x1080 ios 4747";
  };
}

