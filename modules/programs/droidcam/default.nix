{
  config,
  pkgs,
  ...
}:
{
  programs.droidcam.enable = true;
  # required for USB connection
  services.usbmuxd.enable = true;
  environment.shellAliases = {
    webcam = "droidcam-cli -dev=/dev/video60 -size=1920x1080 ios 4747";
  };
  # Own loopback device, so droidcam-cli does not take the v4l2-relayd device on IPU6 hosts.
  systemd.services.droidcam-loopback = {
    description = "v4l2loopback device for DroidCam";
    wantedBy = [ "multi-user.target" ];
    after = [ "systemd-modules-load.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${config.boot.kernelPackages.v4l2loopback.bin}/bin/v4l2loopback-ctl add --exclusive-caps 1 --name DroidCam /dev/video60";
      ExecStop = "${config.boot.kernelPackages.v4l2loopback.bin}/bin/v4l2loopback-ctl delete /dev/video60";
    };
  };
  home-manager.users.${config.az-username} = {
    xdg.desktopEntries = {
      droidcam = {
        name = "Droidcam";
        exec = "${pkgs.droidcam}/bin/droidcam";
        terminal = false;
        categories = [
          "Video"
          "AudioVideo"
        ];
      };
    };
  };
}
