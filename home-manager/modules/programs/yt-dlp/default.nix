{ config, lib, ... }:
let
  cfg = config.programs.az-yt-dlp;
in
{
  options = {
    programs.az-yt-dlp.enable = lib.mkEnableOption "Enable yt-dlp.";
  };

  config = lib.mkIf cfg.enable {
    programs.yt-dlp = {
      enable = true;
      extraConfig = ''
        -S "+codec:h264"
      '';
      settings = {
        remux-video = "mkv";
      };
    };
  };
}
