{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-yt-dlp;
  unstable = inputs.nixpkgs-unstable.legacyPackages.${pkgs.system};
in
{
  options = {
    programs.az-yt-dlp.enable = lib.mkEnableOption "Enable yt-dlp.";
  };

  config = lib.mkIf cfg.enable {
    programs.yt-dlp = {
      enable = true;
      package = unstable.yt-dlp;
      settings = {
        remux-video = "mkv";
      };
    };
  };
}
