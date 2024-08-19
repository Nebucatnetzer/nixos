{ config, lib, ... }:
let
  cfg = config.programs.az-beets;
in
{
  options = {
    programs.az-beets.enable = lib.mkEnableOption "My configuration for the beets package.";
  };

  config = lib.mkIf cfg.enable {
    programs.beets = {
      enable = true;
      settings = {
        asciify_paths = true;
        directory = "/run/user/1000/gvfs/smb-share:server=10.7.89.108,share=media/music";
        import = {
          autotag = false;
          copy = true;
          move = false;
          resume = false;
          write = false;
        };
        paths = {
          default = "$albumartist/$year_$album/$track_$title";
          singleton = "$artist/Non-Album/$title";
          comp = "Various_Artists/$album/$track_$title";
        };
        plugins = [
          "export"
          "fetchart"
          "lastgenre"
        ];
      };
    };
  };
}