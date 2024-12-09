{ config, lib, ... }:
let
  cfg = config.programs.az-beets;
in
{
  options = {
    programs.az-beets.enable = lib.mkEnableOption "My configuration for the beets package.";
  };

  config = lib.mkIf cfg.enable {

    programs.bash.shellAliases = {
      pra = ''mpv "$(beet random -a -p)" &'';
    };

    programs.beets = {
      enable = true;
      settings = {
        asciify_paths = true;
        directory = "/mnt/media/music";
        import = {
          autotag = false;
          copy = true;
          move = false;
          resume = false;
          write = false;
        };
        paths = {
          default = ''$albumartist/''${year}_''${album}/''${track}_''${title}'';
          singleton = "$artist/Non-Album/$title";
          comp = ''Various_Artists/$album/''${track}_''${title}'';
        };
        plugins = [
          "export"
          "fetchart"
          "lastgenre"
          "random"
        ];
      };
    };
  };
}
