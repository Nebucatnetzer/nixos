{ config, ... }: {
  programs.beets = {
    enable = true;
    settings = {
      asciify_paths = true;
      convert = {
        copy_album_art = "yes";
        dest = "${config.home.homeDirectory}/Music";
        format = "opus";
        formats = {
          opus = "ffmpeg -i $source -y -vn -acodec libopus -ab 192k -vbr on $dest";
        };
        never_convert_lossy_files = true;
        # convert.paths replaces the global paths block, it does not merge, so
        # singleton and comp are repeated here. %title{} is string.capwords, which
        # collapses the 15 albumartist spellings that differ only in case. The SD
        # card is exFAT and cannot hold two names that differ only in case.
        paths = {
          default = "%title{$albumartist}/\${year}_\${album}/\${track}_\${title}";
          singleton = "%title{$artist}/Non-Album/$title";
          comp = "Various_Artists/$album/\${track}_\${title}";
        };

      };
      directory = "/mnt/archive-disk/media/audio/music/music";
      embedart = {
        auto = true;
      };
      fetchart = {
        auto = true;
      };
      import = {
        autotag = true;
        copy = true;
        move = false;
        resume = false;
        write = false;
      };
      paths = {
        default = "$albumartist/\${year}_\${album}/\${track}_\${title}";
        singleton = "$artist/Non-Album/$title";
        comp = "Various_Artists/$album/\${track}_\${title}";
      };
      # Beets replaces the default rules instead of merging, so the defaults are
      # repeated here. The last rule turns every run of whitespace into "_".
      # Nix sorts these keys and beets applies them in that order. The default
      # rules '\s+$' and '^\s+' are left out because they sort after '\s+' and
      # would never match.
      replace = {
        "\"" = "_";
        "[<>:\\?\\*\\|]" = "_";
        "[\\\\/]" = "_";
        "[\\x00-\\x1f]" = "_";
        "\\.$" = "_";
        "\\s+" = "_";
        "^-" = "_";
        "^\\." = "_";
      };
      plugins = [
        "convert"
        "embedart"
        "export"
        "fetchart"
        "lastgenre"
        "random"
      ];
    };
  };
}
