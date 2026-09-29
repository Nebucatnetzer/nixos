{ config, ... }: {
  programs.beets = {
    enable = true;
    settings = {
      asciify_paths = true;
      convert = {
        copy_album_art = false;
        dest = "${config.home.homeDirectory}/Music";
        embed = true;
        format = "opus";
        formats = {
          opus = "ffmpeg -i $source -y -vn -acodec libopus -ab 192k -vbr on $dest";
        };
        never_convert_lossy_files = true;
        # convert.paths replaces the global paths block, it does not merge, so
        # singleton and comp are repeated here. They are identical to the global
        # ones, so the SD card and the library use the same names.
        paths = {
          default = "%titlecase{$albumartist}/\${year}_%titlecase{$album}/\${track}_\${title}";
          singleton = "%titlecase{$artist}/Non-Album/$title";
          comp = "Various_Artists/%titlecase{$album}/\${track}_\${title}";
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
        write = true;
      };
      paths = {
        default = "%titlecase{$albumartist}/\${year}_%titlecase{$album}/\${track}_\${title}";
        singleton = "%titlecase{$artist}/Non-Album/$title";
        comp = "Various_Artists/%titlecase{$album}/\${track}_\${title}";
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
      # exFAT is case insensitive, so two path names that differ only in case
      # collide on the SD card. force_lowercase makes %titlecase{} lowercase the
      # text before it cases it, so two spellings that differ only in case always
      # produce the same name. auto is off: the tags keep the spelling the band
      # uses, only the paths are normalised.
      titlecase = {
        auto = false;
        force_lowercase = true;
        replace = [ { "æ" = "ae"; } ];
      };
      plugins = [
        "convert"
        "embedart"
        "export"
        "fetchart"
        "lastgenre"
        "random"
        "titlecase"
      ];
    };
  };
}
