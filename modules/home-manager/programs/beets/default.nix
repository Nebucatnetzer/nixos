{ ... }:
{
  programs.bash.shellAliases = {
    pra = ''mpv "$(beet random -a -p)" &'';
  };

  programs.beets = {
    enable = true;
    settings = {
      asciify_paths = true;
      convert = {
        copy_album_art = "yes";
        format = "opus";
        formats = {
          opus = "ffmpeg -i $source -y -vn -acodec libopus -ab 256k -vbr on $dest";
        };
        never_convert_lossy_files = true;
      };
      directory = "/mnt/fileserver/media/audio/music/music";
      fetchart = {
        auto = true;
      };
      import = {
        autotag = false;
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
      plugins = [
        "convert"
        "export"
        "fetchart"
        "lastgenre"
        "random"
      ];
    };
  };
}
