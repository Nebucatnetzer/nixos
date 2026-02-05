{
  fetchurl,
  ffmpeg-full,
  mpv-unwrapped,
  stdenvNoCC,
}:
let
  delete-file = (
    stdenvNoCC.mkDerivation rec {
      name = "mpv-delete-file";
      src = fetchurl {
        url = "https://raw.githubusercontent.com/zenyd/mpv-scripts/19ea069abcb794d1bf8fac2f59b50d71ab992130/delete_file.lua";
        sha256 = "sha256-1FX23t+O1aFZnbuvl+9zDT8OcKEziWNGj5cAMSvRIas=";
      };
      dontBuild = true;
      dontUnpack = true;
      installPhase = ''
        install -Dm644 ${src} $out/share/mpv/scripts/delete_file.lua
      '';
      passthru.scriptName = "delete_file.lua";
    }
  );
  move-watched = (
    stdenvNoCC.mkDerivation rec {
      name = "mpv-move-watched";
      src = ./move_watched.lua;
      dontBuild = true;
      dontUnpack = true;
      installPhase = ''
        install -Dm644 ${src} $out/share/mpv/scripts/move_watched.lua
      '';
      passthru.scriptName = "move_watched.lua";
    }
  );
in
mpv-unwrapped.wrapper {
  mpv = mpv-unwrapped.override {
    ffmpeg = ffmpeg-full;
  };
  scripts = [
    delete-file
    move-watched
  ];
  youtubeSupport = true;
}
