{ lib, pkgs, ... }:
let
  delete-current-file = (pkgs.stdenvNoCC.mkDerivation rec {
    name = "mpv-delete-current-file";
    src = pkgs.fetchurl {
      url = "https://git.2li.ch/various/mpv-scripts/raw/commit/e8ff33e18197e10e458e0392b337b5da4a420049/delete_current_file.lua";
      sha256 = "sha256-tsN7zB1eTgGVSKyE010DNajV7/e0tNDvxbVYlMsdoKs=";
    };
    dontBuild = true;
    dontUnpack = true;
    installPhase = ''
      install -Dm644 ${src} $out/share/mpv/scripts/delete_current_file.lua
    '';
    passthru.scriptName = "delete_current_file.lua";
  });
in
{
  programs.mpv = {
    enable = true;
    bindings = {
      s = "playlist-shuffle";
      r = "cycle_values video-rotate 90 180 270 0";
      d = "script-message-to delete_current_file delete-file";
    };
    config = {
      "keepaspect-window" = "no";
    };
    scripts = [ delete-current-file ];
  };

  xdg.mimeApps = {
    enable = true;
    associations.added = {
      "inode/directory" = [ "mpv.desktop" ];
    };
    defaultApplications = {
      "application/mxf" = "mpv.desktop";
      "application/sdp" = "mpv.desktop";
      "application/smil" = "mpv.desktop";
      "application/streamingmedia" = "mpv.desktop";
      "application/vnd.apple.mpegurl" = "mpv.desktop";
      "application/vnd.ms-asf" = "mpv.desktop";
      "application/vnd.rn-realmedia" = "mpv.desktop";
      "application/vnd.rn-realmedia-vbr" = "mpv.desktop";
      "application/x-cue" = "mpv.desktop";
      "application/x-extension-m4a" = "mpv.desktop";
      "application/x-extension-mp4" = "mpv.desktop";
      "application/x-matroska" = "mpv.desktop";
      "application/x-mpegurl" = "mpv.desktop";
      "application/x-ogm" = "mpv.desktop";
      "application/x-ogm-video" = "mpv.desktop";
      "application/x-shorten" = "mpv.desktop";
      "application/x-smil" = "mpv.desktop";
      "application/x-streamingmedia" = "mpv.desktop";
      "video/3gp" = "mpv.desktop";
      "video/3gpp" = "mpv.desktop";
      "video/3gpp2" = "mpv.desktop";
      "video/avi" = "mpv.desktop";
      "video/divx" = "mpv.desktop";
      "video/dv" = "mpv.desktop";
      "video/fli" = "mpv.desktop";
      "video/flv" = "mpv.desktop";
      "video/mkv" = "mpv.desktop";
      "video/mp2t" = "mpv.desktop";
      "video/mp4" = "mpv.desktop";
      "video/mp4v-es" = "mpv.desktop";
      "video/mpeg" = "mpv.desktop";
      "video/msvideo" = "mpv.desktop";
      "video/ogg" = "mpv.desktop";
      "video/quicktime" = "mpv.desktop";
      "video/vnd.divx" = "mpv.desktop";
      "video/vnd.mpegurl" = "mpv.desktop";
      "video/vnd.rn-realvideo" = "mpv.desktop";
      "video/webm" = "mpv.desktop";
      "video/x-avi" = "mpv.desktop";
      "video/x-flc" = "mpv.desktop";
      "video/x-flic" = "mpv.desktop";
      "video/x-flv" = "mpv.desktop";
      "video/x-m4v" = "mpv.desktop";
      "video/x-matroska" = "mpv.desktop";
      "video/x-mpeg2" = "mpv.desktop";
      "video/x-mpeg3" = "mpv.desktop";
      "video/x-ms-afs" = "mpv.desktop";
      "video/x-ms-asf" = "mpv.desktop";
      "video/x-ms-wmv" = "mpv.desktop";
      "video/x-ms-wmx" = "mpv.desktop";
      "video/x-ms-wvxvideo" = "mpv.desktop";
      "video/x-msvideo" = "mpv.desktop";
      "video/x-ogm" = "mpv.desktop";
      "video/x-ogm+ogg" = "mpv.desktop";
      "video/x-theora" = "mpv.desktop";
      "video/x-theora+ogg" = "mpv.desktop";
    };
  };
}

