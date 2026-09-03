{
  pkgs,
  unstable-pkgs,
}:
let
  inherit (unstable-pkgs) yt-dlp;
  hosts = import ../modules/misc/hosts/hosts.nix;
  mediaPaths = import ./mediaPaths.nix;
in
rec {
  az-emacs = pkgs.callPackage ./az-emacs {
    inherit unstable-pkgs;
    emacsDir = ../modules/home-manager/programs/emacs;
  };
  az-media =
    pkgs.lib.meta.addMetaAttrs
      {
        description = "Open four shuffled mpv instances for ambient video playback";
        license = pkgs.lib.licenses.gpl3Plus;
        mainProgram = "az-media";
        platforms = pkgs.lib.platforms.linux;
      }
      (
        pkgs.writeShellScriptBin "az-media" ''
          videos="videos"
          directory="''${1:-videos}"
          for i in $(seq 1 4);
          do
              mpv --shuffle --mute=yes "${mediaPaths.variousVideos}/$directory/" &
          done
        ''
      );
  custom-mpv = pkgs.callPackage ./mpv { };
  dap-sync = pkgs.callPackage ./dap-sync { };
  date-to-filename = pkgs.callPackage ./date-to-filename { };
  denote-rename = pkgs.callPackage ./denote-rename { };
  download-articles = pkgs.callPackage ./download-articles { };
  download-video = pkgs.callPackage ./download-video { inherit yt-dlp mediaPaths; };
  download-playlist = pkgs.callPackage ./download-playlist { inherit yt-dlp mediaPaths; };
  dptfxtract = pkgs.callPackage ./dptfxtract { };
  jdownloader = pkgs.callPackage ./jdownloader { inherit mediaPaths; };
  qudedup-extract-tool = pkgs.callPackage ./qudedup-extract-tool { };
  raiffeisen-csv-cleanup = pkgs.callPackage ./raiffeisen-csv-cleanup { };
  rebuild = pkgs.callPackage ./rebuild { builderHost = hosts.fenoglio.wgIp; };
  sidecar-cleanup = pkgs.callPackage ./sidecar-cleanup { };
  swiss-qr-bill-decoder = pkgs.callPackage ./swiss-qr-bill-decoder { };
  toggle-keyboard = pkgs.callPackage ./toggle-keyboard { };
  tube-podder = pkgs.callPackage ./tube-podder { inherit yt-dlp; };
  unlock-luks = pkgs.callPackage ./unlock-luks { };
  unsloth-studio = pkgs.callPackage ./unsloth-studio { };
  update-file-dates = pkgs.callPackage ./update-file-dates { };
  update-hosts = pkgs.callPackage ./update-hosts { };
  video-to-mpv = pkgs.callPackage ./video-to-mpv {
    inherit yt-dlp;
    mpv = custom-mpv;
  };
  watch-playlist = pkgs.callPackage ./watch-playlist {
    mpv = custom-mpv;
    inherit mediaPaths;
  };
  watch-random-video = pkgs.callPackage ./watch-random-video {
    mpv = custom-mpv;
    inherit mediaPaths;
  };
  watch-video = pkgs.callPackage ./watch-video {
    mpv = custom-mpv;
    inherit mediaPaths;
  };
  win32yank = pkgs.callPackage ./win32yank { inherit (pkgs) pkgsCross; };
  zotero-mcp = pkgs.callPackage ./zotero-mcp { };
  container-updates = pkgs.callPackage ./container-updates { };
  dry-build-all = pkgs.callPackage ./dry-build-all { };
  fingerprint-fix = pkgs.callPackage ./fingerprint-fix { };
  format-disk = pkgs.callPackage ./format-disk { };
  install-home-manager = pkgs.callPackage ./install-home-manager { };
  mount-btrfs-partitions = pkgs.callPackage ./mount-btrfs-partitions { };
  rename-partitions = pkgs.callPackage ./rename-partitions { };
  run-command = pkgs.callPackage ./run-command { };
  test-build = pkgs.callPackage ./test-build { };
  unmount-btrfs-partitions = pkgs.callPackage ./unmount-btrfs-partitions { };
}
