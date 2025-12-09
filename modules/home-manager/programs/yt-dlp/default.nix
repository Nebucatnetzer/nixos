{ inputs, pkgs, ... }:
let
  unstable = inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system};
in
{
  programs.yt-dlp = {
    enable = true;
    package = unstable.yt-dlp;
    settings = {
      remux-video = "mkv";
    };
  };
}
