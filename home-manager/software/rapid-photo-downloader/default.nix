{ custom }: { pkgs, ... }:
let
  # TODO: switch back to stable on 23.05
  unstable = import custom.inputs.nixpkgs-unstable {
    system = "x86_64-linux";
  };
in
{
  home.packages = [
    unstable.rapid-photo-downloader
  ];
  home.file.".config/Rapid Photo Downloader".source = ./config;
}
