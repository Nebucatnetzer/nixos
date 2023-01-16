{ custom }: { pkgs, ... }:
let
  rpdfix = import custom.inputs.nixpkgs-rpdfix {
    system = "x86_64-linux";
  };
in
{
  home.packages = [
    rpdfix.rapid-photo-downloader
  ];
  home.file.".config/Rapid Photo Downloader".source = ./config;
}
