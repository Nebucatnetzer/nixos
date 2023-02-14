{ pkgs, ... }:
{
  home.packages = with pkgs; [
    unstable.tdesktop
  ];
}
