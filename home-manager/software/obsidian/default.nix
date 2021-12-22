{ pkgs, ... }:
{
  home.packages = with pkgs; [
    unstable.obsidian
  ];
}
