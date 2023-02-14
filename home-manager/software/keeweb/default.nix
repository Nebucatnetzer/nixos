{ pkgs, ... }:
{
  home.packages = with pkgs; [
    keeweb
  ];
}
