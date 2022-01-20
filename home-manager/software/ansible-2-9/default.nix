{ pkgs, ... }:
{
  home.packages = with pkgs; [
    ansible_2_9
    ansible-lint
  ];
}
