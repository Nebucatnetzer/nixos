{ pkgs, ... }:
{
  home.packages = with pkgs; [
    ansible
    ansible-lint
    nodejs
    nodePackages.npm
    nodePackages.prettier
    sshpass
  ];
}
