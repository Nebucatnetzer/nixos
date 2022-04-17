{ pkgs, ... }:
{
  home.packages = with pkgs; [
    ansible
    ansible-lint
    nodePackages.prettier
    sshpass
  ];
}
