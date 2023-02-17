{ pkgs, ... }:
{
  home.packages = with pkgs; [
    sshpass # it's the only system package that I need to run Ansible
  ];
}
