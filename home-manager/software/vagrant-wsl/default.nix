{ pkgs, username, ... }:
{
  home.packages = with pkgs; [
    vagrant
  ];

  programs.bash = {
    sessionVariables = {
      VAGRANT_WSL_ENABLE_WINDOWS_ACCESS = 1;
      PATH = "$PATH:/mnt/c/Program Files/Oracle/VirtualBox";
    };
  };
}
