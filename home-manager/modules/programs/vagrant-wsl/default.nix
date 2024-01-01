{ config, lib, pkgs, ... }:
let cfg = config.programs.az-vagrant-wsl;
in {
  options = {
    programs.az-vagrant-wsl.enable =
      lib.mkEnableOption "Enable Vagrant in WSL.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ vagrant ];

    programs.bash = {
      sessionVariables = {
        VAGRANT_WSL_ENABLE_WINDOWS_ACCESS = 1;
        PATH = "$PATH:/mnt/c/Program Files/Oracle/VirtualBox";
      };
    };
  };
}
