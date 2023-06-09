{ config, lib, pkgs, ... }:
let
  cfg = config.programs.az-ansible;
in
{
  options = {
    programs.az-ansible.enable = lib.mkEnableOption "Enable ansible.";
  };

  config = lib.mkIf cfg.enable {
    home = {
      # NPM is required for the language server which I currently can only
      # install through npm because the version in Nix is broken 2023-04-27
      sessionPath = [ "$HOME/.local/share/node_modules/bin" ];
      sessionVariables = {
        NPM_CONFIG_PREFIX = "$HOME/.local/share/node_modules";
      };
      packages = [
        pkgs.nodejs # needed for ansible-language-server
        pkgs.sshpass # it's the only system package that I need to run Ansible
      ];
    };

    programs.emacs = {
      extraPackages = epkgs: with pkgs;[
        pkgs.ansible
      ];
    };
  };
}
