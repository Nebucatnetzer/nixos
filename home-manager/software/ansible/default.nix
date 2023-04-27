{ pkgs, ... }:
{
  home = {
    # NPM is required for the language server which I currently can only
    # install through npm because the version in Nix is broken 2023-04-27
    sessionPath = [ "$HOME/.local/share/node_modules/bin" ];
    sessionVariables = {
      NPM_CONFIG_PREFIX = "$HOME/.local/share/node_modules";
    };
    packages = with pkgs; [
      sshpass # it's the only system package that I need to run Ansible
    ];
  };
}
