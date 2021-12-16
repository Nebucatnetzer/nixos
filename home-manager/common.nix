{ pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "andreas";
  home.homeDirectory = "/home/andreas";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  # home.stateVersion = import ../version.nix;

  # Let Home Manager install and manage itself.
  # programs.home-manager.enable = true;

  home.packages = with pkgs; [
    ansible
    ansible-lint
    mosh
    stow
    tmux
  ];

  home.shellAliases = {
    dist-upgrade = "sudo nixos-rebuild switch -j auto";
  };

  programs.vim = {
    enable = true;
    settings = {
      expandtab = true;
      tabstop = 4;
      shiftwidth = 4;
      number = true;
      relativenumber = true;
    };
  };

}
