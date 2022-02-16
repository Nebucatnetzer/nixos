{ inputs, ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = inputs.custom.username;
  home.homeDirectory = "/home/${inputs.custom.username}";
  programs.home-manager.enable = true;
}
