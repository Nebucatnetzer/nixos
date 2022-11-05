{ custom }: { ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = custom.username;
  home.homeDirectory = "/home/${custom.username}";
  programs.home-manager.enable = true;
}
