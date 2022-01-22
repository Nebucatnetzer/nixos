{ self, ... }:
let
  username = import "${self}/username.nix";
in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = username;
  home.homeDirectory = "/home/${username}";
  programs.home-manager.enable = true;
}
