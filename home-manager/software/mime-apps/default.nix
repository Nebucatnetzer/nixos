{ ... }:
{
  # forcecully override the mimeapps.list
  # this is required because it isn't a file nix can easily lock
  # https://github.com/nix-community/home-manager/issues/1213
  xdg.configFile."mimeapps.list".force = true;
}
