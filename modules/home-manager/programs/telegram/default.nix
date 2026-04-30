{ unstable-pkgs, ... }:
let
  telegram = unstable-pkgs.telegram-desktop;
in
{
  home.packages = [ telegram ];
}
