{ inputs, custom, pkgs, ... }:
{
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  home-manager.users.${custom.username} = {
    programs.direnv.enable = true;
    programs.direnv.nix-direnv.enable = true;
    programs.direnv.nix-direnv.enableFlakes = true;
  };
}
