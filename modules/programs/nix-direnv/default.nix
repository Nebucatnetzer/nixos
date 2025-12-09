{ config, ... }:
{
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  home-manager.users.${config.az-username} = {
    programs.direnv.enable = true;
    programs.direnv.nix-direnv.enable = true;
  };
}
