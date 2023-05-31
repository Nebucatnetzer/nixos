{ config, lib, ... }:
let
  cfg = config.programs.az-nix-direnv;
in
{
  options = {
    programs.az-nix-direnv.enable = lib.mkEnableOption "Enable nix-direnv";
  };

  config = lib.mkIf cfg.enable {
    nix.extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';

    home-manager.users.${config.az-username} = {
      programs.direnv.enable = true;
      programs.direnv.nix-direnv.enable = true;
    };
  };

}
