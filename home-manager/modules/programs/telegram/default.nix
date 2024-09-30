{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-telegram;
  telegram = inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.tdesktop;
in
{
  options = {
    programs.az-telegram.enable = lib.mkEnableOption "Enable Telegram.";
  };

  config = lib.mkIf cfg.enable { home.packages = [ telegram ]; };
}
