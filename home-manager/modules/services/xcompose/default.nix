{ config, lib, ... }:
let
  cfg = config.programs.az-xcompose;
in
{
  options = {
    programs.az-xcompose.enable = lib.mkEnableOption "Make the intl keyboard layout behave like on Windows.";
  };
  config = lib.mkIf cfg.enable { home.file.".XCompose".source = ./XCompose; };
}
