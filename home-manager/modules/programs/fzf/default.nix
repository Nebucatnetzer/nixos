{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-fzf;
in
{
  options = {
    programs.az-fzf.enable = lib.mkEnableOption "My config for fzf.";
  };

  config = lib.mkIf cfg.enable {
    programs.fzf = {
      enable = true;
      enableBashIntegration = true;
      tmux.enableShellIntegration = true;
    };
  };
}
