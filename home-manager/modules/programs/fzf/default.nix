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
      changeDirWidgetCommand = "${pkgs.fd}/bin/fd --type d . --color=never --exclude .git --exclude .devenv --exclude .direnv --exclude lost+found";
      defaultCommand = "{pkgs.fd}/bin/fd --type f --exclude .git --exclude .devenv --exclude .direnv --strip-cwd-prefix --exclude lost+found --exclude windows-home";
      defaultOptions = [
        "--border"
        "--height 40%"
        "--walker file"
      ];
      enableBashIntegration = true;
      tmux.enableShellIntegration = true;
    };
  };
}
