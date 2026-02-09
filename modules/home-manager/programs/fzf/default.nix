{ pkgs, ... }:
{
  programs.fzf = {
    enable = true;
    changeDirWidgetCommand = "${pkgs.fd}/bin/fd --type d . --color=never --exclude .git --exclude .devenv --exclude .direnv --exclude lost+found";
    fileWidgetCommand = "${pkgs.fd}/bin/fd --type f --exclude .git --exclude .devenv --exclude .direnv --strip-cwd-prefix --exclude lost+found --exclude windows-home";
    defaultOptions = [
      "--border"
      "--height 40%"
      "--walker file"
    ];
    enableBashIntegration = true;
    tmux.enableShellIntegration = true;
  };
}
