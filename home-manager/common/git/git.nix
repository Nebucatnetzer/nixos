{config, pkgs, ...}:
{
  programs.git = {
    enable = true;
    userName = "Andreas Zweili";
    delta = {
      enable = true;
      options = {
        navigate = true;
        line-numbers = true;
        syntax-theme = "GitHub";
      };
    };
  };
  # raw files
  home.file.".config/git/hooks".source = ./hooks;
}
