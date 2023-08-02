{ config, lib, ... }:
let
  cfg = config.programs.az-git;
in
{
  options = {
    programs.az-git = {
      enable = lib.mkEnableOption "Enable git.";
      userEmail = lib.mkOption {
        type = lib.types.str;
        description = "The hostname of the system.";
        default = "andreas@zweili.ch";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    programs.git = {
      enable = true;
      userName = "Andreas Zweili";
      userEmail = cfg.userEmail;
      delta = {
        enable = true;
        options = {
          navigate = true;
          line-numbers = true;
          syntax-theme = "GitHub";
        };
      };
      includes = [{
        path = "~/.config/git/workconfig";
        condition = "gitdir:~/git_repos/work/";
      }];
      extraConfig = {
        core = {
          hooksPath = "~/.config/git/hooks/";
        };
        safe = {
          directory = "*";
        };
        pull = {
          rebase = false;
        };
        push = {
          autoSetupRemote = true;
        };
      };
      ignores = [
        # ---> VisualStudioCode
        ".vscode/*"
        "!.vscode/settings.json"
        "!.vscode/tasks.json"
        "!.vscode/launch.json"
        "!.vscode/extensions.json"
        "*.code-workspace"

        # Local History for Visual Studio Code"
        ".history/"

        # ---> Emacs"
        # -*- mode: gitignore; -*-"
        "*~"
        "\#*\#"
        "/.emacs.desktop"
        "/.emacs.desktop.lock"
        "*.elc"
        "auto-save-list"
        "tramp"
        ".\#*"

        # Org-mode"
        ".org-id-locations"
        "*_archive"

        # flymake-mode"
        "*_flymake.*"

        # eshell files"
        "/eshell/history"
        "/eshell/lastdir"

        # elpa packages"
        "/elpa/"

        # reftex files"
        "*.rel"

        # AUCTeX auto folder"
        "/auto/"

        # cask packages"
        ".cask/"
        "dist/"

        # Flycheck"
        "flycheck_*.el"

        # server auth directory"
        "/server/"

        # projectiles files"
        ".projectile"

        # directory configuration"
        ".dir-locals.el"

        # network security"
        "/network-security.data"


        # ---> Vim"
        # Swap"
        "[._]*.s[a-v][a-z]"
        "!*.svg  # comment out if you don't need vector files"
        "[._]*.sw[a-p]"
        "[._]s[a-rt-v][a-z]"
        "[._]ss[a-gi-z]"
        "[._]sw[a-p]"

        # Session"
        "Session.vim"
        "Sessionx.vim"

        # Temporary"
        ".netrwhist"
        "*~"
        # Auto-generated tag files"
        "tags"
        # Persistent undo"
        "[._]*.un~"

        # ignore pycache"
        "__pycache__/"

      ];
    };
    # raw files
    home.file.".config/git/hooks".source = ./hooks;
    home.file.".config/git/workconfig".source = ./workconfig;
    home.shellAliases = {
      git-clean = ''
        git fetch --all -p;
        git branch --merged origin/master | grep -v "\*" | xargs git branch -d;
        git branch -vv | grep -v '\[origin/'| grep -v "\*" | awk '{ print $1; }' | xargs -r git branch -D;
      '';
    };
  };
}
