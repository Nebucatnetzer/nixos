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
      includes = [
        {
          path = "~/.config/git/workconfig";
          condition = "gitdir:~/git_repos/work/";
        }
      ];
      extraConfig = {
        branch.sort = "-committerdate";
        core = {
          autocrlf = "input";
          fsmonitor = true; # not clear if I keep it
          untrackedCache = true; # not clear if I keep it
        };
        diff = {
          algorithm = "histogram";
          colorMoved = true;
          mnemonicPrefix = true;
          renames = true;
        };
        feature.experimental = true;
        fetch = {
          all = true;
          prune = true; # not clear if I keep it
          pruneTags = true; # not clear if I keep it
        };
        grep.patternType = "perl";
        help.autocorrect = "prompt";
        merge.conflictStyle = "zdiff3";
        pull.ff = "only"; # not clear if I keep it
        push.autoSetupRemote = true;
        rebase = {
          autosquash = true;
          autostash = true;
          updateRefs = true;
        };
        rerere = {
          autoupdate = true;
          enabled = true;
        };
        safe.directory = "*";
        tag.sort = "version:refname";
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
        "#*#"
        "/.emacs.desktop"
        "/.emacs.desktop.lock"
        "*.elc"
        "auto-save-list"
        "tramp"
        ".#*"

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
