{
  repoRoot ? "/var/lib/git",
}:
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  builderHost = config.az-hosts.fenoglio.wgIp;
  cacheDirectory = "/var/cache/flake-updater";
  defaultSchedule = "Sat *-*-* 04:00:00";
  defaultUpdateScript = "nix flake update --commit-lock-file";
  # The update settings live with the repository in repos.nix.
  repos = lib.mapAttrs (name: repo: repo.update) (
    lib.filterAttrs (name: repo: repo ? update) (
      import "${inputs.self}/modules/services/git-server/repos.nix"
    )
  );
  send-to-telegram =
    pkgs.callPackage "${inputs.self}/modules/services/telegram-notifications/send_to_telegram.nix"
      {
        envFile = config.age.secrets.telegramNotifyEnv.path;
      };
  slug = name: lib.replaceStrings [ "/" ] [ "-" ] name;
  unitName = name: "flake-update-${slug name}";
  # The post-receive hook mirrors this branch to GitHub as well.
  updateBranch = "update/automatic";

  updateScript =
    name: repo:
    pkgs.writeShellApplication {
      name = unitName name;
      runtimeInputs = [
        pkgs.bash
        pkgs.git
        pkgs.netcat
        config.nix.package
        send-to-telegram
      ];
      text = ''
        work_tree=${cacheDirectory}/${slug name}
        rm --recursive --force "$work_tree"
        git clone --quiet ${repoRoot}/${name}.git "$work_tree"
        cd "$work_tree"

        bash -c ${lib.escapeShellArg (repo.script or defaultUpdateScript)}
        if [ -n "$(git status --porcelain)" ]; then
          git add --all
          git commit --quiet --message "Automatic update"
        fi
        if [ "$(git rev-parse HEAD)" = "$(git rev-parse '@{upstream}')" ]; then
          echo "Nothing to update."
          exit 0
        fi
      ''
      + lib.optionalString (repo ? tests) ''
        # Build only on fenoglio. Local builds would fill the store and load the CPU of this host.
        if ! nc -zw2 ${builderHost} 22 >/dev/null 2>&1; then
          echo "Builder ${builderHost} is unreachable." >&2
          exit 1
        fi
        bash -c ${lib.escapeShellArg repo.tests}
      ''
      + ''
        git push --force origin HEAD:refs/heads/${updateBranch}
        changes="$(git diff --stat '@{upstream}' HEAD)"
        send-to-telegram "${name}: ${updateBranch} is ready.
        $changes"
      '';
    };

  updateService = name: repo: {
    description = "Update ${name}";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    onFailure = [ "unit-status-telegram@%N.service" ];
    environment = {
      # Inputs from this git server are fetched from the bare repositories, without SSH.
      # flake.lock still records the ssh:// URL.
      GIT_CONFIG_COUNT = "1";
      GIT_CONFIG_KEY_0 = "url.file://${repoRoot}/.insteadOf";
      GIT_CONFIG_VALUE_0 = "ssh://git@git.zweili.org/";
      GIT_AUTHOR_NAME = "flake-updater";
      GIT_AUTHOR_EMAIL = "git@${config.networking.hostName}";
      GIT_COMMITTER_NAME = "flake-updater";
      GIT_COMMITTER_EMAIL = "git@${config.networking.hostName}";
      # The real home is the repository root, which restic backs up.
      HOME = cacheDirectory;
    };
    serviceConfig = {
      Type = "oneshot";
      User = "git";
      Group = "git";
      CacheDirectory = "flake-updater";
      ExecStart = lib.getExe (updateScript name repo);

      # The update script and the tests run code from the repository. Keep them away
      # from the mirror key and from the other repositories.
      InaccessiblePaths = [ config.age.secrets.githubMirrorKey.path ];
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectHome = true;
      ProtectSystem = "strict";
      ReadWritePaths = [
        "${repoRoot}/${name}.git"
        "${repoRoot}/.spool"
      ];
    };
  };

  updateTimer = name: repo: {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = repo.schedule or defaultSchedule;
      Persistent = true;
    };
  };
in
{
  imports = [
    "${inputs.self}/modules/services/telegram-notifications"
  ];

  systemd.services = lib.mapAttrs' (
    name: repo: lib.nameValuePair (unitName name) (updateService name repo)
  ) repos;
  systemd.timers = lib.mapAttrs' (
    name: repo: lib.nameValuePair (unitName name) (updateTimer name repo)
  ) repos;
}
