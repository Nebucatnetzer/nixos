{
  repository,
  paths ? [ ],
  pruneSchedule ? "weekly",
  schedule ? "hourly",
}:
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  backupPaths = if paths == [ ] then [ config.home.homeDirectory ] else paths;
  excludeFile = "${inputs.self}/modules/misc/restic-client/excludes.txt";

  # restic refuses an empty password unless --insecure-no-password is passed to every
  # single command, and that flag cannot be expressed through the environment. Overriding
  # the package is the module's only injection point for a global flag; the unset keeps
  # the RESTIC_PASSWORD_FILE the module insists on out of the way.
  restic = pkgs.writeShellApplication {
    name = "restic";
    runtimeInputs = [ pkgs.restic ];
    text = ''
      unset RESTIC_PASSWORD RESTIC_PASSWORD_FILE RESTIC_PASSWORD_COMMAND
      exec restic --insecure-no-password "$@"
    '';
  };

  # /mnt/c is mounted by WSL init, not by systemd --user, so ordering against
  # mnt-c.mount is impossible. An ExecCondition exiting non-zero skips the run
  # instead of failing it.
  guard = pkgs.writeShellApplication {
    name = "restic-wsl-guard";
    runtimeInputs = [ pkgs.util-linux ];
    text = ''
      if ! mountpoint -q /mnt/c; then
        echo "/mnt/c is not mounted, skipping."
        exit 1
      fi
      # Without this restic would silently create the repository on the Linux disk.
      if [ ! -d ${builtins.dirOf repository} ]; then
        echo "${builtins.dirOf repository} is missing, skipping."
        exit 1
      fi
    '';
  };

  commonBackup = {
    package = restic;
    inherit repository;
    # The module asserts that exactly one of passwordFile/passwordCommand is set;
    # the wrapper above unsets it again.
    passwordFile = "/dev/null";
  };
in
{
  services.restic = {
    enable = true;
    backups = {
      wsl = commonBackup // {
        paths = backupPaths;
        initialize = true;
        extraBackupArgs = [
          "--exclude-file=${excludeFile}"
          "--one-file-system"
          "--tag paths"
        ];
        timerConfig = {
          OnCalendar = schedule;
          Persistent = true;
          RandomizedDelaySec = "5m";
        };
      };
      # paths = [ ] makes this a prune only job. runCheck defaults to true because
      # pruneOpts is non-empty, so it also verifies the repository weekly.
      wsl-prune = commonBackup // {
        createWrapper = false;
        pruneOpts = [
          "--tag paths"
          "--keep-daily 7"
          "--keep-weekly 5"
          "--keep-monthly 12"
          "--keep-yearly 2"
        ];
        timerConfig = {
          OnCalendar = pruneSchedule;
          Persistent = true;
          RandomizedDelaySec = "30m";
        };
      };
    };
  };

  systemd.user.services = {
    restic-backups-wsl.Service.ExecCondition = lib.getExe guard;
    restic-backups-wsl-prune.Service.ExecCondition = lib.getExe guard;
  };
}
