{ config, pkgs, ... }:
{
  services.restic.backups."andreas" = {
    user = "andreas";
    repository = "sftp:borg@10.7.89.117:restic";
    timerConfig.OnCalendar = "hourly";
    passwordFile = "/home/andreas/git_repos/nixos/secrets/passwords/restic.key";
    paths = [ "/home/andreas/" ];
    extraBackupArgs = [
      "--exclude-file=/home/andreas/git_repos/nixos/modules/restic/excludes.txt"
    ];

  };
}
