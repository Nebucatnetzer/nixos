{ ... }:
let
  username = import ../../username.nix;
in
{
  services.restic.backups.${username} = {
    user = username;
    repository = "sftp:borg@10.7.89.117:restic";
    timerConfig.OnCalendar = "hourly";
    passwordFile = "/home/${username}/git_repos/nixos/secrets/passwords/restic.key";
    paths = [ "/home/${username}/" ];
    extraBackupArgs = [
      "--exclude-file=/home/${username}/git_repos/nixos/modules/restic/excludes.txt"
    ];

  };
}
