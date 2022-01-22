{ self, ... }:
let
  username = import "${self}/username.nix";
in
{
  services.restic.backups.${username} = {
    user = username;
    repository = "sftp:borg@10.7.89.117:restic";
    timerConfig.OnCalendar = "hourly";
    passwordFile = "/home/${username}/.nixos/secrets/passwords/restic.key";
    paths = [ "/home/${username}/" ];
    extraBackupArgs = [
      "--exclude-file=/home/${username}/.nixos/modules/restic/excludes.txt"
    ];

  };
}
