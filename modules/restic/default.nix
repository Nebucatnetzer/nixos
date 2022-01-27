{ username, ... }:
{
  services.restic.backups.${username} = {
    user = username;
    repository = "rest:http://10.7.89.30:8000";
    timerConfig.OnCalendar = "hourly";
    passwordFile = "/home/${username}/.nixos/secrets/passwords/restic.key";
    paths = [ "/home/${username}/" ];
    extraBackupArgs = [
      "--exclude-file=/home/${username}/.nixos/modules/restic/excludes.txt"
    ];

  };
}
