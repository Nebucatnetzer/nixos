{ self, time, username, ... }:
{
  services.restic.backups.${username} = {
    user = "root";
    repository = "rest:http://10.7.89.30:8000";
    timerConfig.OnCalendar = time;
    passwordFile = "/home/${username}/.nixos/secrets/passwords/restic.key";
    paths = [ "/home/${username}/" ];
    extraBackupArgs = [
      "--exclude-file=${self}/modules/restic/excludes.txt"
    ];
    pruneOpts = [
      "--keep-daily 7"
      "--keep-weekly 5"
      "--keep-monthly 12"
    ];
  };
}
