{ inputs, custom, time, ... }:
{
  services.restic.backups.${custom.username} = {
    user = "root";
    repository = "rest:http://10.7.89.30:8000";
    timerConfig.OnCalendar = time;
    passwordFile = "/home/${custom.username}/.nixos/secrets/passwords/restic.key";
    paths = [ "/home/${custom.username}/" ];
    extraBackupArgs = [
      "--exclude-file=${inputs.self}/modules/restic/excludes.txt"
    ];
    pruneOpts = [
      "--keep-hourly 24"
      "--keep-daily 7"
      "--keep-weekly 5"
      "--keep-monthly 12"
    ];
  };
}
