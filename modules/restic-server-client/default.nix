{ inputs, custom, time, ... }:
let
  repository = "rest:http://10.7.89.30:8000";
in
{
  services.restic.backups.${custom.username} = {
    user = "root";
    repository = repository;
    timerConfig.OnCalendar = time;
    passwordFile = "/home/${custom.username}/.nixos/secrets/passwords/restic.key";
    paths = [ "/home/${custom.username}/" ];
    extraBackupArgs = [
      "--exclude-file=${inputs.self}/modules/restic/excludes.txt"
    ];
  };

  systemd.services.prune-restic = {
    serviceConfig = {
      User = "restic";
      Type = "oneshot";
      CacheDirectory = "prune-restic";
      CacheDirectoryMode = "0700";
    };
    after = [ "restic-backups-${custom.username}.service" ];
    script = ''
      ${pkgs.restic}/bin/restic \
      --repo ${repository} \
      --password-file "/home/${custom.username}/.nixos/secrets/passwords/restic.key" \
      --cache-dir="/var/cache/prune-restic" \
      forget \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 75 \
    '';
  };
}
