{ inputs, custom, pkgs, ... }:
let
  repository = "rest:http://10.7.89.30:8000";
in
{
  environment.systemPackages = with pkgs; [
    restic
  ];

  services.restic.backups.${custom.username} = {
    user = custom.username;
    repository = repository;
    timerConfig = {
      OnCalendar = "hourly";
      RandomizedDelaySec = "15min";
    };
    passwordFile = "/home/${custom.username}/.nixos/secrets/passwords/restic.key";
    paths = [ "/home/${custom.username}/" ];
    extraBackupArgs = [
      "--exclude-file=${inputs.self}/modules/restic/excludes.txt"
    ];
  };

  systemd.services.prune-restic = {
    User = custom.username;
    serviceConfig.Type = "oneshot";
    After = "restic-backups-${custom.username}.service";
    script = ''
      ${pkgs.restic}/bin/restic \
      --repo ${repository} \
      --password-file "/home/${custom.username}/.nixos/secrets/passwords/restic.key" \
      forget \
        --keep-hourly 25 \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 75 \
    '';
  };
}
