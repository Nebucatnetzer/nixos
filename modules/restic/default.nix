{ inputs, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    restic
  ];

  services.restic.backups.${inputs.custom.username} = {
    user = username;
    repository = "rest:http://10.7.89.30:8000";
    timerConfig = {
      OnCalendar = "hourly";
      RandomizedDelaySec = "15min";
    };
    passwordFile = "/home/${inputs.custom.username}/.nixos/secrets/passwords/restic.key";
    paths = [ "/home/${inputs.custom.username}/" ];
    extraBackupArgs = [
      "--exclude-file=${inputs.self}/modules/restic/excludes.txt"
    ];
  };
}
