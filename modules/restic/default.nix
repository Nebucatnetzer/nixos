{ inputs, custom, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    restic
  ];

  services.restic.backups.${custom.username} = {
    user = custom.username;
    repository = "rest:http://10.7.89.30:8000";
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
}
