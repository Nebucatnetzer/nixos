{ self, pkgs, username, ... }:
{
  environment.systemPackages = with pkgs; [
    restic
  ];

  services.restic.backups.${username} = {
    user = username;
    repository = "rest:http://10.7.89.30:8000";
    timerConfig = {
      OnCalendar = "hourly";
      RandomizedDelaySec = "15min";
    };
    passwordFile = "/home/${username}/.nixos/secrets/passwords/restic.key";
    paths = [ "/home/${username}/" ];
    extraBackupArgs = [
      "--exclude-file=${self}/modules/restic/excludes.txt"
    ];
  };
}
