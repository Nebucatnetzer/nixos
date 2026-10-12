{
  config,
  lib,
  pkgs,
  ...
}:
let
  secondsPerDay = 24 * 60 * 60;

  idleCondition = pkgs.writers.writePython3Bin "idle-maintenance-condition" {
    flakeIgnore = [ "E501" ];
    makeWrapperArgs = [
      "--prefix"
      "PATH"
      ":"
      (lib.makeBinPath [ pkgs.systemd ])
    ];
  } (builtins.readFile ./idle_maintenance_condition.py);

  idleServiceConfig =
    unitName: minimumIntervalDays:
    let
      stampFile = "/var/lib/idle-maintenance/${unitName}/last-run";
      minimumIntervalSeconds = toString (minimumIntervalDays * secondsPerDay);
    in
    {
      StateDirectory = "idle-maintenance/${unitName}";
      ExecCondition = "${lib.getExe idleCondition} ${stampFile} ${minimumIntervalSeconds}";
      ExecStartPost = "${lib.getExe' pkgs.coreutils "touch"} ${stampFile}";
    };
in
{
  nix.gc = {
    dates = "hourly";
    persistent = false;
  };
  services.fstrim.interval = "hourly";

  systemd.services.fstrim = lib.mkIf config.services.fstrim.enable {
    unitConfig.ConditionACPower = true;
    serviceConfig = idleServiceConfig "fstrim" 30;
  };
  systemd.services.nix-gc = lib.mkIf config.nix.gc.automatic {
    unitConfig.ConditionACPower = true;
    serviceConfig = idleServiceConfig "nix-gc" 7;
  };
}
