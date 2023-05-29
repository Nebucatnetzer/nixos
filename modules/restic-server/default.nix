{ config, inputs, pkgs, ... }:
let
  repository = "/var/lib/restic-server";
in
{
  imports = [
    "${inputs.self}/modules/telegram-notifications"
  ];
  age.secrets.resticKey = {
    file = "${inputs.self}/scrts/restic.key.age";
    mode = "440";
    owner = "restic";
    group = "restic";
  };

  environment.systemPackages = with pkgs; [
    restic
  ];

  fileSystems."${repository}" = {
    device = "10.7.89.108:restic-server";
    fsType = "nfs";
    options = [ "noatime" "hard" "nfsvers=4.0" ];
  };
  services.restic.server = {
    enable = true;
    dataDir = repository;
    extraFlags = [ "--no-auth" ];
  };
  networking.firewall.allowedTCPPorts = [ 8000 ];

  systemd.services.restic-prune = {
    serviceConfig = {
      Type = "oneshot";
      User = "restic";
    };
    onFailure = [ "unit-status-telegram@%n.service" ];
    script = ''
      ${pkgs.restic}/bin/restic \
      --repo ${repository} \
      --password-file ${config.age.secrets.resticKey.path} \
      prune \
    '';
  };

  systemd.timers.restic-prune = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-prune.service" ];
    timerConfig.OnCalendar = [ "*-*-* 08:00:00" ];
  };

  systemd.services.restic-check = {
    serviceConfig = {
      Type = "oneshot";
      User = "restic";
    };
    onFailure = [ "unit-status-telegram@%n.service" ];
    script = ''
      ${pkgs.restic}/bin/restic \
      --repo ${repository} \
      --password-file ${config.age.secrets.resticKey.path} \
      check \
    '';
  };
  systemd.timers.restic-check = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-check.service" ];
    timerConfig.OnCalendar = [ "*-*-* 07:00:00" ];
  };
}
