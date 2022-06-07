{ inputs, custom, pkgs, ... }:
let
  repository = "/mnt/restic-server";
in
{
  environment.systemPackages = with pkgs; [
    restic
  ];

  fileSystems.${repository} = {
    device = "10.7.89.108:restic-server";
    fsType = "nfs";
    options = [ "noatime" "hard" ];
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
    script = ''
      ${pkgs.restic}/bin/restic \
      --repo ${repository} \
      --password-file "/etc/restic/restic.key" \
      prune \
    '';
  };

  systemd.timers.restic-prune = {
    wantedBy = [ "timers.target" ];
    partOf = [ "restic-prune.service" ];
    timerConfig.OnCalendar = [ "*-*-* 12:00:00" ];
  };
}
