{ inputs }: { config, pkgs, ... }:
let
  repository = "/mnt/restic-server";
in
{
  age.secrets.resticKey.file = "${inputs.self}/scrts/restic.key.age";

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
    timerConfig.OnCalendar = [ "*-*-* 12:00:00" ];
  };
}
