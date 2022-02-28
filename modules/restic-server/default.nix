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
  };
  services.restic.server = {
    enable = true;
    dataDir = repository;
    extraFlags = [ "--no-auth" ];
  };
  networking.firewall.allowedTCPPorts = [ 8000 ];

  systemd.services.prune-restic = {
    serviceConfig.Type = "oneshot";
    script = ''
      ${pkgs.restic}/bin/restic \
      --repo ${repository} \
      --password-file "/home/${custom.username}/.nixos/secrets/passwords/restic.key" \
      forget \
        --keep-daily 7 \
        --keep-weekly 5 \
        --keep-monthly 12 \
        --keep-yearly 75 \
        --prune
    '';
  };

  systemd.timers.prune-restic = {
    wantedBy = [ "timers.target" ];
    partOf = [ "prune-restic.service" ];
    timerConfig.OnCalendar = [ "*-*-* 12:00:00" ];
  };
}
