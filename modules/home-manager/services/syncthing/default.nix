{
  config,
  pkgs,
  ...
}:
{
  systemd.user.services.syncthing = {
    Unit = {
      Description = "Syncthing - Open Source Continuous File Synchronization";
      Documentation = "man:syncthing(1)";
      # Wait for network to be available before starting
      After = [ "network.target" ];
    };
    Service = {
      ExecStart = ''
        ${pkgs.syncthing}/bin/syncthing \
          --no-browser \
          --gui-address=127.0.0.1:8384 \
          --home=/home/${config.home.username}/.config/syncthing
      '';
      Restart = "on-failure";
      RestartSec = 1;
      SuccessExitStatus = [
        3
        4
      ];
      RestartForceExitStatus = [
        3
        4
      ];
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      NoNewPrivileges = true;
      PrivateUsers = true;
      RestrictNamespaces = true;
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}
