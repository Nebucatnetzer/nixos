{ config, lib, ... }:

let
  cfg = config.services.az-ssh-luks;
in
{
  options = {
    services.az-ssh-luks.enable = lib.mkEnableOption "Enable the configuration required to unlock Lucks via SSH.";
  };

  config = lib.mkIf cfg.enable {
    boot.initrd.network = {
      enable = true;
      ssh = {
        enable = true;
        port = 22;
        shell = "/bin/cryptsetup-askpass";
        authorizedKeys = config.users.users.${config.az-username}.openssh.authorizedKeys.keys;
        hostKeys = [ "/etc/secrets/initrd/ssh_host_ed25519_key" ];
      };
    };
  };
}
