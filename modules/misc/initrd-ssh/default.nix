{ config, ... }:
{
  boot.initrd = {
    systemd.users.root.shell = "/bin/systemd-tty-ask-password-agent";
    network = {
      enable = true;
      ssh = {
        enable = true;
        port = 22;
        authorizedKeys = config.users.users.${config.az-username}.openssh.authorizedKeys.keys;
        hostKeys = [ "/etc/secrets/initrd/ssh_host_ed25519_key" ];
      };
    };
  };
}
