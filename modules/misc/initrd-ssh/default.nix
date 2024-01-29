{ config, inputs, ... }: {
  boot.initrd.network = {
    enable = true;
    ssh = {
      enable = true;
      port = 22;
      shell = "/bin/cryptsetup-askpass";
      authorizedKeys =
        config.users.users.${config.az-username}.openssh.authorizedKeys.keys;
      hostKeys = [ "/etc/secrets/initrd/ssh_host_ed25519_key" ];
    };
  };
}
