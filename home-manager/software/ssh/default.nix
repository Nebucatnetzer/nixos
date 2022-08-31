{ ... }: {
  programs.ssh = {
    enable = true;
    extraConfig = ''
      Host nixos.2li.local
        StrictHostKeyChecking no
        UserKnownHostsFile /dev/null
        User nixos
        LogLevel QUIET

      Host sensors.2li.local
        User ubuntu
        IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key

      Host *.2li.local
        User andreas
        IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key

      Host 10.7.89.*
        User andreas
        IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key
    '';
  };
}
