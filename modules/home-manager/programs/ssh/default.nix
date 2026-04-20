{ ... }:
{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    extraConfig = ''
      Host nixos.int.zweili.org
        StrictHostKeyChecking no
        UserKnownHostsFile /dev/null
        User nixos

      Host *.int.zweili.org
        User andreas
        IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key

      Host *.vpn.zweili.org
        User andreas
        IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key

      Host 10.7.89.*
        User andreas
        IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key

      Host 10.70.89.*
        User andreas
        IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key

      Host 10.49.0.100
        User zweili
        IdentityFile ~/.ssh/zweili.key
    '';
    matchBlocks."*" = {
      forwardAgent = false;
      addKeysToAgent = "no";
      compression = false;
      serverAliveInterval = 0;
      serverAliveCountMax = 3;
      hashKnownHosts = false;
      userKnownHostsFile = "~/.ssh/known_hosts";
      controlMaster = "no";
      controlPath = "~/.ssh/master-%r@%n:%p";
      controlPersist = "no";
    };
  };
}
