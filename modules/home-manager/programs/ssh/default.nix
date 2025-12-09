{ ... }:
{
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    extraConfig = ''
      Host nixos.2li.local
        StrictHostKeyChecking no
        UserKnownHostsFile /dev/null
        User nixos

      Host mobile.2li.local
        StrictHostKeyChecking no
        UserKnownHostsFile /dev/null

      Host *.2li.local
        User andreas
        IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key

      Host 10.7.89.*
        User andreas
        IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key

      Host 10.49.0.100
        User zweili
        IdentityFile ~/.ssh/zweili.key

      Host cache.zweili.org
        IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key
        Port 2222
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
