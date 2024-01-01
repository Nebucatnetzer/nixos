{ config, lib, ... }:
let cfg = config.programs.az-ssh;
in {
  options = { programs.az-ssh.enable = lib.mkEnableOption "Enable SSH host."; };

  config = lib.mkIf cfg.enable {
    programs.ssh = {
      enable = true;
      extraConfig = ''
        Host nixos.2li.local
          StrictHostKeyChecking no
          UserKnownHostsFile /dev/null
          User nixos
          LogLevel QUIET

        Host *.2li.local
          User andreas
          IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key

        Host 10.7.89.*
          User andreas
          IdentityFile ~/.nixos/secrets/ssh_keys/ansible/ansible.key

        Host 10.49.0.100
          User zweili
          IdentityFile ~/.ssh/zweili.key
      '';
    };
  };
}
