{
  config,
  lib,
  inputs,
  ...
}:
let
  cfg = config.services.az-binary-cache-client;
in
{
  options = {
    services.az-binary-cache-client.enable = lib.mkEnableOption "Make the host a client of my binary cache with the possibility to sign packages.";
  };

  config = lib.mkIf cfg.enable {
    age.secrets.gwynRootSshKey = {
      file = "${inputs.self}/scrts/gwyn_root_ssh_key.age";
      path = "/root/.ssh/id_ed25519";
      mode = "600";
      owner = "root";
      group = "root";
    };
    nix.settings = {
      substituters = [
        "ssh://nix-ssh@${config.services.az-binary-cache-common.server}?priority=50"
      ];
    };

    programs.ssh.extraConfig = ''
      Host cache.zweili.org
        Port 2222
    '';
    services.az-binary-cache-common.enable = true;
  };
}
