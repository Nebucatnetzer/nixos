{ inputs, ... }:
let
  server = "cache.zweili.org";
in
{
  imports = [
    (import "${inputs.self}/modules/services/binary-cache-common" { inherit server; })
  ];
  age.secrets.gwynRootSshKey = {
    file = "${inputs.self}/scrts/gwyn_root_ssh_key.age";
    path = "/root/.ssh/id_ed25519";
    mode = "600";
    owner = "root";
    group = "root";
  };
  nix.settings = {
    substituters = [
      "ssh://nix-ssh@${server}?priority=50"
    ];
  };

  programs.ssh.extraConfig = ''
    Host cache.zweili.org
      Port 2222
  '';
}
