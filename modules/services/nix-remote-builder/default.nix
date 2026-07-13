{ role }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  # ed25519 host public keys (from scrts/secrets.nix)
  gwynHostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDgU7uBGqpsp39oIotlhE5ohdFyTMGkLqOScW5ER6KAA root@gwyn";
  capricornHostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRobGMQkRNxHCjRsNGDgCivywhVylkyN71V1ikWiPhX root@capricorn";
  builderUser = "nixremote";
in
lib.mkMerge [
  # ---- gwyn: accept remote builds from capricorn ----
  (lib.mkIf (role == "server") {
    users.groups.${builderUser} = { };
    users.users.${builderUser} = {
      isSystemUser = true;
      group = builderUser;
      shell = pkgs.bashInteractive; # nix-store --serve needs a real shell over ssh
      openssh.authorizedKeys.keys = [ capricornHostKey ];
    };
    # merges with the existing [ "root" "@wheel" ] in common/default.nix
    nix.settings.trusted-users = [ builderUser ];
  })

  # ---- capricorn: offload builds to gwyn, fall back locally ----
  (lib.mkIf (role == "client") {
    nix.distributedBuilds = true;
    nix.buildMachines = [
      {
        # IP, not FQDN, to avoid a DNS lookup that stalls when the VPN is down
        hostName = config.az-hosts.gwyn.wgIp;
        sshUser = builderUser;
        sshKey = "/etc/ssh/ssh_host_ed25519_key";
        protocol = "ssh-ng";
        system = "x86_64-linux";
        maxJobs = 8;
        speedFactor = 2; # prefer gwyn over local when reachable
        supportedFeatures = [
          "nixos-test"
          "benchmark"
          "big-parallel"
          "kvm"
        ];
      }
    ];
    # gwyn pulls dependencies straight from binary caches instead of capricorn
    # uploading its whole store over the tunnel
    nix.settings.builders-use-substitutes = true;

    # pin gwyn's host key so root's non-interactive ssh doesn't prompt/fail
    programs.ssh.knownHosts.gwyn-builder = {
      hostNames = [
        config.az-hosts.gwyn.wgIp
        config.az-hosts.gwyn.wgFqdn
      ];
      publicKey = gwynHostKey;
    };
    # bound the connect attempt so an unreachable gwyn fails fast -> local fallback
    programs.ssh.extraConfig = ''
      Host ${config.az-hosts.gwyn.wgIp}
        ConnectTimeout 5
    '';
  })
]
