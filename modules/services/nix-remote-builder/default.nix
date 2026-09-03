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
  fenoglioHostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOonkLlbxOoCS/8s2BOhctEiFQfOrZrGMdYGY2Y69lbW root@fenoglio";
  builderUser = "nixremote";
in
lib.mkMerge [
  # ---- fenoglio: accept remote builds from capricorn and gwyn ----
  (lib.mkIf (role == "server") {
    users.groups.${builderUser} = { };
    users.users.${builderUser} = {
      isSystemUser = true;
      group = builderUser;
      shell = pkgs.bashInteractive; # nix-store --serve needs a real shell over ssh
      openssh.authorizedKeys.keys = [
        capricornHostKey
        gwynHostKey
      ];
    };
    # merges with the existing [ "root" "@wheel" ] in common/default.nix
    nix.settings.trusted-users = [ builderUser ];
  })

  # ---- clients (capricorn, gwyn): offload builds to fenoglio, fall back locally ----
  (lib.mkIf (role == "client") {
    # Off by default: fenoglio is an on-demand builder, not a service host, so no build
    # should assume it is up. /etc/nix/machines is still written, because buildMachines
    # and distributedBuilds are independent, and the rebuild wrapper opts in per
    # invocation with --builders @/etc/nix/machines once it has probed fenoglio.
    nix.distributedBuilds = false;
    nix.buildMachines = [
      {
        # IP, not FQDN, to avoid a DNS lookup that stalls when the VPN is down
        hostName = config.az-hosts.fenoglio.wgIp;
        sshUser = builderUser;
        sshKey = "/etc/ssh/ssh_host_ed25519_key";
        protocol = "ssh-ng";
        system = "x86_64-linux";
        maxJobs = 8;
        speedFactor = 2; # prefer fenoglio over local when reachable
        supportedFeatures = [
          "nixos-test"
          "benchmark"
          "big-parallel"
          "kvm"
        ];
      }
    ];
    # fenoglio pulls dependencies straight from binary caches instead of the client
    # uploading its whole store over the tunnel
    nix.settings.builders-use-substitutes = true;

    # pin fenoglio's host key so root's non-interactive ssh doesn't prompt/fail
    programs.ssh.knownHosts.fenoglio-builder = {
      hostNames = [
        config.az-hosts.fenoglio.wgIp
        config.az-hosts.fenoglio.wgFqdn
      ];
      publicKey = fenoglioHostKey;
    };
    # bound the connect attempt so an unreachable fenoglio fails fast -> local fallback
    programs.ssh.extraConfig = ''
      Host ${config.az-hosts.fenoglio.wgIp}
        ConnectTimeout 5
    '';
  })
]
