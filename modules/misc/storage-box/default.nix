{
  host,
  hostPublicKey,
  # Defaults to <hostname>_storagebox.key.age, resolved in the body because an
  # imports entry cannot depend on config.
  keyFile ? null,
  path,
  port ? 23,
  user,
}:
{
  config,
  inputs,
  lib,
  ...
}:
let
  secretFile =
    if keyFile != null then keyFile else "${config.networking.hostName}_storagebox.key.age";
  # Only the host running the rest server has this user.
  hasResticUser = config.users.users ? "restic";
in
{
  # Declared globally in ./options.nix, so that consumers can read it without caring
  # whether this module was instantiated on the host.
  az-storage-box = import ./restic.nix { inherit host path; };

  # 0400, because ssh ignores a private key that is accessible by others. Owned by the
  # main user, who runs the interactive restic-offsite-* helpers.
  age.secrets.storageBoxKey = {
    file = "${inputs.self}/scrts/${secretFile}";
    mode = "400";
    owner = config.az-username;
  };

  # The same key again for the offsite units, which run as restic so that nothing but
  # the repository's own uid writes into the local repository. Two decrypted files
  # rather than one shared 0440, because ssh refuses a group readable key.
  age.secrets.storageBoxKeyRestic = lib.mkIf hasResticUser {
    file = "${inputs.self}/scrts/${secretFile}";
    mode = "400";
    owner = "restic";
  };

  programs.ssh.knownHosts.storage-box = {
    hostNames = [ "[${host}]:${toString port}" ];
    publicKey = hostPublicKey;
  };

  # IdentityFile accumulates across matching blocks instead of being overridden, so it
  # is set once per identity in a Match block and never in the Host block. localuser is
  # the local side criterion; plain user matches the remote login name.
  programs.ssh.extraConfig = ''
    Host ${host}
      Port ${toString port}
      User ${user}
      IdentitiesOnly yes

    Match host ${host} !localuser restic
      IdentityFile ${config.age.secrets.storageBoxKey.path}
  ''
  + lib.optionalString hasResticUser ''

    Match host ${host} localuser restic
      IdentityFile ${config.age.secrets.storageBoxKeyRestic.path}
  '';
}
