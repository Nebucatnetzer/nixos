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
  ...
}:
let
  secretFile =
    if keyFile != null then keyFile else "${config.networking.hostName}_storagebox.key.age";
in
{
  # Declared globally in ./options.nix, so that consumers can read it without caring
  # whether this module was instantiated on the host.
  az-storage-box = import ./restic.nix { inherit host path; };

  # 0400, because ssh ignores a private key that is accessible by others. Owned by the
  # main user, who runs the interactive restic-offsite-* helpers; root reads it anyway,
  # which is why the offsite units run as root rather than as restic.
  age.secrets.storageBoxKey = {
    file = "${inputs.self}/scrts/${secretFile}";
    mode = "400";
    owner = config.az-username;
  };

  programs.ssh.knownHosts.storage-box = {
    hostNames = [ "[${host}]:${toString port}" ];
    publicKey = hostPublicKey;
  };

  programs.ssh.extraConfig = ''
    Host ${host}
      Port ${toString port}
      User ${user}
      IdentityFile ${config.age.secrets.storageBoxKey.path}
      IdentitiesOnly yes
  '';
}
