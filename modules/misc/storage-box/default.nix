{
  host,
  hostPublicKey,
  keyFile,
  port ? 23,
  user,
}:
{
  config,
  inputs,
  ...
}:
{
  age.secrets.storageBoxKey.file = "${inputs.self}/scrts/${keyFile}";

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
