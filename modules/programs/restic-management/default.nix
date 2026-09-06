{
  config,
  lib,
  pkgs,
  ...
}:
let
  password_file = config.age.secrets.resticKey.path;
  repository = "rest:http://${config.az-hosts.gwyn.wgIp}:8123";

  restic-mount = pkgs.writeShellScriptBin "restic-mount" ''
    mkdir -p /tmp/restic &&
    ${pkgs.restic}/bin/restic \
      --repo ${repository} \
      --password-file ${password_file} \
      --host ${config.networking.hostName} \
      mount /tmp/restic'';

  restic-mount-all = pkgs.writeShellScriptBin "restic-mount-all" ''
    mkdir -p /tmp/restic &&
    ${pkgs.restic}/bin/restic \
      --repo ${repository} \
      --password-file ${password_file} \
      mount /tmp/restic'';

  # No guard on az-storage-box: profiles/management imports the Storage Box module
  # alongside this one, so every host with these helpers can reach the box.
  # The transport is append-only, which is enough for list, mount and restore.
  offsiteRepository = config.az-storage-box.repository;
  offsiteRestic = lib.concatStringsSep " " (
    [ "${pkgs.restic}/bin/restic" ] ++ config.az-storage-box.extraResticArgs
  );

  restic-offsite-list = pkgs.writeShellScriptBin "restic-offsite-list" ''
    ${offsiteRestic} \
      --repo ${offsiteRepository} \
      --password-file ${password_file} \
      snapshots
  '';

  restic-offsite-mount = pkgs.writeShellScriptBin "restic-offsite-mount" ''
    mkdir -p /tmp/restic &&
    ${offsiteRestic} \
      --repo ${offsiteRepository} \
      --password-file ${password_file} \
      mount /tmp/restic
  '';
in
{
  environment.shellAliases = {
    restic-list = ''
      ${pkgs.restic}/bin/restic \
        --repo ${repository} \
        --password-file ${password_file} \
        snapshots --host ${config.networking.hostName}'';
    restic-list-all = ''
      ${pkgs.restic}/bin/restic \
        --repo ${repository} \
        --password-file ${password_file} snapshots'';
    restic-unlock = ''
      ${pkgs.restic}/bin/restic \
        --repo ${repository} \
        --password-file ${password_file} \
        unlock'';
    restic-forget = ''
      ${pkgs.restic}/bin/restic --repo ${repository} \
        --password-file ${password_file} \
        forget $1'';
  };
  environment.systemPackages = [
    pkgs.restic
    restic-mount
    restic-mount-all
    restic-offsite-list
    restic-offsite-mount
  ];
}
