{ config, inputs, lib, pkgs, ... }:
let
  cfg = config.programs.az-restic-management;
  password_file = config.age.secrets.resticKey.path;
  repository = "rest:http://10.7.89.30:8000";

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

  infomaniak-env = config.age.secrets.infomaniakEnv.path;
  infomaniak-repo = "swift:default:/";
  infomaniak-auth-url = "https://swiss-backup02.infomaniak.com/identity/v3";

  restic-infomaniak-list = pkgs.writeShellScriptBin "restic-infomaniak-list" ''
    export $(${pkgs.gnugrep}/bin/grep -v '^#' ${infomaniak-env} | ${pkgs.findutils}/bin/xargs)
    export RESTIC_REPOSITORY="${infomaniak-repo}"
    export OS_AUTH_URL="${infomaniak-auth-url}"
    export OS_USER_DOMAIN_NAME=default

    mkdir -p /tmp/restic &&

    ${pkgs.restic}/bin/restic --password-file ${password_file} snapshots'';

  restic-infomaniak-mount = pkgs.writeShellScriptBin "restic-infomaniak-mount" ''
    export $(${pkgs.gnugrep}/bin/grep -v '^#' ${infomaniak-env} | ${pkgs.findutils}/bin/xargs)
    export RESTIC_REPOSITORY="${infomaniak-repo}"
    export OS_AUTH_URL="${infomaniak-auth-url}"
    export OS_USER_DOMAIN_NAME=default

    mkdir -p /tmp/restic &&

    ${pkgs.restic}/bin/restic --password-file ${password_file} mount /tmp/restic'';
in
{
  options = {
    programs.az-restic-management.enable = lib.mkEnableOption "Enable restic management commands.";
  };
  config = lib.mkIf cfg.enable {
    age.secrets.infomaniakEnv = {
      file = "${inputs.self}/scrts/infomaniak_env.age";
      mode = "600";
      owner = config.az-username;
      group = "users";
    };
    age.secrets.resticKey = {
      file = "${inputs.self}/scrts/restic.key.age";
      mode = "600";
      owner = config.az-username;
      group = "users";
    };
    environment.shellAliases = {
      restic-list = ''
        ${pkgs.restic}/bin/restic \
          --repo ${repository} \
          --password-file ${password_file} \
          snapshots --host ${config.networking.hostName}'';
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
    environment.systemPackages = with pkgs;
      [
        restic
        restic-mount
        restic-mount-all
        restic-infomaniak-list
        restic-infomaniak-mount
      ];
  };
}
