{
  config,
  inputs,
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

  offsiteRepo = "swift:default:/";
  swiftStorage = import "${inputs.self}/modules/misc/swift-storage" config;
  restic-offsite-list = pkgs.writeShellScriptBin "restic-offsite-list" ''
    while IFS='=' read -r key value; do
        # Skip lines starting with # or empty lines
        if [[ ! $key =~ ^# && -n $key ]]; then
            export "$key=$value"
        fi
    done <${swiftStorage.envFile}
    export RESTIC_REPOSITORY="${offsiteRepo}"
    export OS_AUTH_URL="${swiftStorage.swiftAuthUrl}"
    export OS_USER_DOMAIN_NAME=default

    mkdir -p /tmp/restic &&

    ${pkgs.restic}/bin/restic --password-file ${password_file} snapshots'';

  restic-offsite-mount = pkgs.writeShellScriptBin "restic-offsite-mount" ''
    while IFS='=' read -r key value; do
        # Skip lines starting with # or empty lines
        if [[ ! $key =~ ^# && -n $key ]]; then
            export "$key=$value"
        fi
    done <${swiftStorage.envFile}
    export RESTIC_REPOSITORY="${offsiteRepo}"
    export OS_AUTH_URL="${swiftStorage.swiftAuthUrl}"
    export OS_USER_DOMAIN_NAME=default

    mkdir -p /tmp/restic &&

    ${pkgs.restic}/bin/restic --password-file ${password_file} mount /tmp/restic'';
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
