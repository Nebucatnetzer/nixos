{
  config,
  inputs,
  pkgs,
  ...
}:
let
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

  infomaniakRepo = "swift:default:/";
  swiftStorage = import "${inputs.self}/modules/misc/swift-storage" config;

  offsite-repo-check = pkgs.callPackage ./offsite_repo_check.nix {
    envFile = swiftStorage.envFile;
    resticPassword = password_file;
    resticRepo = infomaniakRepo;
    swiftAuthUrl = swiftStorage.swiftAuthUrl;
  };

  restic-infomaniak-list = pkgs.writeShellScriptBin "restic-infomaniak-list" ''
    while IFS='=' read -r key value; do
        # Skip lines starting with # or empty lines
        if [[ ! $key =~ ^# && -n $key ]]; then
            export "$key=$value"
        fi
    done <${swiftStorage.envFile}
    export RESTIC_REPOSITORY="${infomaniakRepo}"
    export OS_AUTH_URL="${swiftStorage.swiftAuthUrl}"
    export OS_USER_DOMAIN_NAME=default

    mkdir -p /tmp/restic &&

    ${pkgs.restic}/bin/restic --password-file ${password_file} snapshots'';

  restic-infomaniak-mount = pkgs.writeShellScriptBin "restic-infomaniak-mount" ''
    while IFS='=' read -r key value; do
        # Skip lines starting with # or empty lines
        if [[ ! $key =~ ^# && -n $key ]]; then
            export "$key=$value"
        fi
    done <${swiftStorage.envFile}
    export RESTIC_REPOSITORY="${infomaniakRepo}"
    export OS_AUTH_URL="${swiftStorage.swiftAuthUrl}"
    export OS_USER_DOMAIN_NAME=default

    mkdir -p /tmp/restic &&

    ${pkgs.restic}/bin/restic --password-file ${password_file} mount /tmp/restic'';
in
{
  age.secrets.infomaniakEnv = {
    file = "${inputs.self}/scrts/infomaniak_env.age";
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
    offsite-repo-check
    restic-mount
    restic-mount-all
    restic-infomaniak-list
    restic-infomaniak-mount
  ];
}
