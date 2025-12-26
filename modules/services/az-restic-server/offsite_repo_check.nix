{
  envFile,
  resticPassword,
  resticRepo,
  restic,
  swiftAuthUrl,
  writeShellApplication,
}:
writeShellApplication {
  name = "restic-offsite-check";
  runtimeInputs = [
    restic
  ];
  text = ''
    while IFS='=' read -r key value; do
        # Skip lines starting with # or empty lines
        if [[ ! $key =~ ^# && -n $key ]]; then
            export "$key=$value"
        fi
    done <${envFile}
    export RESTIC_REPOSITORY="${resticRepo}"
    export OS_AUTH_URL="${swiftAuthUrl}"
    export OS_USER_DOMAIN_NAME=default

    mkdir -p /tmp/restic &&

    ${restic}/bin/restic \
      --password-file ${resticPassword} \
      check
  '';
}
