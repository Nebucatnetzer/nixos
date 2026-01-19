{
  envFile,
  localResticRepo,
  rclone,
  swiftAuthUrl,
  swiftRegion,
  writeShellApplication,
}:
writeShellApplication {
  name = "restic-offsite-sync";
  runtimeInputs = [
    rclone
  ];
  text = ''
    while IFS='=' read -r key value; do
        # Skip lines starting with # or empty lines
        if [[ ! $key =~ ^# && -n $key ]]; then
            export "$key=$value"
        fi
    done <${envFile}
    export OS_AUTH_URL="${swiftAuthUrl}"
    export OS_REGION_NAME="${swiftRegion}"
    export OS_USER_DOMAIN_NAME=default

    mkdir -p /tmp/restic &&

    ${rclone}/bin/rclone \
      --no-gzip-encoding \
      --config ${./rclone.conf} \
      -v \
      sync \
      --delete-before \
      ${localResticRepo} \
      offsite:default/
  '';
}
