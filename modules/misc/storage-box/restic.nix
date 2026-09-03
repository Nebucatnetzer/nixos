# Value provider, imported like modules/misc/swift-storage. Port, user and identity
# file come from the ssh Host block that default.nix writes, so they are not repeated
# here.
{
  host,
  path,
}:
{
  repository = "rclone:${path}";
  # Both options are required: restic appends its own default rclone.args to whatever
  # program is set, so a program that already ends in "serve restic --stdio" would
  # send that phrase to the box twice.
  extraResticArgs = [
    ''-o rclone.program="ssh ${host} rclone"''
    ''-o rclone.args="serve restic --stdio"''
  ];
}
