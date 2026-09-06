# Value provider consumed through the az-storage-box option. Port, user and identity
# file come from the ssh Host block that default.nix writes, so they are not repeated
# here.
{
  host,
  path,
}:
let
  # Both options are required: restic appends its own default rclone.args to whatever
  # program is set, so a program that already ends in "serve restic --stdio" would send
  # that phrase to the box twice.
  transport = serveArgs: [
    ''-o rclone.program="ssh ${host} rclone"''
    ''-o rclone.args="serve restic --stdio${serveArgs}"''
  ];
in
{
  repository = "rclone:${path}";

  # Append-only by default. rclone's serve restic refuses every DELETE whose path is not
  # under locks/, so copy, check, restore and mount all work while nothing on a client
  # can remove a snapshot or a pack. That is the point: capricorn and fenoglio hold the
  # restic key, so the offsite copy has to survive a mistake or a compromise on either.
  extraResticArgs = transport " --append-only";

  # forget and prune get a 403 through the transport above, so they need this one.
  # Deliberately not wired into any unit: it is run by hand, rarely.
  pruneResticArgs = transport "";
}
