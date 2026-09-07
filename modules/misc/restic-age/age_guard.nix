# Deliberately restic-free: it dates a backup from filesystem metadata only, so it still
# reports when the repository password, the rclone transport or restic itself is what
# broke. Reports for itself rather than through onFailure, so the message can name the job
# instead of being a systemctl status dump, and still exits non-zero so the unit shows up
# in systemctl --failed.
{
  coreutils,
  findutils,
  lib,
  # Hours without a recorded backup before a tier is called stale.
  maxAge,
  # Derivation and binary name. The caller makes it unique per unit.
  name,
  sendToTelegram,
  # One entry per thing that has to stay fresh:
  #   label = what the alert calls it
  #   paths = files or directories whose newest mtime dates it
  tiers,
  writeShellApplication,
}:
writeShellApplication {
  inherit name;
  runtimeInputs = [
    coreutils
    findutils
    sendToTelegram
  ];
  text = ''
    now=$(date +%s)
    stale=0

    # Newest mtime among the arguments as a whole-second epoch, empty when there is
    # nothing to date from. The || true is what keeps a path that does not exist yet from
    # aborting the run under set -o pipefail.
    newest_epoch() {
      { find "$@" -maxdepth 1 -type f -printf '%T@\n' 2>/dev/null || true; } |
        sort --numeric-sort |
        tail --lines=1 |
        cut --delimiter=. --fields=1
    }

    report() {
      tier="$1"
      epoch="$2"

      if [ -z "$epoch" ]; then
        send-to-telegram "⚠ No $tier backup on record ⚠
        Nothing was found to date it from, so it has most likely never run."
        stale=1
        return
      fi

      age=$(( (now - epoch) / 3600 ))
      if [ "$age" -gt ${toString maxAge} ]; then
        send-to-telegram "⚠ The $tier backup is stale ⚠
        The newest one is $age hours old, and the threshold is ${toString maxAge} hours."
        stale=1
      fi
    }

    ${lib.concatMapStringsSep "\n" (
      tier:
      ''report ${lib.escapeShellArg tier.label} "$(newest_epoch ${lib.escapeShellArgs tier.paths})"''
    ) tiers}

    exit "$stale"
  '';
}
