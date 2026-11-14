{
  darktable,
  procps,
  sqlite,
  writeShellApplication,
}:
writeShellApplication {
  name = "darktable-purge-missing";
  # purge_non_existing_images.sh calls sqlite3 and pgrep.
  runtimeInputs = [
    procps
    sqlite
  ];
  text = ''
    library="$HOME/.config/darktable/library.db"
    workbench="$HOME/data/20_pictures/0_workbench"
    lock_file="$library.lock"

    if [[ ! -f "$library" ]]; then
        echo "darktable-purge-missing: $library does not exist, skipping" >&2
        exit 0
    fi
    # Without the workbench, every image counts as missing and the purge empties the library.
    if [[ ! -d "$workbench" ]]; then
        echo "darktable-purge-missing: $workbench does not exist, refusing to purge" >&2
        exit 1
    fi
    # The pgrep check in the darktable script misses darktable, because the Nix wrapper renames the process.
    if [[ -f "$lock_file" ]] && kill -0 "$(cat "$lock_file")" 2>/dev/null; then
        echo "darktable-purge-missing: darktable is running, skipping" >&2
        exit 0
    fi

    ${darktable}/share/darktable/tools/purge_non_existing_images.sh --purge
  '';
}
