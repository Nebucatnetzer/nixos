all_files_in_target=$(mktemp)
playlist="$(realpath "$1")"
source="/run/user/1000/gvfs/smb-share:server=10.7.89.108,share=media/"
target="$2/synced"
synced_files=$(mktemp)

# stop the whole script on Ctrl-C
trap "exit" INT

sync-files-to-target() {
    # Exlude all lines starting with #
    # Read each line of the given playlist file.
    # Cut the $source directory from the $file, this is required so that rsync
    # doesn't create the full /run/user/etc. path with the --relative option.
    # Finally sync the given file with rsync to the target and recreate the
    # directory structure.

    cd "$source"
    grep -v '^#' <"$playlist" |
        while IFS="" read -r line || [ -n "$line" ]; do
            file=${line##$source}
            rsync \
                --archive \
                --relative \
                --info=NAME1 \
                "$file" \
                "$target" || break
        done
}

create-synced-files-list() {
    trap 'rm -f "$synced_files"' EXIT
    awk '!/^#/ && NF {print "./" substr($0, length("'"$source"'") + 1)}' "$playlist" >"$synced_files"
    sort -o "$synced_files"{,}
}

find-all-files-in-target() {
    # Look for all files in target directory and write them to a file.
    trap 'rm -f "$all_files_in_target"' EXIT
    cd "$target"
    find . -type f -print >"$all_files_in_target"
    sort -o "$all_files_in_target"{,}
}

remove-surplus-files() {
    # Create diff between synced playlist and all files.
    # Then remove the surplus files on the target.
    printf "Removing surplus files."
    cd "$target"
    comm -23 "$all_files_in_target" "$synced_files" |
        while IFS="" read -r line || [ -n "$line" ]; do
            printf 'Removing: %s\n' "$line"
            rm "$line"
        done
    printf "Cleaning empty directories \n"
    find "$target" -type d -empty -delete
}

mkdir -p "$target"
sync-files-to-target
create-synced-files-list
find-all-files-in-target
remove-surplus-files
