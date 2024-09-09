album_directories=$(mktemp)
all_files_in_target=$(mktemp)
playlist="$(realpath "$1")"
source="/run/user/1000/gvfs/smb-share:server=10.7.89.108,share=media/"
target="$2/synced"
files_to_sync=$(mktemp)

# stop the whole script on Ctrl-C
trap "exit" INT

sync-files-to-target() {
    echo "Sync files"
    cd "$source"
    rsync \
        --archive \
        --relative \
        --info=NAME1 \
        --files-from="$files_to_sync" \
        . \
        "$target"
}

create-list-of-album-directories() {
    echo "Create list of album directories"
    trap 'rm -f "$album_directories"' EXIT
    while IFS="" read -r line || [ -n "$line" ]; do
        dirname "$line" >>"$album_directories"
    done <"$files_to_sync"

    sort -o "$album_directories"{,}
    # Remove duplicate entries
    awk -i inplace '!seen[$0]++' "$album_directories"
}

append-cover-art-paths() {
    echo "Append cover art paths"
    cd "$source"
    while IFS="" read -r line || [ -n "$line" ]; do
        for file in "$line"/cover.*; do
            if [ -f "$file" ]; then
                ls "$file" >>"$files_to_sync"
            fi
        done
    done <"$album_directories"

    sort -o "$files_to_sync"{,}
    # remove duplicate entries
    awk -i inplace '!seen[$0]++' "$files_to_sync"
}

convert-playlist-to-syncable-paths() {
    echo "Convert playlist"
    trap 'rm -f "$files_to_sync"' EXIT
    awk '!/^#/ && NF {print "./" substr($0, length("'"$source"'") + 1)}' "$playlist" >"$files_to_sync"
}

find-all-files-in-target() {
    # Look for all files in target directory and write them to a file.
    echo "Get all files in target"
    trap 'rm -f "$all_files_in_target"' EXIT
    cd "$target"
    find . -type f -print >"$all_files_in_target"
    sort -o "$all_files_in_target"{,}
}

remove-surplus-files() {
    # Create diff between synced playlist and all files.
    # Then remove the surplus files on the target.
    printf "Removing surplus files.\n"
    cd "$target"
    comm -23 "$all_files_in_target" "$files_to_sync" |
        while IFS="" read -r line || [ -n "$line" ]; do
            printf 'Removing: %s\n' "$line"
            rm "$line"
        done
    printf "Cleaning empty directories \n"
    find "$target" -type d -empty -delete
}

mkdir -p "$target"
convert-playlist-to-syncable-paths
create-list-of-album-directories
append-cover-art-paths
sync-files-to-target
find-all-files-in-target
remove-surplus-files
