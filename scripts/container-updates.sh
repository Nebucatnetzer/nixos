#!/usr/bin/env bash
set -e

# Get the digest hash for the "latest" image of each image in the list.
# In addition show the latest three tags for each image.

# ../modules/services/grav/default.nix
# ../modules/services/firefly/default.nix
# ../modules/services/gitea/default.nix
# ../modules/services/heimdall/default.nix
# ../modules/services/docker-mailserver/default.nix
# ../modules/services/nextcloud/default.nix
# ../modules/services/plex/default.nix
images=("docker.io/linuxserver/grav" "lscr.io/linuxserver/plex" "lscr.io/linuxserver/heimdall" "docker.io/fireflyiii/data-importer" "docker.io/mailserver/docker-mailserver" "docker.io/gitea/gitea")

for image in "${images[@]}"; do
    digest=$(skopeo inspect "docker://$image":latest | jq -r '.Digest')
    tags_string=$(skopeo inspect "docker://$image" | jq -r .RepoTags[] | grep -v "arm" | grep -v "amd" | grep -v "bionic" | grep -v "^dev" | grep -v "rootless" | grep -v "js" | grep -v "nightly" | grep -v "latest" | grep -v "edge" | grep -v "version-61a5a1a8" | grep -v "version-3a9bdd2c" | sed 's/version-v/version-/i' | sort -ruV)
    set -o noglob
    IFS=$'\n' tags=($tags_string)
    set +o noglob
    echo "Image: $image"
    echo "Digest: $digest"
    echo "Tags:"
    for tag in "${tags[@]:0:3}"; do
        echo "$tag"
    done
    printf "\n"
    printf "\n"
done
