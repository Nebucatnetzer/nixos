# A 50 GiB ext4 image on the root btrfs for downloaded videos.
# It is created on first use and automounted on access.
{
  config,
  inputs,
  pkgs,
  ...
}:
let
  mediaPaths = import "${inputs.self}/pkgs/mediaPaths.nix";
  imageDirectory = "/var/lib/video-image";
  imageFile = "${imageDirectory}/videos.img";
  imageSize = "50G";
  username = config.az-username;
  userGroup = config.users.users.${username}.group;
in
{
  systemd.services.video-image-create = {
    description = "Create the ext4 image for videos";
    unitConfig.ConditionPathExists = "!${imageFile}";
    path = [
      pkgs.coreutils
      pkgs.e2fsprogs
      pkgs.util-linux
    ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      StateDirectory = "video-image";
      StateDirectoryMode = "0700";
    };
    # Build under a temporary name, so a failed run does not leave a broken image behind.
    script = ''
      partial_image="${imageFile}.partial"
      seed_directory=$(mktemp --directory)

      rm --force "$partial_image"
      touch "$partial_image"
      # btrfs only honours nodatacow on an empty file.
      chattr +C "$partial_image"
      fallocate --length ${imageSize} "$partial_image"

      # mkfs copies owner and mode from the seed directory into the image.
      install --directory --owner=${username} --group=${userGroup} "$seed_directory/Videos"
      # -L the filesystem label
      # -m 0 disable the block reservation for root
      # -d sets the seed directory
      mkfs.ext4 -L videos -m 0 -d "$seed_directory" "$partial_image"

      rm --recursive "$seed_directory"
      mv "$partial_image" "${imageFile}"
    '';
  };

  fileSystems."${mediaPaths.videoImage}" = {
    device = imageFile;
    fsType = "ext4";
    # systemd-fsck only checks block devices, not image files.
    noCheck = true;
    options = [
      "loop"
      "noatime"
      "noauto"
      "nofail"
      "x-systemd.automount"
      "x-systemd.requires=video-image-create.service"
    ];
  };
  home-manager.users.${username} =
    { config, ... }:
    {
      home.file."Videos".source = config.lib.file.mkOutOfStoreSymlink mediaPaths.youtubeVideos;
    };

}
