{ config, ... }:
{
  # The package and its .desktop launcher moved to the NixOS module
  # modules/programs/rapid-photo-downloader. Only the mutable, out-of-store
  # config symlink stays here: a system activation would create a root-owned
  # file in the user's home, which is worse than mkOutOfStoreSymlink.
  home.file.".config/Rapid Photo Downloader/Rapid Photo Downloader.conf".source =
    config.lib.file.mkOutOfStoreSymlink "/home/andreas/.nixos/modules/home-manager/programs/rapid-photo-downloader/config/Rapid Photo Downloader.conf";
}
