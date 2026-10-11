{ config, ... }: {
  home = {
    file.".config/ART".source =
      config.lib.file.mkOutOfStoreSymlink "/home/andreas/.nixos/modules/home-manager/programs/art/config/ART";
  };
}
