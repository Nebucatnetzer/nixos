{ config, ... }: {
  home = {
    file.".config/digikamrc".source =
      config.lib.file.mkOutOfStoreSymlink "/home/andreas/.nixos/modules/home-manager/programs/digikam/config/digikamrc";
    file.".config/digikam_systemrc".source =
      config.lib.file.mkOutOfStoreSymlink "/home/andreas/.nixos/modules/home-manager/programs/digikam/config/digikam_systemrc";
  };
}
