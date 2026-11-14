{ config, ... }:
{
  # darktable rewrites darktablerc on exit. It writes through the symlink.
  home.file.".config/darktable/darktablerc".source =
    config.lib.file.mkOutOfStoreSymlink "/home/andreas/.nixos/modules/home-manager/programs/darktable/config/darktablerc";
  # darktable lists soft proof profiles only from color/out.
  home.file.".config/darktable/color/out/SaalDigital_PhotoPrints_matte.icc".source =
    config.lib.file.mkOutOfStoreSymlink "/home/andreas/data/98_applications/photo_profiles/saal_digital_icc/SaalDigital_PhotoPrints_matte.icc";
  # darktable runs luarc on every start. Import new files from the workbench.
  home.file.".config/darktable/luarc".text = ''
    local dt = require "darktable"
    dt.database.import(os.getenv("HOME") .. "/data/20_pictures/0_workbench")
  '';
}
