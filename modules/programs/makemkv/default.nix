{
  config,
  pkgs,
  ...
}:
{
  # https://forum.makemkv.com/forum/viewtopic.php?t=1053
  home-manager.users.${config.az-username} = {
    home.packages = [ pkgs.makemkv ];
  };
  boot.kernelModules = [ "sg" ];
}
