{
  dataRoot,
  hostname,
}:
{
  config,
  inputs,
  lib,
  nixosConfig,
  ...
}:
{
  services.syncthing = {
    enable = true;
    settings = import "${inputs.self}/modules/services/syncthing/settings.nix" {
      azHosts = nixosConfig.az-hosts;
      homeDirectory = config.home.homeDirectory;
      inherit dataRoot hostname lib;
    };
  };
}
