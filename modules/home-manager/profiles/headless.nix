{ inputs, ... }:
{
  imports = [ "${inputs.self}/modules/home-manager" ];
  systemd.user.startServices = "sd-switch";
  nix = {
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };
  systemd.user.timers.nix-gc = {
    Timer = {
      Persistent = true;
    };
  };
}
