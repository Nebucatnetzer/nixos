{ inputs, ... }:
{
  imports = [ "${inputs.self}/modules/home-manager" ];
  programs.carapace = {
    enable = true;
    enableBashIntegration = true;
  };
  systemd.user.startServices = "sd-switch";
}
