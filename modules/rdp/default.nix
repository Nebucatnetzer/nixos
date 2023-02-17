{ pkgs, ... }:
{
  services.xrdp = {
    enable = true;
    defaultWindowManager = "${pkgs.gnome.gnome-shell}/bin/gnome-shell";
  };
  networking.firewall.allowedTCPPorts = [ 3389 ];
}
