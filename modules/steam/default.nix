{ self, ... }:
let
  username = import "${self}/username.nix";
in
{
  programs.steam.enable = true;
  hardware.steam-hardware.enable = true;
  networking.firewall = {
    allowedTCPPorts = [ 27036 ];
    allowedUDPPorts = [ 27031 ];
  };
  home-manager.users.${username} = {
    home.file.".local/share/applications/steam.desktop".source = ./steam.desktop;
  };
}
