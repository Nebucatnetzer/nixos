{ pkgs, username, ... }:
{
  services.code-server = {
    enable = true;
    user = username;
    host = "0.0.0.0";
    auth = "none";
    extraPackages = with pkgs;
      [
        python3
        # other
        bash
        git
      ];
  };
  networking.firewall.allowedTCPPorts = [ 4444 ];
}
