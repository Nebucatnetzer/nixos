{ pkgs, username, ... }:
{
  services.code-server = {
    enable = true;
    user = username;
    host = "0.0.0.0";
    auth = "none";
    extraPackages = with pkgs;
      [
        coreutils
        python3
        # other
        bash
        git
      ];
    extraEnvironment = {
      HOME = "/home/${username}";
    };
  };
  networking.firewall.allowedTCPPorts = [ 4444 ];
}
