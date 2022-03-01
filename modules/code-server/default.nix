{ inputs, custom, pkgs, ... }:
{
  services.code-server = {
    enable = true;
    user = custom.username;
    host = "0.0.0.0";
    auth = "none";
    extraPackages = with pkgs;
      [
        coreutils
        python3
        # other
        bashInteractive
        git
      ];
    extraEnvironment = {
      HOME = "/home/${custom.username}";
      EXTENSIONS_GALLERY = ''
        { "serviceUrl": "https://marketplace.visualstudio.com/_apis/public/gallery"}'';
    };
  };
  networking.firewall.allowedTCPPorts = [ 4444 ];
}
