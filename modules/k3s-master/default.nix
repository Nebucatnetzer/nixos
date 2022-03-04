{ pkgs, ... }:
{
  services.k3s = {
    enable = true;
    role = "server";
  };
  environment.systemPackages = [
    pkgs.k3s
    pkgs.kubectl
    pkgs.kubernetes-helm
  ];
  networking.firewall.allowedTCPPorts = [ 6443 ];
}
