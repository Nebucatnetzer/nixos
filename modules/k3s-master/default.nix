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
  environment.variables = {
    KUBECONFIG = "/etc/rancher/k3s/k3s.yaml";
  };
  networking.firewall.allowedTCPPorts = [ 6443 ];
}
