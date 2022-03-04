{ inputs, ... }:
{
  services.k3s = {
    enable = true;
    role = "agent";
    serverAddr = "https://10.7.89.130:6443";
    tokenFile = "/etc/k3s/token.key";
  };
}
