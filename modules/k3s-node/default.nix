{ inputs, ... }:
{
  services.k3s = {
    enable = true;
    role = "agent";
    serverAddr = "https://10.7.89.130:6443";
    tokenFile = "${inputs.self}/secrets/passwords/k3s_token.key";
  };
}
