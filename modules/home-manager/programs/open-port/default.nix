{ ... }:
{
  programs = {
    bash = {
      bashrcExtra = ''
        open-port() {
          local port=$1
          sudo iptables -A INPUT -p tcp --dport $port -j ACCEPT
        }

        close-port() {
          local port=$1
          sudo iptables -D INPUT -p tcp --dport $port -j ACCEPT
        }
      '';
    };
  };
}
