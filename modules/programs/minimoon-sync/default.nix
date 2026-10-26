{
  config,
  inputs,
  pkgs,
  unstable-pkgs,
  ...
}:
let
  azPkgs = import "${inputs.self}/pkgs" { inherit pkgs unstable-pkgs; };
  minimoonSync = pkgs.writeShellApplication {
    name = "minimoon-sync";
    runtimeInputs = [
      azPkgs.minimoon-sync-server
      config.networking.firewall.package
      pkgs.util-linux # setpriv
    ];
    text = ''
      if [ "$EUID" -ne 0 ] || [ -z "''${SUDO_UID:-}" ] || [ -z "''${SUDO_GID:-}" ]; then
        echo "Run this with sudo from your normal user, for example: sudo minimoon-sync" >&2
        exit 1
      fi

      # Use the music directory unless an argument without a leading "-" names another one.
      directory_given=false
      for argument in "$@"; do
        if [[ "$argument" != -* ]]; then
          directory_given=true
        fi
      done
      if [ "$directory_given" = false ]; then
        set -- "$@" "/home/${config.az-username}/Music"
      fi

      # Allow only the iPhone (groeibschi), and only through WireGuard.
      rule=(nixos-fw --in-interface wg0 --source ${config.az-hosts.groeibschi.wgIp} --protocol tcp --destination-port 41324 --jump nixos-fw-accept)

      close_port() {
        iptables --delete "''${rule[@]}"
        echo "Closed port 41324 for groeibschi"
      }

      if ! iptables --check "''${rule[@]}" 2>/dev/null; then
        iptables --insert "''${rule[@]}"
      fi
      trap close_port EXIT
      echo "Opened port 41324 for groeibschi"
      echo "Enter http://${config.az-hosts.capricorn.wgIp}:41324 in MiniMoon, not the LAN address below."

      # Serve the files as the calling user, not as root.
      setpriv --reuid="$SUDO_UID" --regid="$SUDO_GID" --init-groups \
        minimoon-sync-server "$@"
    '';
  };
in
{
  environment.systemPackages = [
    minimoonSync
  ];
}
