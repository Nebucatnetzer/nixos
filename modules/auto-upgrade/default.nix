{ custom }: { ... }:
{
  system.autoUpgrade = {
    enable = true;
    dates = "daily";
    flake = "/home/${custom.username}/.nixos";
    flags = [
      "--update-input"
      "nixpkgs"
      "--update-input"
      "nixpkgs-unstable"
      "--update-input"
      "nixos-hardware"
      "--update-input"
      "agenix"
      "--update-input"
      "home-manager"
      "--commit-lock-file"
    ];
  };
  systemd.services.nixos-upgrade.after = [ "network-online.target" ];
}
