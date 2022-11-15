{ custom }: { ... }:
{
  system.autoUpgrade = {
    enable = true;
    dates = "daily";
    flake = "/home/${custom.username}/.nixos";
    flags = [
      "--update-input"
      "nixpkgs"
      "--commit-lock-file"
    ];
  };
}
