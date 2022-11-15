{ custom }: { ... }:
{
  system.autoUpgrade = {
    enable = true;
    dates = "10:20";
    flake = "/home/${custom.username}/.nixos";
    flags = [
      "--update-input"
      "nixpkgs"
      "--commit-lock-file"
    ];
  };
}
