{ inputs }: { pkgs, ... }:
{
  programs.nix-ld.enable = true;
  environment.systemPackages = with pkgs; [
    nix-alien
    nix-index
    nix-index-update
  ];
}
