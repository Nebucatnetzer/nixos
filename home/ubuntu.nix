{config, pkgs, ...}:
{ 
  imports = [
    ./personal-desktop.nix
  ];
  programs.bash.enable = true;
  targets.genericLinux.enable = true;
}
