{ pkgs, lib, ... }:
{
  imports = [ "${fetchTarball "https://github.com/NixOS/nixos-hardware/archive/32f61571b486efc987baca553fb35df22532ba63.tar.gz" }/raspberry-pi/4" ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  environment.systemPackages = with pkgs; [
    raspberrypi-eeprom
  ];

  system.stateVersion = "22.05";
  services.openssh.enable = true;
  networking.hostName = "nixos";

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
      warn-dirty = false
    '';
    trustedUsers = [ "root" "@wheel" ];
  };

  users = {
    mutableUsers = false;
    users."nixos" = {
      isNormalUser = true;
      initialPassword = "password";
      extraGroups = [ "wheel" ];
    };
  };
}
