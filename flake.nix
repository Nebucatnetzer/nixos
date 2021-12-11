{
  description = "Andreas Zweili's Nixos configuration";

  inputs = {
    system-version = import ../version.nix;
    nixpkgs.url = "github:nixos/nixpkgs/nixos-" + system-version;
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-" + system-version;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  flake-utils.url = "github:numtide/flake-utils";


  outputs = { self, nixpkgs }: { };
}
