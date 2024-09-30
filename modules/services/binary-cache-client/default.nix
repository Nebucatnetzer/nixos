{
  config,
  lib,
  inputs,
  pkgs,
  ...
}:
let
  cachingServer = "management.2li.local";
  cfg = config.services.az-binary-cache-client;
  sign-all-packages = pkgs.writeShellScriptBin "sign-all-packages" ''
    ${pkgs.sudo}/bin/sudo ${pkgs.nix}/bin/nix store sign --extra-experimental-features nix-command --all --key-file ${config.age.secrets.signingKey.path}
  '';
  upload-to-cache = pkgs.writeShellScriptBin "upload-to-cache" ''
    until ${pkgs.netcat}/bin/nc -vzw 2 $1 22; do
        sleep 1
    done &&
        nix-copy-closure --to andreas@${cachingServer} $1
  '';
in
{
  options = {
    services.az-binary-cache-client.enable = lib.mkEnableOption "Make the host a client of my binary cache with the possibility to sign packages.";
  };

  config = lib.mkIf cfg.enable {
    age.secrets.gwynRootSshKey = {
      file = "${inputs.self}/scrts/gwyn_root_ssh_key.age";
      path = "/root/.ssh/id_ed25519";
      mode = "600";
      owner = "root";
      group = "root";
    };
    age.secrets.signingKey = {
      file = "${inputs.self}/scrts/signing.key.age";
      mode = "600";
      owner = "root";
      group = "root";
    };
    environment.systemPackages = [
      sign-all-packages
      upload-to-cache
    ];
    nix.settings = {
      substituters = [
        "ssh://nix-ssh@${cachingServer}?priority=50"
      ];
      secret-key-files = config.age.secrets.signingKey.path;
    };
  };
}
