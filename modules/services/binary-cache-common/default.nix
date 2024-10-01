{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-binary-cache-common;
  signAllPackages = pkgs.writeShellScriptBin "sign-all-packages" ''
    ${pkgs.sudo}/bin/sudo ${pkgs.nix}/bin/nix store sign --extra-experimental-features nix-command --all --key-file ${config.age.secrets.signingKey.path}
  '';
  uploadToCache = pkgs.writeShellScriptBin "upload-to-cache" ''
    ${pkgs.netcat}/bin/nc -vzw 2 ${config.services.az-binary-cache-common.server} 22 &&
      ${pkgs.nix}/bin/nix-copy-closure --to andreas@${config.services.az-binary-cache-common.server} $1
  '';
in
{
  options = {
    services.az-binary-cache-common.enable = lib.mkEnableOption "Enable common options for my binary cache.";
    services.az-binary-cache-common.server = lib.mkOption {
      type = lib.types.str;
      description = "The FQDN under which the binary cache is reachable.";
    };
  };

  config = lib.mkIf cfg.enable {
    age.secrets.signingKey = {
      file = "${inputs.self}/scrts/signing.key.age";
      mode = "600";
      owner = "root";
      group = "root";
    };
    environment.systemPackages = [
      signAllPackages
      uploadToCache
    ];
    nix.settings = {
      secret-key-files = config.age.secrets.signingKey.path;
    };
    services.az-binary-cache-common.server = "10.7.89.150";
  };
}
