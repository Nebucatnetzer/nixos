{
  server ? "cache.zweili.org",
}:
{
  config,
  inputs,
  pkgs,
  ...
}:
let
  signAllPackages = pkgs.writeShellScriptBin "sign-all-packages" ''
    ${pkgs.sudo}/bin/sudo ${pkgs.nix}/bin/nix store sign --extra-experimental-features nix-command --all --key-file ${config.age.secrets.signingKey.path}
  '';
  uploadToCache = pkgs.writeShellScriptBin "upload-to-cache" ''
    ${pkgs.netcat}/bin/nc -vzw 2 ${server} 2222 &&
      ${pkgs.nix}/bin/nix-copy-closure --to andreas@${server} $1
  '';
in
{
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
}
