{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-binary-cache-server;
  GiB = 1024 * 1024 * 1024;
  sign-all-packages = pkgs.writeShellScriptBin "sign-all-packages" ''
    ${pkgs.sudo}/bin/sudo ${pkgs.nix}/bin/nix store sign --extra-experimental-features nix-command --all --key-file ${config.age.secrets.signingKey.path}
  '';
  upload-to-cache = pkgs.writeShellScriptBin "upload-to-cache" ''
    until ${pkgs.netcat}/bin/nc -vzw 2 $1 22; do
  '';
in
{
  options = {
    services.az-binary-cache-server.enable = lib.mkEnableOption "Setup a binary cache serving via SSH.";
  };

  config = lib.mkIf cfg.enable {
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
    nix = {
      gc = {
        automatic = lib.mkForce false;
      };
      settings = {
        min-free = lib.mkForce (300 * GiB);
        max-free = lib.mkForce (512 * GiB);
        secret-key-files = config.age.secrets.signingKey.path;
      };
      sshServe = {
        enable = true;
        keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCR+JXNHSAEQamn2QiaKV0vejCPy6OmzOePXoaQF6CEknXyvBO4j7+qpgZ5RAhe7ups8xZrEpBKdtxRMf7OdQQEXg1PLlfWZSJTC8EGu1TbMltbwwHizgsK/15LkDhJ0Gk/GFz9O9GvGqjizik8Kvvqz8XWY0tEtYs5Riq8bB5D5Ctwl10iultqnIQkdaX0bNa/2X57XKeutWdbqhuSC/C7awC1aVDIdfy1BNT3weHhQhFVAeAlH7Fy4rx3gYPclICfzu27lulLeXKJj9F+NdeY84zEy7E8IkE7eqdo1zfdJJpXSIh3FqekWen5njzWJsXqZCa2Ynk1poK/Rv/ti+ySE+4XicyXp0VJM8fDz6iUI0S/pjumHwzpoN9CeNe5PDK3Y7iQzSlO9REvkj/+v7r2s6XKslk9B7hTKunvH5JgHlIeYymzXb4r2LggNrP/1KUgNk1Ztu+s1c5onXYfBNul1iQOFU3+kgTk8Oh/UFK3FA0dYeWrOLA02TdH2S7U6yE= andreas@gwyn
"
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDxBun0BYQaz/WjiP+g5+Hs9/JZnWAuLTpTVYgx/9shJwKS5Zu9K3I115DYOro/lpu0AMeeJca5We2AICcxYcM0lIZvsJqfOnFOHFjgmHxHc6IuzrUPM7msoLneF5lxfJ8ko2/LcFq8EtGlzjkllRpFpp2FtxDviD1lr4mJda4cOuQES4ujH3HP5Shpwa96oqnDENWCL+XPFe+Ur+88tuKTQ2MIX5Iqhs2sMIwsMI1o8HjBi4sMd+kd7qb232XcwWTlP3iIWvq/0D3OxZ6J6uSNyC4UCl781lupLOKrC6ml58RUrYP8nrF0a53+i0hgLuDiCWhj0vkY7W9nJW1no425 andreas@python"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGAw9i7NfKt9C2v4Uc4gucNKy5Ru4HZD39aSpS0zLOUq andreas@management"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG2m+S9zhlgRKdHCcNf+WJA0bdd+Xoj6LatGflYnWmQv gwyn_root"
        ];
      };
    };
  };
}
