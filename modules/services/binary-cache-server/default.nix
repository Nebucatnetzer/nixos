{
  config,
  lib,
  ...
}:
let
  cfg = config.services.az-binary-cache-server;
  GiB = 1024 * 1024 * 1024;
in
{
  options = {
    services.az-binary-cache-server.enable = lib.mkEnableOption "Setup a binary cache serving via SSH.";
  };

  config = lib.mkIf cfg.enable {
    nix = {
      gc = {
        automatic = lib.mkForce false;
      };
      settings = {
        min-free = lib.mkForce (300 * GiB);
        max-free = lib.mkForce (512 * GiB);
      };
      sshServe = {
        enable = true;
        keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCR+JXNHSAEQamn2QiaKV0vejCPy6OmzOePXoaQF6CEknXyvBO4j7+qpgZ5RAhe7ups8xZrEpBKdtxRMf7OdQQEXg1PLlfWZSJTC8EGu1TbMltbwwHizgsK/15LkDhJ0Gk/GFz9O9GvGqjizik8Kvvqz8XWY0tEtYs5Riq8bB5D5Ctwl10iultqnIQkdaX0bNa/2X57XKeutWdbqhuSC/C7awC1aVDIdfy1BNT3weHhQhFVAeAlH7Fy4rx3gYPclICfzu27lulLeXKJj9F+NdeY84zEy7E8IkE7eqdo1zfdJJpXSIh3FqekWen5njzWJsXqZCa2Ynk1poK/Rv/ti+ySE+4XicyXp0VJM8fDz6iUI0S/pjumHwzpoN9CeNe5PDK3Y7iQzSlO9REvkj/+v7r2s6XKslk9B7hTKunvH5JgHlIeYymzXb4r2LggNrP/1KUgNk1Ztu+s1c5onXYfBNul1iQOFU3+kgTk8Oh/UFK3FA0dYeWrOLA02TdH2S7U6yE= andreas@gwyn
"
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDxBun0BYQaz/WjiP+g5+Hs9/JZnWAuLTpTVYgx/9shJwKS5Zu9K3I115DYOro/lpu0AMeeJca5We2AICcxYcM0lIZvsJqfOnFOHFjgmHxHc6IuzrUPM7msoLneF5lxfJ8ko2/LcFq8EtGlzjkllRpFpp2FtxDviD1lr4mJda4cOuQES4ujH3HP5Shpwa96oqnDENWCL+XPFe+Ur+88tuKTQ2MIX5Iqhs2sMIwsMI1o8HjBi4sMd+kd7qb232XcwWTlP3iIWvq/0D3OxZ6J6uSNyC4UCl781lupLOKrC6ml58RUrYP8nrF0a53+i0hgLuDiCWhj0vkY7W9nJW1no425 andreas@python"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGAw9i7NfKt9C2v4Uc4gucNKy5Ru4HZD39aSpS0zLOUq andreas@management"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG2m+S9zhlgRKdHCcNf+WJA0bdd+Xoj6LatGflYnWmQv gwyn_root"
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCcwq2DNEJyum5Q362dlC97MW9IBJSiL4ZqXmQxdXSCiKvad6QSVic2UE+iBSzpOigB0uqGPH5iCQ2tgnTVB0n3Ttv0FV9Nchu8hEJA6tmdMOvAifVptv4KWU0H7uUI0EV1UewUznofvw67EbbQYhZiT0ydHgjohBBuvrhptME4YBaU74+ZeIDuVfQBRcj2dfvfQKZuJzyRcNg9Ms5XKMkJ2y2Z0KPX9dX91mAHio0grBtck+G2+OlswRmhE67lXoOkeiIqPo158QZEWEakfMCxHBP7KAE6OKevzXcViDksTzVy21LGGFaPvUMDszVal6Gq4WVK0yRjvIqzz66v8QVpsJ7mlsUM3vPbWphNjRNMq7UHcHyI8SYEKtzwC6CFCySMJLZNYPHGP6Q+zsqeSsD9DlxK0i/d5Yzs8YRImqcbdkFc56xJ5s4hd9mP4t4jIuiDR3hXQCIgoO2+a2R0nrRrmw4vD05hnye5+BYFnYLtsvgJGnEhKlmmBpL/T/kcUWk= andreas@co-ws-con4"
        ];
      };
    };
    services.az-binary-cache-common.enable = true;
  };
}
