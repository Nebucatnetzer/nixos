{ hostname }:
{
  config,
  inputs,
  lib,
  ...
}:
{
  hardware = {
    az-raspi4-ethernet = {
      enable = true;
      hostname = hostname;
      ip = "10.7.89.150";
    };
  };

  age.secrets.signingKey = {
    file = "${inputs.self}/scrts/signing.key.age";
    mode = "600";
    owner = "root";
    group = "root";
  };
  # Features
  nix = {
    gc = {
      automatic = lib.mkForce false;
    };
    settings = {
      min-free = "300G";
      max-free = "512G";
      secret-key-files = config.age.secrets.signingKey.path;
    };
    sshServe = {
      enable = true;
      keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCR+JXNHSAEQamn2QiaKV0vejCPy6OmzOePXoaQF6CEknXyvBO4j7+qpgZ5RAhe7ups8xZrEpBKdtxRMf7OdQQEXg1PLlfWZSJTC8EGu1TbMltbwwHizgsK/15LkDhJ0Gk/GFz9O9GvGqjizik8Kvvqz8XWY0tEtYs5Riq8bB5D5Ctwl10iultqnIQkdaX0bNa/2X57XKeutWdbqhuSC/C7awC1aVDIdfy1BNT3weHhQhFVAeAlH7Fy4rx3gYPclICfzu27lulLeXKJj9F+NdeY84zEy7E8IkE7eqdo1zfdJJpXSIh3FqekWen5njzWJsXqZCa2Ynk1poK/Rv/ti+ySE+4XicyXp0VJM8fDz6iUI0S/pjumHwzpoN9CeNe5PDK3Y7iQzSlO9REvkj/+v7r2s6XKslk9B7hTKunvH5JgHlIeYymzXb4r2LggNrP/1KUgNk1Ztu+s1c5onXYfBNul1iQOFU3+kgTk8Oh/UFK3FA0dYeWrOLA02TdH2S7U6yE= andreas@gwyn
"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGAw9i7NfKt9C2v4Uc4gucNKy5Ru4HZD39aSpS0zLOUq andreas@management"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG2m+S9zhlgRKdHCcNf+WJA0bdd+Xoj6LatGflYnWmQv gwyn_root"
      ];
    };
  };
  profiles.az-server.enable = true;
  services = {
    az-data-share.enable = true;
    az-docker.enable = true;
    az-logs-share.enable = true;
    az-nextcloud-cli-client.enable = true;
    az-restic-client-server-postgres = {
      enable = true;
      path = "/home/andreas";
      tag = "management";
      time = "23:30";
    };
  };
  # Enable dictionaries
  programs = {
    az-nix-direnv.enable = true;
    az-restic-management.enable = true;
  };
}
