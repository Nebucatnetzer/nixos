let
  andreas = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCR+JXNHSAEQamn2QiaKV0vejCPy6OmzOePXoaQF6CEknXyvBO4j7+qpgZ5RAhe7ups8xZrEpBKdtxRMf7OdQQEXg1PLlfWZSJTC8EGu1TbMltbwwHizgsK/15LkDhJ0Gk/GFz9O9GvGqjizik8Kvvqz8XWY0tEtYs5Riq8bB5D5Ctwl10iultqnIQkdaX0bNa/2X57XKeutWdbqhuSC/C7awC1aVDIdfy1BNT3weHhQhFVAeAlH7Fy4rx3gYPclICfzu27lulLeXKJj9F+NdeY84zEy7E8IkE7eqdo1zfdJJpXSIh3FqekWen5njzWJsXqZCa2Ynk1poK/Rv/ti+ySE+4XicyXp0VJM8fDz6iUI0S/pjumHwzpoN9CeNe5PDK3Y7iQzSlO9REvkj/+v7r2s6XKslk9B7hTKunvH5JgHlIeYymzXb4r2LggNrP/1KUgNk1Ztu+s1c5onXYfBNul1iQOFU3+kgTk8Oh/UFK3FA0dYeWrOLA02TdH2S7U6yE= andreas@gwyn";
  users = [ andreas ];

  gwyn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGNmtdodpXHcwEsX2x89RyxjX5F6eERanzM4OXlNDx50";
  proxy = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIACtJWes3zBh0Hs0BEC2ZC+9+ddLALlzuAxyNjLgf5Fh";
  git = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGDYdusJTepCONjESFlQ1OTzCWqiTKwOFrYCyV5J77hS";
  nextcloud = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII4k14NXzaM5AqM9OfsFERnCT2ZMYXIaPrt34H2Jq5tx";
  ttrss = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILWQKvlLbat4Bp3iu4oFpdHSaiyi6x/efCMmYtCNfgfb";
  mail = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG1SpBpTmpUymwjkevYttnOAS4xkzu7wtQYRHTK6XRgQ";
  pihole = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ18YfLmcINwuPcqlAPJTLkNM4/JvA8MrKutlyzKYJtA";
  plex = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDsIpx4uigis0II6BW0s5Huum+sOnuS/R11MaNPh3qFm";
  restic-server = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPsjf063cd2QPTTRRP6s+VvAkDal1pHbVxBIycv5ntz+";
  management = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDXLCjTgxnu2WHt6AtEeJ0a0H3x6dc3Cb+GN8GAJPqRO";
  loki-test = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKqCmY9F2nWasFtmBpk401lacclXeddDm+OTZ4+tNM94";
  systems = [ gwyn proxy git nextcloud ttrss mail pihole plex restic-server management loki-test ];
in
{
  "test.age".publicKeys = users ++ systems;
  "plex_claim.age".publicKeys = users ++ systems;
}

