let
  andreas = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCR+JXNHSAEQamn2QiaKV0vejCPy6OmzOePXoaQF6CEknXyvBO4j7+qpgZ5RAhe7ups8xZrEpBKdtxRMf7OdQQEXg1PLlfWZSJTC8EGu1TbMltbwwHizgsK/15LkDhJ0Gk/GFz9O9GvGqjizik8Kvvqz8XWY0tEtYs5Riq8bB5D5Ctwl10iultqnIQkdaX0bNa/2X57XKeutWdbqhuSC/C7awC1aVDIdfy1BNT3weHhQhFVAeAlH7Fy4rx3gYPclICfzu27lulLeXKJj9F+NdeY84zEy7E8IkE7eqdo1zfdJJpXSIh3FqekWen5njzWJsXqZCa2Ynk1poK/Rv/ti+ySE+4XicyXp0VJM8fDz6iUI0S/pjumHwzpoN9CeNe5PDK3Y7iQzSlO9REvkj/+v7r2s6XKslk9B7hTKunvH5JgHlIeYymzXb4r2LggNrP/1KUgNk1Ztu+s1c5onXYfBNul1iQOFU3+kgTk8Oh/UFK3FA0dYeWrOLA02TdH2S7U6yE= andreas@gwyn";
  andreas-nixos-vm = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDA3/7NfN0GSFq4CCZa1BSizm95tFx7Ogz4IS2SaIosZ7xdjVpqAQ7iK/UG5GRKvBjhUnrGUwU9CRt6wYLhs47/qj20pMlnpvLb/0yDUwvY+2SfH6taZeLArL/o4d6jFrPuOGxP5Lt213qe/hZA0hnc6f7gZY13vXJ37Wm4K+st3N8qtk3lcTncb9aPrPoYBfrm7DGWBczdJtPPp/eJmgKATDHsy95hUuUYm17Pu2uzSVRaXIZnuevfySgRHi9qsk962GdC7CcWlctMGry7++1D0lzXdollseZQaKf40wRzMQJrOb7OueI0s5hlKsVNJR9SXbZm0rJhTeUJOzh3v+b5nNzcjqG4Cx6BOOg400ayvTxBC5pH7GRBO1Pgpj3+r2mWFOlPYwfIYS0EjoQIdKYQBefrqRrzDavancIFlqyifs8XQIru7PnF6IP6EmyQwIoybUlZzLPAWXSTXyyx7F0+w181+hYLnbHd7+u6ddVLXbUKfZ77SXiPThep9Tfw3J8= andreas@nixos";
  users = [ andreas andreas-nixos-vm ];

  git = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDO40In82pEqQJexG9nlXOsYb4T/sYrb/4EVtGc0bfEb";
  gwyn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGNmtdodpXHcwEsX2x89RyxjX5F6eERanzM4OXlNDx50";
  loki-test = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKqCmY9F2nWasFtmBpk401lacclXeddDm+OTZ4+tNM94";
  mail = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGQ64p/o1u8KUqxmOQkngMdNkfTCNkqYh8ptinatxUDX";
  management = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIENM7fUohjQY2BfkjCwMJ/hZzneBynREusTXBLX5LVnD";
  nextcloud = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHASRPSKyADQUBe6lQEo8EHixPwktbHQjAPX24GIoWwg";
  nixos-vm = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOcmWE9b7GQKOOq61gYLdFA5uZ+hhpBYePmmdRDGwIVu";
  plex = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDAp4qkxNLabAuwRSKjD1e7nNZ0QuB+BO2VxcYpdfr/X";
  proxy = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIACtJWes3zBh0Hs0BEC2ZC+9+ddLALlzuAxyNjLgf5Fh";
  staubfinger = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDcmDv8tnbuykX/0cUK+FnPD5YSjf/8wmsjWxqtXKuTYy1dtLS+Dx9X/LGS9GS1gd/LzYX+r9Kw1a4HfAz0+iinUaL/glbfGFm593BlS9jJaBz8nWV+pz3sJRj1GQ5oiKxN9bg+oNu8hZVpIqhMTpH7HkqgU5IWJfaVB5oNXaCCK7emh3fuJeqvQkKABumqji7eNr5la9qhc/XvI7O9aIc1sB05SVF+2TqYcZVpjMc27A3eSbS7+YXiOuP4I+51l9p7dH4Q1M9LB4+90XRP7DGA6kMwQ+cFTWMrFWwMy3NvaA9PnR2g3viNhbU7wLC+r6wCdS/Xu81HWwuXI/9lBScfZbxXIzfprjCUr4uifWevlTusYtgV0t1JJuWjefm8l/Sb+oJKEcGH/gxioM/pJCQiAcwoMVZRqZsNzYerNJ85VIKViuQhkek5A9EJsYhT1sOrQHYPGE+CReycwyswheXSnJ/VtkbyxRzu+q1573yfZgV5PVi8EUBI4i+gyvmz47E=";
  restic-server = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILX9Sm69VGPrek8PRgWa8xJPqzRrixs1g+8hBu2F6265";
  ttrss = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOfWq/ZWeMNIMqXsI4rnkwR+wc/FVdb2jA70sdLMEnyX";
  systems = [
    git
    gwyn
    loki-test
    mail
    management
    nextcloud
    nixos-vm
    plex
    proxy
    restic-server
    ttrss
  ];
  defaultKeys = [ andreas andreas-nixos-vm gwyn management nixos-vm staubfinger ];
  all = users ++ systems;
in
{
  "gitea_env.age".publicKeys = defaultKeys ++ [ git ];
  "infomaniak_env.age".publicKeys = all;
  "nextcloud_env.age".publicKeys = defaultKeys ++ [ nextcloud ];
  "personal_email.key.age".publicKeys = defaultKeys;
  "plex_claim.age".publicKeys = defaultKeys ++ [ plex ];
  "restic.key.age".publicKeys = all;
  "telegram_notify_env.age".publicKeys = all;
  "ttrss_env.age".publicKeys = defaultKeys ++ [ ttrss ];
}

