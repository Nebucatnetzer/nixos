let
  andreas = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCR+JXNHSAEQamn2QiaKV0vejCPy6OmzOePXoaQF6CEknXyvBO4j7+qpgZ5RAhe7ups8xZrEpBKdtxRMf7OdQQEXg1PLlfWZSJTC8EGu1TbMltbwwHizgsK/15LkDhJ0Gk/GFz9O9GvGqjizik8Kvvqz8XWY0tEtYs5Riq8bB5D5Ctwl10iultqnIQkdaX0bNa/2X57XKeutWdbqhuSC/C7awC1aVDIdfy1BNT3weHhQhFVAeAlH7Fy4rx3gYPclICfzu27lulLeXKJj9F+NdeY84zEy7E8IkE7eqdo1zfdJJpXSIh3FqekWen5njzWJsXqZCa2Ynk1poK/Rv/ti+ySE+4XicyXp0VJM8fDz6iUI0S/pjumHwzpoN9CeNe5PDK3Y7iQzSlO9REvkj/+v7r2s6XKslk9B7hTKunvH5JgHlIeYymzXb4r2LggNrP/1KUgNk1Ztu+s1c5onXYfBNul1iQOFU3+kgTk8Oh/UFK3FA0dYeWrOLA02TdH2S7U6yE= andreas@gwyn";
  andreas-management = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGAw9i7NfKt9C2v4Uc4gucNKy5Ru4HZD39aSpS0zLOUq andreas@management";
  andreas-nixos-vm = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDA3/7NfN0GSFq4CCZa1BSizm95tFx7Ogz4IS2SaIosZ7xdjVpqAQ7iK/UG5GRKvBjhUnrGUwU9CRt6wYLhs47/qj20pMlnpvLb/0yDUwvY+2SfH6taZeLArL/o4d6jFrPuOGxP5Lt213qe/hZA0hnc6f7gZY13vXJ37Wm4K+st3N8qtk3lcTncb9aPrPoYBfrm7DGWBczdJtPPp/eJmgKATDHsy95hUuUYm17Pu2uzSVRaXIZnuevfySgRHi9qsk962GdC7CcWlctMGry7++1D0lzXdollseZQaKf40wRzMQJrOb7OueI0s5hlKsVNJR9SXbZm0rJhTeUJOzh3v+b5nNzcjqG4Cx6BOOg400ayvTxBC5pH7GRBO1Pgpj3+r2mWFOlPYwfIYS0EjoQIdKYQBefrqRrzDavancIFlqyifs8XQIru7PnF6IP6EmyQwIoybUlZzLPAWXSTXyyx7F0+w181+hYLnbHd7+u6ddVLXbUKfZ77SXiPThep9Tfw3J8= andreas@nixos";
  users = [
    andreas
    andreas-management
    andreas-nixos-vm
  ];

  budget = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMQi+/CpU4lMi2E1106NuK9+DX2fNWtAxioLb9/3a2U/";
  capricorn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRobGMQkRNxHCjRsNGDgCivywhVylkyN71V1ikWiPhX root@capricorn";
  git = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKYqSAhHFHpDzlUjm7VJcwfoRaXxUz7QP7CCg6R637uq";
  gwyn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDgU7uBGqpsp39oIotlhE5ohdFyTMGkLqOScW5ER6KAA root@gwyn";
  management = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICr5M2m7cic6fCaVNEesCn8Ii/VDB0EtyxYWs79aE4BD";
  nextcloud = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHASRPSKyADQUBe6lQEo8EHixPwktbHQjAPX24GIoWwg";
  plex = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJGQwCwCgYq2iO/wHoj4dGGXzVNtaTNG5JgBIUnGkLPN";
  proxy = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJImsnXnmjAOXCzNHuYdPL/usGLokxmGS60JNIfPA0aX";
  restic-server = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIk2L0h0eV7wZDaltoSJUtkRX7KCErCfGtpEYCBH4nyP";
  ttrss = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAyv7bwarmm7mpXeghkT766JR658XLwpYSGw4GbVB6tn";

  systems = [
    budget
    git
    gwyn
    management
    nextcloud
    plex
    proxy
    restic-server
    ttrss
  ];
  defaultKeys = [
    andreas
    andreas-management
    andreas-nixos-vm
    capricorn
    gwyn
    management
  ];
  all = users ++ systems;
in
{
  "cache_readonly.age".publicKeys = all;
  "capricorn_wg.key.age".publicKeys = defaultKeys;
  "davis_admin_password.age".publicKeys = defaultKeys;
  "davis_app_secret.age".publicKeys = defaultKeys;
  "davis_mail_dsn.age".publicKeys = defaultKeys;
  "freshrss_db_pass.age".publicKeys = defaultKeys ++ [ ttrss ];
  "freshrss_user_pass.age".publicKeys = defaultKeys ++ [ ttrss ];
  "gitea_env.age".publicKeys = defaultKeys ++ [ git ];
  "gwyn_wg.key.age".publicKeys = defaultKeys;
  "infomaniak_env.age".publicKeys = all;
  "mail_password.age".publicKeys = defaultKeys;
  "nextcloud_cli.age".publicKeys = defaultKeys;
  "nextcloud_env.age".publicKeys = defaultKeys ++ [ nextcloud ];
  "personal_email.key.age".publicKeys = defaultKeys;
  "plex_claim.age".publicKeys = defaultKeys ++ [ plex ];
  "restic.key.age".publicKeys = all;
  "searxng_env.age".publicKeys = defaultKeys ++ [ proxy ];
  "searxng_htpasswd.age".publicKeys = defaultKeys ++ [ proxy ];
  "telegram_notify_env.age".publicKeys = all;
  "zweili_search_env.age".publicKeys = defaultKeys ++ [ proxy ];
}
