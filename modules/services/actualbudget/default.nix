{
  domain,
  name,
  port,
}:
{
  inputs,
  lib,
  pkgs,
  unstable-pkgs,
  ...
}:
let
  configFile = (pkgs.formats.json { }).generate "${serviceName}.json" {
    hostname = "127.0.0.1";
    inherit port;
    dataDir = dataDirectory;
    serverFiles = "${dataDirectory}/server-files";
    userFiles = "${dataDirectory}/user-files";
    loginMethod = "password";
    allowedLoginMethods = [ "password" ];
    trustedProxies = [
      "127.0.0.1/32"
      "::1/128"
    ];
  };
  dataDirectory = "/var/lib/actualbudget/${name}";
  serviceName = "actual-${name}";
in
{
  imports = [
    "${inputs.self}/modules/services/nginx-acme-base"
    "${inputs.self}/modules/services/telegram-notifications"
  ];
  # Webserver setup
  services.nginx = {
    enable = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;
    virtualHosts."${domain}" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${builtins.toString port}";
        proxyWebsockets = true; # needed if you need to use WebSocket
      };
    };
  };

  # nixpkgs services.actual supports only one instance, so this copies its unit.
  # https://github.com/NixOS/nixpkgs/blob/6510408d147d8c5e6d76043e899e9251756f6a75/nixos/modules/services/web-apps/actual.nix
  systemd.services."${serviceName}" = {
    description = "Actual budget server for ${domain}";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    environment.ACTUAL_CONFIG_PATH = configFile;
    serviceConfig = {
      # nixpkgs 26.05 has an older version than the data was last migrated with.
      ExecStart = lib.getExe unstable-pkgs.actual-server;
      User = serviceName;
      Group = serviceName;
      StateDirectory = "actualbudget/${name}";
      StateDirectoryMode = "0700";
      WorkingDirectory = dataDirectory;
      LimitNOFILE = "1048576";
      PrivateTmp = true;
      PrivateDevices = true;
      Restart = "always";

      # Hardening
      CapabilityBoundingSet = "";
      LockPersonality = true;
      #MemoryDenyWriteExecute = true; # Leads to coredump because V8 does JIT
      PrivateUsers = true;
      ProtectClock = true;
      ProtectControlGroups = true;
      ProtectHome = true;
      ProtectHostname = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectProc = "invisible";
      ProcSubset = "pid";
      ProtectSystem = "strict";
      RestrictAddressFamilies = [
        "AF_INET"
        "AF_INET6"
        "AF_NETLINK"
      ];
      RestrictNamespaces = true;
      RestrictRealtime = true;
      SystemCallArchitectures = "native";
      SystemCallFilter = [
        "@system-service"
        "@pkey"
      ];
      UMask = "0077";
    };
  };

  users.users."${serviceName}" = {
    isSystemUser = true;
    group = serviceName;
  };
  users.groups."${serviceName}" = { };

}
