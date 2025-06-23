{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.az-librenms-certificate;
  certificateSource = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/librenms/librenms-agent/def5f830b3672526c4353a92a1804485f645733c/snmp/certificate.py";
    sha256 = "sha256-uAPf9auzoRQZUIRpjzZ3avC5wjeboRVATSXgtRH6l6U=";
  };
  certificatePy = pkgs.writers.writePython3Bin "certificate" {
    flakeIgnore = [
      "E501"
      "E265"
    ];
    libraries = [
      pkgs.python3Packages.pyopenssl
    ];
  } (builtins.readFile certificateSource);
in
{
  options.services.az-librenms-certificate = {
    enable = lib.mkEnableOption "LibreNMS agent";
    domains = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      description = ''An attribute set of the domains you want to monitor.'';
      example = ''
        [
          { fqdn = "foo.example.com"; }
        ]
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.etc."snmp/certificate.json".text = ''
      {
        "domains": ${builtins.toJSON cfg.domains}
      }
    '';
    services.az-snmpd.enable = true;
    services.snmpd.configText = ''
      extend certificate ${certificatePy}/bin/certificate
    '';
  };
}
