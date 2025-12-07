{ domains }:
{ inputs, pkgs, ... }:
let
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
  imports = [
    "${inputs.self}/modules/services/snmpd"
  ];
  environment.etc."snmp/certificate.json".text = ''
    {
      "domains": ${builtins.toJSON domains}
    }
  '';
  services.snmpd.configText = ''
    extend certificate ${certificatePy}/bin/certificate
  '';
}
