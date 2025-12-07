{ inputs, ... }:
{
  imports = [
    "${inputs.self}/modules/services/snmpd"
    "${inputs.self}/modules/services/syslog"
  ];
}
