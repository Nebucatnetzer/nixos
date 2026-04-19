{ lib, ... }:
{
  options = {
    az-hosts = lib.mkOption {
      type = lib.types.attrs;
      description = "An attribute set of the hosts.";
    };
  };
  config = { };
}
