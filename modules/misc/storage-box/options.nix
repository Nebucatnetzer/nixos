{ lib, ... }:
{
  options = {
    az-storage-box = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = ''
        restic settings for the Hetzner Storage Box: repository, extraResticArgs and
        pruneResticArgs. Set by modules/misc/storage-box, which profiles/management
        imports, so it is populated on every host that has the restic helpers.
      '';
    };
  };
  config = { };
}
