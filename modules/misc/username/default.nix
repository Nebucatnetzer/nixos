{ lib, ... }: {
  options = {
    az-username = lib.mkOption {
      type = lib.types.str;
      description = "The main user of a system";
    };
  };
  config = { };
}
