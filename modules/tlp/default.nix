{ ... }:
{
  services.tlp = {
    enable = true;
    settings = {
      DEVICES_TO_DISABLE_ON_STARTUP = "bluetooth wifi wwan";
      DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE = "bluetooth wifi wwan";
    };
  };
}
