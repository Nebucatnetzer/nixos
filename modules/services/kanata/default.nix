{ ... }:
{
  services.kanata = {
    enable = true;
    keyboards."notebook" = {
      config = builtins.readFile ./config.kbd;
      extraDefCfg = ''
        process-unmapped-keys yes
      '';
      devices = [ "/dev/input/by-path/platform-i8042-serio-0-event-kbd" ];
    };
  };
}
