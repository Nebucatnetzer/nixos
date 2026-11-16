{ config, lib, ... }:
{
  # Windows cuts the speakers by about 20 dB around 350 Hz, Linux does not.
  # The preset copies that cut, measured with REW on both systems.
  home-manager.users.${config.az-username} = {
    services.easyeffects.extraPresets.Notebook = lib.importJSON ./easyeffects-notebook.json;
    xdg.dataFile."easyeffects/autoload/output/alsa_output.pci-0000_00_1f.3.analog-stereo:Speakers.json".text =
      builtins.toJSON {
        device = "alsa_output.pci-0000_00_1f.3.analog-stereo";
        device-description = "Built-in Audio Analog Stereo";
        device-profile = "Speakers";
        preset-name = "Notebook";
      };
  };
}
