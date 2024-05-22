{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.az-copilot-cli;
in
{
  options = {
    programs.az-copilot-cli.enable = lib.mkEnableOption "Enable NeoVIM configured with Copilot Chat";
  };
  config = lib.mkIf cfg.enable {
    programs.neovim = {
      enable = true;
      plugins = [ pkgs.unstable.vimPlugins.CopilotChat-nvim ];
      extraLuaConfig = ''
        require("copilot").setup({})
        require("CopilotChat").setup({})
        local chat = require("CopilotChat")

        -- Open chat window
        chat.open()
      '';
    };
  };
}
