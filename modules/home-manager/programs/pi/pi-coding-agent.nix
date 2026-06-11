# Minimal local home-manager module for pi-coding-agent.
#
# Adapted from nix-community/home-manager (modules/programs/pi-coding-agent.nix).
# The upstream module is not yet in our pinned home-manager input, so we carry a
# trimmed copy covering only the options we use: enable, package, settings,
# models, and context. Drop this file and its import once the option lands in
# the pinned revision.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    mkPackageOption
    ;

  cfg = config.programs.pi-coding-agent;

  jsonFormat = pkgs.formats.json { };

  configDir = "${config.home.homeDirectory}/.pi/agent";
in
{
  options.programs.pi-coding-agent = {
    enable = mkEnableOption "pi-coding-agent";

    package = mkPackageOption pkgs "pi-coding-agent" { };

    settings = mkOption {
      inherit (jsonFormat) type;
      default = { };
      description = "Configuration written to {file}`~/.pi/agent/settings.json`.";
    };

    models = mkOption {
      inherit (jsonFormat) type;
      default = { };
      description = "Custom model providers written to {file}`~/.pi/agent/models.json`.";
    };

    context = mkOption {
      type = lib.types.path;
      description = "Path to a file written to {file}`~/.pi/agent/AGENTS.md` (global agent context).";
    };
  };

  config = mkIf cfg.enable {
    home = {
      packages = [ cfg.package ];

      file = {
        "${configDir}/settings.json".source =
          jsonFormat.generate "pi-coding-agent-settings.json" cfg.settings;
        "${configDir}/models.json".source = jsonFormat.generate "pi-coding-agent-models.json" cfg.models;
        "${configDir}/AGENTS.md".source = cfg.context;
      };
    };
  };
}
