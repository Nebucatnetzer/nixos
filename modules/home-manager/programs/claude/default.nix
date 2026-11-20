{
  inputs,
  lib,
  pkgs,
  unstable-pkgs,
  ...
}:
let
  azPkgs = import "${inputs.self}/pkgs" { inherit pkgs unstable-pkgs; };
in
{
  home.file.".claude/output-styles".source = ./output-styles;
  programs.claude-code = {
    enable = true;
    package = pkgs.callPackage ./claude_wrapper.nix {
      inherit (unstable-pkgs) claude-code;
    };
    commands = {
      commit = ../ai/commands/commit.md;
    };

    # Claude has no write mode switch like pi, so the advisory rules always come first.
    # The option checks `lib.isPath`, and a derivation fails that check. So join the text
    # instead of building a file.
    context = lib.concatStringsSep "\n" [
      (builtins.readFile ../ai/ADVISORY.md)
      (builtins.readFile ../ai/RULES.md)
    ];

    skills = ../ai/skills;
    mcpServers = {
      zotero = {
        type = "stdio";
        command = "${azPkgs.zotero-mcp}/bin/zotero-mcp";
        env.ZOTERO_LOCAL = "true";
      };
    };
  };
}
