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

    # Claude is always read-only (see claude_wrapper.nix), so the advisory posture is
    # prepended unconditionally to the rules shared with pi. The option takes `lines` or a
    # path and branches on `lib.isPath`, which a derivation would not satisfy — so
    # concatenate the text rather than building a file.
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
