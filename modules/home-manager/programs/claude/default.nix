{ inputs, pkgs, unstable-pkgs, ... }:
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
      commit = ./skills/commit.md;
    };
    context = ./CLAUDE.md;
    mcpServers = {
      zotero = {
        type = "stdio";
        command = "${azPkgs.zotero-mcp}/bin/zotero-mcp";
        env.ZOTERO_LOCAL = "true";
      };
    };
  };
}
