{ pkgs, unstable-pkgs, ... }:
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
  };
}
