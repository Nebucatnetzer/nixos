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
    memory.source = ./CLAUDE.md;
    settings = {
      outputStyle = "Mentor";
      sandbox = {
        enabled = true;
        allowUnsandboxedCommands = false;
        filesystem = {
          denyRead = [
            "./.env"
            "./secrets"
          ];
        };
      };
      permissions = {
        defaultMode = "plan";
        allow = [
          "Read"
          "WebSearch"

        ];
        deny = [
          "Bash(ask-vault-pass)"
          "Bash(curl *)"
          "Bash(git checkout*)"
          "Bash(git commit*)"
          "Bash(git push*)"
          "Bash(git restore*)"
          "Bash(git switch*)"
          "Bash(home-manager expire-generations *)"
          "Bash(home-manager init *)"
          "Bash(home-manager remove-generations *)"
          "Bash(home-manager switch *)"
          "Bash(nix run *)"
          "Bash(nix-collect-garbage *)"
          "Bash(nixos-rebuild boot *)"
          "Bash(nixos-rebuild switch *)"
          "Bash(nixos-rebuild test *)"
          "Bash(nixos-rebuild-ng boot *)"
          "Bash(nixos-rebuild-ng switch *)"
          "Bash(nixos-rebuild-ng test *)"
          "Bash(./scripts/update-single-machine *)"
          "Bash(rebuild)"
          "Bash(rm*)"
          "Bash(ssh*)"
          "Bash(sudo *)"
          "Read(./secrets)"
          "Write(./scrts/**)"
          "Write(~/.claude/settings.json)"
        ];
      };
    };
  };
}
