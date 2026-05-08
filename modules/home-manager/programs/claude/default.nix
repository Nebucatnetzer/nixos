{ pkgs, unstable-pkgs, ... }:
{
  programs.claude-code = {
    enable = true;
    package = pkgs.callPackage ./claude_wrapper.nix {
      claude-code = unstable-pkgs.claude-code;
    };
    commands = {
      commit = ./skills/commit.md;
    };
    memory.source = ./CLAUDE.md;
    settings = {
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
        allow = [
          "Bash(black *)"
          "Bash(deadnix *)"
          "Bash(dev *)"
          "Bash(docformatter *)"
          "Bash(isort *)"
          "Bash(mypy *)"
          "Bash(nix build *)"
          "Bash(nixfmt *)"
          "Bash(prettier *)"
          "Bash(pylint *)"
          "Bash(pytest *)"
          "Bash(ruff *)"
          "Bash(shellcheck *)"
          "Bash(shfmt *)"
          "Read(//etc)"
          "WebSearch"

        ];
        deny = [
          "Bash(ask-vault-pass)"
          "Bash(curl *)"
          "Bash(git push *)"
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
