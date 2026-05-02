{ unstable-pkgs, ... }:
{
  programs.claude-code = {
    enable = true;
    package = unstable-pkgs.claude-code;
    settings = {
      sandbox = {
        enabled = true;
        autoAllowBashIfSandboxed = true;
        filesystem = {
          denyRead = [
            "./.env"
            "./secrets"
            "//etc)"
            "~/.config"
            "~/.ssh"
            "//mnt"
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
        ];
        deny = [
          "Bash(ask-vault-pass)"
          "Bash(curl *)"
          "Bash(git commit *)"
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
          "Write(pyproject.yml)"
          "Write(~/.claude/settings.json)"
        ];
      };
    };
  };
}
