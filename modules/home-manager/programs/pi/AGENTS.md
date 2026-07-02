# Environment Context

Global behavioral rules and the read-only advisory workflow live in `APPEND_SYSTEM.md`
(loaded alongside this file). This file holds only environment/context facts.

- Host OS is NixOS; many repositories here are Nix flakes (system config, home-manager).
- For working on the NixOS/home-manager config in this repo, see the `/skill:nixos-flake`
  skill for build/test commands and packaging conventions.
- Prefer the project's existing patterns, utilities, and idioms over introducing new ones.
