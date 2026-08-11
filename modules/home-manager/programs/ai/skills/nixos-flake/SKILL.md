---
name: nixos-flake
description: Build and test individual custom packages and modules in this NixOS flake config. Use when working on packages under pkgs/, home-manager modules, or verifying that a Nix change evaluates and builds.
---
# NixOS flake conventions

This repo is a NixOS + home-manager flake.

## Build/test one custom package

Build a single custom package (and get its store path / hash) without a full rebuild:

```
nix build .#azPkgs.<name>
```

## Verify a home-manager change

Build the activation package to check evaluation before switching:

```
nix build .#homeConfigurations.<host>.activationPackage
```

Read `flake.nix` for the exact host / output names before running.

## Conventions

- First-party scripts under `pkgs/` omit the Nix `meta` section (packaging convention here).
- Custom packages are exposed via `azPkgs` (see `pkgs/default.nix`).
- Match the surrounding module's existing style and idioms rather than introducing new ones.
