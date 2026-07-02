{
  inputs,
  pkgs,
  unstable-pkgs,
  ...
}:
let
  fetchPiExt =
    { repo, rev, hash }:
    pkgs.fetchFromGitHub {
      owner = "championswimmer";
      inherit repo rev hash;
    };

  # Token-utility pi extensions (championswimmer), pinned by Nix instead of `pi install`
  # so they stay reproducible and work inside the bwrap sandbox. All three are dependency
  # free (only peer deps on pi's bundled modules), so no node_modules build is needed.
  pi-context-usage = fetchPiExt {
    repo = "pi-context-usage";
    rev = "v1.0.2";
    hash = "sha256-FU9y5DAZlylm1LUFhUo850vR56lMOJ6T60txSHFj3iU=";
  };

  pi-cache-graph = fetchPiExt {
    repo = "pi-cache-graph";
    rev = "v1.0.2";
    hash = "sha256-kz3hpNJNopEXYIRRdzNmHzIb1sfYgLWDA0cjZ07DDpY=";
  };

  # pi bundles typebox under the specifier `typebox`, but pi-context-prune imports
  # `@sinclair/typebox` (same API). Rewrite the specifier so it resolves against pi's
  # bundled module without needing a node_modules closure.
  pi-context-prune = pkgs.runCommandLocal "pi-context-prune-1.2.0" { } ''
    cp -r ${
      fetchPiExt {
        repo = "pi-context-prune";
        rev = "v1.2.0";
        hash = "sha256-ApSQt1gcxfDRM+0TZvsBNu3iHGYG++OrO3k9q6jlTbw=";
      }
    } $out
    chmod -R +w $out
    find $out -name '*.ts' -exec sed -i 's#@sinclair/typebox#typebox#g' {} +
  '';
in
{
  # programs.pi-coding-agent is not yet in our pinned home-manager input.
  imports = [ ./pi-coding-agent.nix ];

  age.secrets.infomaniakAiToken = {
    file = "${inputs.self}/scrts/infomaniak_ai.key.age";
    mode = "600";
  };

  programs.pi-coding-agent = {
    enable = true;
    package = pkgs.callPackage ./pi_wrapper.nix {
      inherit (unstable-pkgs) pi-coding-agent;
    };

    settings = {
      defaultProvider = "infomaniak";
      defaultModel = "google/gemma-4-31B-it";

      # Token frugality: quiet startup, low thinking budget, enable compaction, and
      # expose skills as /skill:<name> commands.
      quietStartup = true;
      defaultThinkingLevel = "low";
      compaction.enabled = true;
      enableSkillCommands = true;

      # Nix-pinned token-utility extensions loaded from their store paths.
      # pi-context-prune: prune verbose tool outputs (agent-message keeps the cache
      #   prefix stable across the tool loop); recover originals via context_tree_query.
      # pi-context-usage: /context window breakdown. pi-cache-graph: /cache stats.
      packages = [
        "${pi-context-prune}"
        "${pi-context-usage}"
        "${pi-cache-graph}"
      ];
      piContextPrune.pruneOn = "agent-message";
    };

    # The Infomaniak provider (baseUrl, apiKey, and the model list with real context
    # windows + per-token pricing) is registered dynamically at startup by the
    # infomaniak-models.ts extension, fetched from the Infomaniak API. Nothing to
    # hard-code here; defaultProvider/defaultModel above must still name a valid entry.
    # models.json still needs the required top-level `providers` key to pass pi's schema,
    # so write an empty one; the extension adds the real provider at runtime.
    models.providers = { };

    context = ./AGENTS.md;
    memory = ./MEMORY.md;
  };

  # Extra pi resources dropped into ~/.pi/agent (auto-discovered by pi, and inside the
  # bwrap-bound ~/.pi). The trimmed pi-coding-agent.nix module has no options for these,
  # so wire them directly via home.file to keep that module an upstream drop-in.
  home.file = {
    ".pi/agent/APPEND_SYSTEM.md".source = ./APPEND_SYSTEM.md;

    # Extensions: read-only default + edit toggle, web_fetch tool, /exit alias,
    # dynamic Infomaniak model+pricing registration, cost/bill visibility, CLAUDE.md context injection,
    # and memory loading at session start.
    ".pi/agent/extensions/edit-mode.ts".source = ./extensions/edit-mode.ts;
    ".pi/agent/extensions/web-fetch.ts".source = ./extensions/web-fetch.ts;
    ".pi/agent/extensions/exit-alias.ts".source = ./extensions/exit-alias.ts;
    ".pi/agent/extensions/infomaniak-models.ts".source = ./extensions/infomaniak-models.ts;
    ".pi/agent/extensions/infomaniak-cost.ts".source = ./extensions/infomaniak-cost.ts;
    ".pi/agent/extensions/claude-context.ts".source = ./extensions/claude-context.ts;
    ".pi/agent/extensions/load-memory.ts".source = ./extensions/load-memory.ts;

    # Prompt templates -> /plan, /review, /commit, /explain.
    ".pi/agent/prompts/plan.md".source = ./prompts/plan.md;
    ".pi/agent/prompts/review.md".source = ./prompts/review.md;
    ".pi/agent/prompts/commit.md".source = ./prompts/commit.md;
    ".pi/agent/prompts/explain.md".source = ./prompts/explain.md;

    # Skills -> /skill:nixos-flake (progressive disclosure).
    ".pi/agent/skills/nixos-flake".source = ./skills/nixos-flake;
  };
}
