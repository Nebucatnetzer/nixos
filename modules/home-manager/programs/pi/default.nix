{
  inputs,
  lib,
  pkgs,
  unstable-pkgs,
  ...
}:
let
  fetchPiExt =
    {
      repo,
      rev,
      hash,
    }:
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
  pi-context-prune = pkgs.runCommandLocal "pi-context-prune-1.3.0" { } ''
    cp -r ${
      fetchPiExt {
        repo = "pi-context-prune";
        rev = "v1.3.0";
        hash = "sha256-WZXcSB3N91NNNPNWihO5krX3a9dOKdSsV+Wgoyp2B14=";
      }
    } $out
    chmod -R +w $out
    find $out -type f -name "*.ts" -print -exec sed -i 's#@sinclair/typebox#typebox#g' {} +

    # The summarizer calls provider.stream directly without maxRetries, and pi-ai's
    # retryProviderRequest treats that as zero retries, so one 429 aborts the prune and
    # the batch is re-queued for the next flush. Opt it into the same retry-after aware
    # backoff the main loop gets. Drop this once upstream honours pi's retry settings.
    summarizer=$out/src/summarizer.ts
    anchor='\.\.\.summarizerThinkingOptions(config),'
    if ! grep -q "$anchor" "$summarizer"; then
      echo "pi-context-prune: retry patch anchor not found, re-check src/summarizer.ts" >&2
      exit 1
    fi
    sed -i "s#^\( *\)$anchor#\1maxRetries: 4,\n\1...summarizerThinkingOptions(config),#" "$summarizer"
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

      # Alabaster (light), linked into ~/.pi/agent/themes below and picked up by name.
      theme = "alabaster-light";

      # Infomaniak throttles, so 429s show up under load. retry.provider.* has no
      # default (undefined becomes 0 retries in pi-ai's retryProviderRequest), which
      # leaves only the session-level retry, and that resends the whole turn. Retrying
      # inside the provider call honours retry-after and costs no extra input tokens.
      retry.provider.maxRetries = 4;

      # Nix-pinned token-utility extensions loaded from their store paths.
      # pi-context-prune: prunes verbose tool outputs; recover originals via
      #   context_tree_query. It reads only ~/.pi/agent/context-prune/settings.json
      #   (seeded below); pi's settings.json has no key for it.
      # pi-context-usage: /context window breakdown. pi-cache-graph: /cache stats.
      packages = [
        "${pi-context-prune}"
        "${pi-context-usage}"
        "${pi-cache-graph}"
      ];
    };

    # The Infomaniak provider (baseUrl, apiKey, and the model list with real context
    # windows + per-token pricing) is registered dynamically at startup by the
    # infomaniak-models.ts extension, fetched from the Infomaniak API. Nothing to
    # hard-code here; defaultProvider/defaultModel above must still name a valid entry.
    # models.json still needs the required top-level `providers` key to pass pi's schema,
    # so write an empty one; the extension adds the real provider at runtime.
    models.providers = { };

    context = ../ai/RULES.md;
    memory = ./MEMORY.md;
  };

  # The pruner rewrites this file itself (/pruner ...), so copy it once rather than
  # linking it read-only from the store. agent-message batching merges a whole tool
  # chain into one summary; per-turn batching lets a single tiny tool result form its
  # own batch, where the summary can only ever come out larger than what it replaces.
  home.activation.seedPiPruneSettings = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    prune_settings="$HOME/.pi/agent/context-prune/settings.json"
    if [ ! -f "$prune_settings" ]; then
      ${pkgs.coreutils}/bin/mkdir -p "$HOME/.pi/agent/context-prune"
      ${pkgs.coreutils}/bin/cp ${./context-prune-settings.json} "$prune_settings"
      ${pkgs.coreutils}/bin/chmod +w "$prune_settings"
    fi
  '';

  # Extra pi resources dropped into ~/.pi/agent (auto-discovered by pi, and inside the
  # bwrap-bound ~/.pi). The trimmed pi-coding-agent.nix module has no options for these,
  # so wire them directly via home.file to keep that module an upstream drop-in.
  home.file = {
    # The read-only posture is mode-dependent for pi, so the modes extension injects it per
    # turn instead of an unconditional APPEND_SYSTEM.md. ai/ADVISORY.md is claude-only.

    # Move app.thinking.cycle off shift+tab so the modes extension can claim it.
    ".pi/agent/keybindings.json".source = ./keybindings.json;

    # Extensions: permission modes (plan/advise/edit) + bash guard, web_fetch tool,
    # /exit alias, dynamic Infomaniak model+pricing registration, cost/bill visibility,
    # CLAUDE.md context injection, and memory loading at session start.
    ".pi/agent/extensions/modes".source = ./extensions/modes;
    ".pi/agent/extensions/web-fetch.ts".source = ./extensions/web-fetch.ts;
    ".pi/agent/extensions/exit-alias.ts".source = ./extensions/exit-alias.ts;
    ".pi/agent/extensions/infomaniak-models.ts".source = ./extensions/infomaniak-models.ts;
    ".pi/agent/extensions/infomaniak-cost.ts".source = ./extensions/infomaniak-cost.ts;
    ".pi/agent/extensions/claude-context.ts".source = ./extensions/claude-context.ts;
    ".pi/agent/extensions/load-memory.ts".source = ./extensions/load-memory.ts;

    # Prompt templates -> /review, /commit, /explain. There is deliberately no plan.md:
    # the modes extension registers /plan, and a template of the same name would collide.
    # commit.md is shared with claude; pi reads only its `description`/`argument-hint`
    # frontmatter and ignores the rest (dist/core/prompt-templates.js, loadTemplateFromFile).
    ".pi/agent/prompts/review.md".source = ./prompts/review.md;
    ".pi/agent/prompts/commit.md".source = ../ai/commands/commit.md;
    ".pi/agent/prompts/explain.md".source = ./prompts/explain.md;

    # Alabaster (light) port. ~/.pi/agent/themes/*.json is auto-discovered; the file must
    # carry exactly the 51 schema tokens, since `colors` is additionalProperties: false.
    ".pi/agent/themes/alabaster-light.json".source = ./themes/alabaster-light.json;

    # Skills -> /skill:nixos-flake (progressive disclosure). Shared with claude.
    ".pi/agent/skills/nixos-flake".source = ../ai/skills/nixos-flake;
  };
}
