{
  inputs,
  pkgs,
  unstable-pkgs,
  ...
}:
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
      defaultModel = "Qwen/Qwen3.5-122B-A10B-FP8";
    };

    models.providers.infomaniak = {
      # Infomaniak AI Tools product id (from `GET /1/ai`; not a secret).
      baseUrl = "https://api.infomaniak.com/2/ai/109278/openai/v1";
      api = "openai-completions";
      apiKey = "!cat /run/user/1000/agenix/infomaniakAiToken";
      models = [
        {
          id = "Qwen/Qwen3.5-122B-A10B-FP8";
          name = "Qwen3.5 122B (Infomaniak)";
        }
        {
          id = "swiss-ai/Apertus-70B-Instruct-2509";
          name = "Apertus 70B (Infomaniak)";
        }
        {
          id = "moonshotai/Kimi-K2.6";
          name = "Kimi K2.6 (Infomaniak)";
        }
        {
          id = "mistralai/Mistral-Small-4-119B-2603";
          name = "Mistral Small 4 119B (Infomaniak)";
        }
        {
          id = "mistralai/Ministral-3-14B-Instruct-2512";
          name = "Ministral 3 14B (Infomaniak)";
        }
        {
          id = "google/gemma-4-31B-it";
          name = "Gemma 4 31B (Infomaniak)";
        }
        {
          id = "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-FP8";
          name = "Nemotron 3 Nano 30B (Infomaniak)";
        }
      ];
    };

    context = ./AGENTS.md;
  };
}
