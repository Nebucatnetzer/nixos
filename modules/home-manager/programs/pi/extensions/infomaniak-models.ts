// Register the Infomaniak provider with a live model list, instead of hard-coding it in
// Nix. The async factory runs before pi finishes startup, so the models are available
// immediately (including to `pi --list-models`). Fully dynamic: if the essential model
// list can't be fetched there is no fallback (Infomaniak is the only provider).
//
// Sources (see infomaniak_api_1783025056.json):
//   GET /2/ai/{product}/openai/v1/models  -> authoritative ids available to the product
//   GET /1/ai/models                      -> max_token_input (real context window)
//   GET /1/ai/{product}/consumptions      -> per-model spend + tokens -> effective rate
//
// Infomaniak exposes no rate card (all models return empty `prices`), so per-token cost is
// derived from real usage history instead.

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { readFileSync } from "node:fs";

const TOKEN_PATH = "/run/user/1000/agenix/infomaniakAiToken";
const PRODUCT_ID = "109278";
const API = "https://api.infomaniak.com";
const BASE_URL = `${API}/2/ai/${PRODUCT_ID}/openai/v1`;
const CONSUMPTIONS_URL = `${API}/1/ai/${PRODUCT_ID}/consumptions`;

// Embedding / rerank models are not usable for chat; keep them out of the picker.
const EXCLUDE_MODEL = /(embed|embedding|rerank|^bge|mini_?lm)/i;

interface OpenAiModel {
  id: string;
}

interface AiModel {
  name?: string;
  max_token_input?: number;
}

interface ConsumptionBucket {
  cost?: Record<string, number>;
  consumption?: Record<string, { input_token_count?: number; output_token_count?: number }>;
}

async function getJson(url: string, token: string): Promise<any> {
  const response = await fetch(url, { headers: { authorization: `Bearer ${token}` } });
  if (!response.ok) {
    throw new Error(`${url} -> HTTP ${response.status} ${response.statusText}`);
  }
  return response.json();
}

// Effective CHF-per-1M-token rate per model, derived from real spend / tokens. Infomaniak
// has no rate card, so we blend input+output; for a coding agent context dominates, so this
// tracks the input rate closely. Models with no usage yet get no entry (rate 0).
function deriveRates(buckets: ConsumptionBucket[]): Map<string, number> {
  const spend = new Map<string, number>();
  const tokens = new Map<string, number>();
  for (const bucket of buckets) {
    for (const [model, amount] of Object.entries(bucket.cost ?? {})) {
      if (typeof amount === "number") spend.set(model, (spend.get(model) ?? 0) + amount);
    }
    for (const [model, use] of Object.entries(bucket.consumption ?? {})) {
      const count = (use?.input_token_count ?? 0) + (use?.output_token_count ?? 0);
      tokens.set(model, (tokens.get(model) ?? 0) + count);
    }
  }
  const rates = new Map<string, number>();
  for (const [model, total] of spend) {
    const count = tokens.get(model) ?? 0;
    if (count > 0) rates.set(model, (total / count) * 1_000_000);
  }
  return rates;
}

// Best-effort join between the OpenAI id and the catalog entry for the context window.
function matchCatalog(id: string, catalog: AiModel[]): AiModel | undefined {
  const exact = catalog.find((model) => model.name === id);
  if (exact) return exact;
  const lower = id.toLowerCase();
  return catalog.find((model) => {
    const name = model.name?.toLowerCase();
    return name ? name.endsWith(lower) || lower.endsWith(name) || name.includes(lower) : false;
  });
}

export default async function (pi: ExtensionAPI) {
  const token = readFileSync(TOKEN_PATH, "utf8").trim();

  // Essential: the authoritative model id list. A failure here has no fallback.
  const available = ((await getJson(`${BASE_URL}/models`, token)) as { data: OpenAiModel[] })
    .data ?? [];

  // Enrichment: best-effort, must not break startup if unavailable.
  let catalog: AiModel[] = [];
  try {
    catalog = ((await getJson(`${API}/1/ai/models`, token)) as { data: AiModel[] }).data ?? [];
  } catch {
    catalog = [];
  }
  let rates = new Map<string, number>();
  try {
    const payload = (await getJson(`${CONSUMPTIONS_URL}?per_page=1000`, token)) as {
      data?: ConsumptionBucket[];
    };
    rates = deriveRates(payload.data ?? []);
  } catch {
    rates = new Map();
  }

  const models = available
    .filter((entry) => !EXCLUDE_MODEL.test(entry.id))
    .sort((left, right) => left.id.localeCompare(right.id))
    .map((entry) => {
      const rate = rates.get(entry.id) ?? 0;
      return {
        id: entry.id,
        name: entry.id,
        reasoning: false,
        input: ["text"],
        // CHF per 1M tokens, effective rate from usage history (see deriveRates). No
        // separate cache price exists, so cached prompt tokens are billed as input.
        cost: { input: rate, output: rate, cacheRead: rate, cacheWrite: 0 },
        contextWindow: matchCatalog(entry.id, catalog)?.max_token_input ?? 32768,
        maxTokens: 4096,
      };
    });

  pi.registerProvider("infomaniak", {
    baseUrl: BASE_URL,
    apiKey: token,
    api: "openai-completions",
    models,
  });

  // pi has no price column anywhere; expose the derived rates via /rates.
  pi.registerCommand("rates", {
    description: "Show Infomaniak model rates (CHF per 1M tokens, from usage history)",
    handler: async (_args: string, ctx: any) => {
      const lines = models.map((model) =>
        model.cost.input > 0
          ? `${model.id}: ~${model.cost.input.toFixed(3)}`
          : `${model.id}: (no usage yet)`,
      );
      ctx.ui.notify(
        `CHF per 1M tokens (effective, from usage history):\n${lines.join("\n")}`,
        "info",
      );
    },
  });
}
