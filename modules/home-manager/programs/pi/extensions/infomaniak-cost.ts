// Cost visibility for the Infomaniak provider:
//   * a footer status showing the running cost of the current session, and
//   * a /bill command showing month-to-date account spend against the budget.
//
// Session cost reuses pi's own per-message usage.cost.total (populated from the model
// prices registered by infomaniak-models.ts), so it needs no extra API calls.
//
// /bill queries GET /1/ai/{product}/consumptions, which returns per-day buckets:
//   { date: <unix seconds>, total_cost: <number>, currency_id: 1|2,
//     prompt: {model: count}, cost: {model: number},
//     consumption: {model: {input_token_count, output_token_count}} }

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { readFileSync } from "node:fs";

const TOKEN_PATH = "/run/user/1000/agenix/infomaniakAiToken";
const PRODUCT_ID = "109278";
const CONSUMPTIONS_URL = `https://api.infomaniak.com/1/ai/${PRODUCT_ID}/consumptions`;
const MONTHLY_BUDGET_CHF = 20;

interface ConsumptionBucket {
  date?: number; // unix seconds
  total_cost?: number;
  currency_id?: number;
  cost?: Record<string, number>;
}

function currencyLabel(id?: number): string {
  return id === 2 ? "EUR" : "CHF"; // 1 = CHF, 2 = EUR
}

function sessionCostChf(ctx: any): number {
  let total = 0;
  for (const entry of ctx.sessionManager.getBranch()) {
    const cost = entry?.message?.usage?.cost?.total;
    if (typeof cost === "number") total += cost;
  }
  return total;
}

export default function (pi: ExtensionAPI) {
  function refreshStatus(ctx: any) {
    ctx?.ui?.setStatus?.(
      "infomaniak-cost",
      `session ~CHF ${sessionCostChf(ctx).toFixed(4)}`,
    );
  }

  pi.on("session_start", async (_event, ctx) => refreshStatus(ctx));
  pi.on("turn_end", async (_event, ctx) => refreshStatus(ctx));

  pi.registerCommand("bill", {
    description: "Show month-to-date Infomaniak AI spend (budget CHF 20)",
    handler: async (_args: string, ctx: any) => {
      let buckets: ConsumptionBucket[];
      try {
        const token = readFileSync(TOKEN_PATH, "utf8").trim();
        const response = await fetch(`${CONSUMPTIONS_URL}?per_page=1000`, {
          headers: { authorization: `Bearer ${token}` },
        });
        if (!response.ok) {
          ctx.ui.notify(`/bill: HTTP ${response.status} ${response.statusText}`, "error");
          return;
        }
        const payload = (await response.json()) as { data?: unknown };
        buckets = Array.isArray(payload.data) ? (payload.data as ConsumptionBucket[]) : [];
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        ctx.ui.notify(`/bill failed: ${message}`, "error");
        return;
      }

      const now = new Date();
      const monthStart = Math.floor(
        new Date(now.getFullYear(), now.getMonth(), 1).getTime() / 1000,
      );

      let monthToDate = 0;
      let returnedTotal = 0;
      const currency = currencyLabel(buckets[0]?.currency_id);
      for (const bucket of buckets) {
        const cost = typeof bucket.total_cost === "number" ? bucket.total_cost : 0;
        returnedTotal += cost;
        if ((bucket.date ?? 0) >= monthStart) monthToDate += cost;
      }

      const pct = ((monthToDate / MONTHLY_BUDGET_CHF) * 100).toFixed(0);
      const session = sessionCostChf(ctx).toFixed(4);
      ctx.ui.notify(
        `Infomaniak this month: ~${currency} ${monthToDate.toFixed(4)} / ` +
          `${MONTHLY_BUDGET_CHF} (${pct}%). ` +
          `Returned buckets total: ~${currency} ${returnedTotal.toFixed(4)}. ` +
          `This session: ~CHF ${session}.`,
        "info",
      );
    },
  });
}
