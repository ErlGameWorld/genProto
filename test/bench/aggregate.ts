/**
 * Aggregate multi-category bench results from test/bench/results/*.json
 * Run: node --experimental-strip-types aggregate.ts
 *
 * Expected file shape:
 * {
 *   language, wire_format?, iterations,
 *   cases: [{ category, message, msgId, body_bytes, encode_ms, decode_ms, encode_ops_per_sec, decode_ops_per_sec }]
 * }
 * Legacy single-case files are still accepted and wrapped as category=heavy.
 */
import { readFileSync, writeFileSync, readdirSync, mkdirSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

type CaseRow = {
   category: string;
   message: string;
   msgId: number;
   body_bytes: number;
   encode_ms: number;
   decode_ms: number;
   encode_ops_per_sec: number;
   decode_ops_per_sec: number;
};

type LangFile = {
   language: string;
   wire_format?: string;
   iterations: number;
   cases: CaseRow[];
};

type FlatRow = CaseRow & {
   language: string;
   wire_format: string;
   iterations: number;
};

const CATEGORY_ORDER = ["scalar", "list", "map", "oneof", "nested", "heavy"];

const here = dirname(fileURLToPath(import.meta.url));
const resultsDir = join(here, "results");
const reportMd = join(here, "bench_report.md");
const reportJson = join(here, "bench_summary.json");

function normalize(raw: any, fallbackLang: string): LangFile {
   if (Array.isArray(raw?.cases)) {
      return {
         language: raw.language ?? fallbackLang,
         wire_format: raw.wire_format,
         iterations: raw.iterations ?? 0,
         cases: raw.cases,
      };
   }
   // legacy single-object format
   return {
      language: raw.language ?? fallbackLang,
      wire_format: raw.wire_format,
      iterations: raw.iterations ?? 0,
      cases: [
         {
            category: "heavy",
            message: raw.message ?? "tBenchHeavy",
            msgId: raw.msgId ?? 0,
            body_bytes: raw.body_bytes ?? 0,
            encode_ms: raw.encode_ms ?? 0,
            decode_ms: raw.decode_ms ?? 0,
            encode_ops_per_sec: raw.encode_ops_per_sec ?? 0,
            decode_ops_per_sec: raw.decode_ops_per_sec ?? 0,
         },
      ],
   };
}

function loadFiles(): LangFile[] {
   if (!existsSync(resultsDir)) return [];
   const files = readdirSync(resultsDir).filter((f) => f.endsWith(".json"));
   const out: LangFile[] = [];
   for (const f of files) {
      const raw = JSON.parse(readFileSync(join(resultsDir, f), "utf8"));
      out.push(normalize(raw, f.replace(/\.json$/, "")));
   }
   out.sort((a, b) => a.language.localeCompare(b.language));
   return out;
}

function flatten(files: LangFile[]): FlatRow[] {
   const rows: FlatRow[] = [];
   for (const f of files) {
      const wf = f.wire_format ?? (f.language === "gpb" ? "protobuf" : "genproto");
      for (const c of f.cases) {
         rows.push({
            language: f.language,
            wire_format: wf,
            iterations: f.iterations,
            category: c.category,
            message: c.message,
            msgId: c.msgId,
            body_bytes: c.body_bytes,
            encode_ms: c.encode_ms,
            decode_ms: c.decode_ms,
            encode_ops_per_sec: c.encode_ops_per_sec,
            decode_ops_per_sec: c.decode_ops_per_sec,
         });
      }
   }
   return rows;
}

function rel(base: number, val: number): string {
   if (base <= 0) return "n/a";
   return (val / base).toFixed(2) + "x";
}

function pad(s: string, n: number): string {
   return s.length >= n ? s : s + " ".repeat(n - s.length);
}

function categoryOrder(cat: string): number {
   const i = CATEGORY_ORDER.indexOf(cat);
   return i >= 0 ? i : 999;
}

function formatReport(files: LangFile[], rows: FlatRow[]): string {
   if (rows.length === 0) return "No bench results found in test/bench/results/\n";

   const iters = files[0]?.iterations ?? 0;
   const cats = [...new Set(rows.map((r) => r.category))].sort(
      (a, b) => categoryOrder(a) - categoryOrder(b)
   );

   const lines: string[] = [];
   lines.push("# genProto 跨语言编解码性能对比（多场景）");
   lines.push("");
   lines.push(`- 迭代次数: **${iters.toLocaleString()}**（环境变量 \`BENCH_ITERS\` 可改）`);
   lines.push(`- 协议: genProto \`proto/3_bench.mpdf\`；gpb \`test/gpb/proto/bench_heavy.proto\``);
   lines.push(`- 场景: ${cats.map((c) => `\`${c}\``).join(", ")}`);
   lines.push(`- 说明: **body_bytes** 为编码后包体大小；gpb 为 Protobuf 线格式，与 genProto **不可字节级等价**，仅作体积对比`);
   lines.push("");

   // size comparison table across languages per category
   lines.push("## 包体大小对比（bytes）");
   lines.push("");
   const langs = [...new Set(rows.map((r) => r.language))];
   lines.push(`| 场景 / 消息 | ${langs.map((l) => `${l}`).join(" | ")} |`);
   lines.push(`|------------- | ${langs.map(() => "------:").join(" | ")} |`);
   for (const cat of cats) {
      const msg = rows.find((r) => r.category === cat)?.message ?? cat;
      const cells = langs.map((lang) => {
         const r = rows.find((x) => x.language === lang && x.category === cat);
         return r ? String(r.body_bytes) : "-";
      });
      lines.push(`| ${cat} / ${msg} | ${cells.join(" | ")} |`);
   }
   lines.push("");

   for (const cat of cats) {
      const subset = rows.filter((r) => r.category === cat);
      const msg = subset[0]?.message ?? cat;
      const msgId = subset.find((r) => r.language === "erlang")?.msgId ?? subset[0]?.msgId ?? 0;
      const erl = subset.find((r) => r.language === "erlang");
      const encBase = erl?.encode_ops_per_sec ?? 0;
      const decBase = erl?.decode_ops_per_sec ?? 0;

      lines.push(`## ${cat} — \`${msg}\` (msgId ${msgId})`);
      lines.push("");
      lines.push("| 语言 | 线格式 | 包体 B | 编码 ms | 解码 ms | 编码 ops/s | 解码 ops/s | 编码倍率 | 解码倍率 |");
      lines.push("|------|--------|-------:|--------:|--------:|-----------:|-----------:|--------:|--------:|");
      for (const r of subset.sort((a, b) => a.language.localeCompare(b.language))) {
         lines.push(
            `| ${r.language} | ${r.wire_format} | ${r.body_bytes} | ${r.encode_ms} | ${r.decode_ms} | ${r.encode_ops_per_sec.toLocaleString()} | ${r.decode_ops_per_sec.toLocaleString()} | ${rel(encBase, r.encode_ops_per_sec)} | ${rel(decBase, r.decode_ops_per_sec)} |`
         );
      }
      lines.push("");
   }

   lines.push(`生成时间: ${new Date().toISOString()}`);
   lines.push("");
   lines.push("---");
   lines.push("");

   // console summary
   console.log("");
   console.log("========== Multi-category Benchmark ==========");
   console.log(`iterations=${iters}`);
   for (const cat of cats) {
      const subset = rows.filter((r) => r.category === cat);
      console.log("");
      console.log(`-- ${cat} / ${subset[0]?.message ?? ""} --`);
      console.log(
         pad("Language", 12) +
            pad("Wire", 10) +
            pad("Body B", 10) +
            pad("Enc ms", 10) +
            pad("Dec ms", 10) +
            pad("Enc ops/s", 14) +
            "Dec ops/s"
      );
      for (const r of subset.sort((a, b) => a.language.localeCompare(b.language))) {
         console.log(
            pad(r.language, 12) +
               pad(r.wire_format, 10) +
               pad(String(r.body_bytes), 10) +
               pad(String(r.encode_ms), 10) +
               pad(String(r.decode_ms), 10) +
               pad(r.encode_ops_per_sec.toLocaleString(), 14) +
               r.decode_ops_per_sec.toLocaleString()
         );
      }
   }
   console.log("");

   return lines.join("\n");
}

function main(): void {
   mkdirSync(resultsDir, { recursive: true });
   const files = loadFiles();
   const rows = flatten(files);
   const md = formatReport(files, rows);
   writeFileSync(reportMd, md);
   writeFileSync(
      reportJson,
      JSON.stringify({ generated_at: new Date().toISOString(), files, rows }, null, 2) + "\n"
   );
   console.log(`Wrote ${reportMd}`);
   console.log(`Wrote ${reportJson}`);
}

main();
