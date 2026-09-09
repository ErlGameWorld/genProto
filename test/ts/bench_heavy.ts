/**
 * Multi-category encode/decode benchmark (TypeScript).
 * Run: node --experimental-strip-types bench_heavy.ts
 */
import { writeFileSync, mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { performance } from "node:perf_hooks";
import {
   ByteArray,
   benchLeaf,
   comboLeaf,
   phoneNumber,
   tBenchScalar,
   tBenchList,
   tBenchMap,
   tBenchOneof,
   tBenchNested,
   tBenchHeavy,
   test,
} from "./protoMsg.ts";

const DEFAULT_ITERS = 50_000;
const WARMUP = 200;

const here = dirname(fileURLToPath(import.meta.url));
const outDir = join(here, "..", "bench", "results");

type CaseResult = {
   category: string;
   message: string;
   msgId: number;
   body_bytes: number;
   encode_ms: number;
   decode_ms: number;
   encode_ops_per_sec: number;
   decode_ops_per_sec: number;
};

type BenchFile = {
   language: string;
   wire_format: string;
   iterations: number;
   cases: CaseResult[];
};

type SampleCase = {
   category: string;
   message: string;
   msgId: number;
   sample: { encode(ba: ByteArray): void; decode(ba: ByteArray): void };
   decodeNew: () => { decode(ba: ByteArray): void };
};

function iterations(): number {
   const v = process.env.BENCH_ITERS;
   return v ? parseInt(v, 10) : DEFAULT_ITERS;
}

function leaf(id: number): benchLeaf {
   const l = new benchLeaf();
   l.id = id;
   l.tag = "leaf";
   l.blob = new Uint8Array([1, 2, 3, 4]);
   return l;
}

function phone(): phoneNumber {
   const ph = new phoneNumber();
   ph.number = new test();
   ph.number.aa = "bench";
   ph.type = 7;
   return ph;
}

function encodeBody(m: { encode(ba: ByteArray): void }): Uint8Array {
   const ba = new ByteArray();
   m.encode(ba);
   return ba.getBytes();
}

function decodeBody(body: Uint8Array, make: () => { decode(ba: ByteArray): void }): void {
   const ba = new ByteArray();
   ba.setBytes(body, true);
   make().decode(ba);
}

function ops(n: number, ms: number): number {
   return ms <= 0 ? 0 : Math.round(n / (ms / 1000));
}

function runCase(c: SampleCase, iters: number): CaseResult {
   const body0 = encodeBody(c.sample);
   decodeBody(body0, c.decodeNew);
   for (let i = 0; i < WARMUP; i++) {
      encodeBody(c.sample);
      decodeBody(body0, c.decodeNew);
   }
   const t0 = performance.now();
   let body = body0;
   for (let i = 0; i < iters; i++) body = encodeBody(c.sample);
   const encMs = performance.now() - t0;
   const t1 = performance.now();
   for (let i = 0; i < iters; i++) decodeBody(body, c.decodeNew);
   const decMs = performance.now() - t1;
   return {
      category: c.category,
      message: c.message,
      msgId: c.msgId,
      body_bytes: body.length,
      encode_ms: Math.round(encMs * 100) / 100,
      decode_ms: Math.round(decMs * 100) / 100,
      encode_ops_per_sec: ops(iters, encMs),
      decode_ops_per_sec: ops(iters, decMs),
   };
}

function cases(): SampleCase[] {
   const sc = new tBenchScalar();
   sc.a1 = 1; sc.a2 = -1; sc.a3 = 100; sc.a4 = -100; sc.a5 = 1000; sc.a6 = -1000; sc.a7 = 100000; sc.a8 = -100000;
   sc.b1 = 1n; sc.b2 = -1n; sc.b3 = 9007199254740991n; sc.b4 = -9007199254740991n;
   sc.s1 = "alpha"; sc.s2 = "beta-beta"; sc.s3 = "gamma-gamma-gamma"; sc.s4 = "utf8-test";
   sc.p1 = new Uint8Array([0, 1, 2, 3]);
   sc.p2 = new Uint8Array(32).fill(9);
   sc.p3 = new Uint8Array(64).fill(7);
   sc.p4 = new Uint8Array(128).fill(3);
   sc.f1 = true; sc.f2 = false; sc.f3 = 1.5; sc.f4 = 2.5;

   const li = new tBenchList();
   li.scores = Array.from({ length: 128 }, (_, i) => i + 1);
   li.labels = Array.from({ length: 48 }, (_, i) => `lbl${i + 1}`);
   li.chunks = Array.from({ length: 16 }, (_, i) => new Uint8Array([i + 1, i + 2, i + 3, i + 4]));
   li.items = Array.from({ length: 48 }, (_, i) => leaf(i + 1));
   li.extras = Array.from({ length: 32 }, (_, i) => {
      const c = new comboLeaf(); c.id = i + 1; c.tag = "x"; return c;
   });

   const mp = new tBenchMap();
   mp.attrs = new Map(Array.from({ length: 32 }, (_, i) => [`k${i + 1}`, i + 1]));
   mp.rev = new Map(Array.from({ length: 32 }, (_, i) => [i + 1, `v${i + 1}`]));
   mp.lookup = new Map(Array.from({ length: 16 }, (_, i) => [`L${i + 1}`, leaf(i + 1)]));
   mp.phones = new Map(Array.from({ length: 8 }, (_, i) => [`p${i + 1}`, phone()]));
   mp.tags = new Map(Array.from({ length: 24 }, (_, i) => [`t${i + 1}`, `tag-${i + 1}`]));

   const oo = new tBenchOneof();
   oo.seq = 7;
   oo.reward = [1, 999];
   oo.meta = [2, "meta-note"];
   oo.payload = [1, new Uint8Array(64).fill(42)];
   oo.tip = "tip";

   const nest = new tBenchNested();
   nest.root = leaf(0);
   nest.contact = phone();
   nest.leaf = new comboLeaf(); nest.leaf.id = 1; nest.leaf.tag = "root";
   nest.children = Array.from({ length: 24 }, (_, i) => leaf(i + 1));
   nest.contacts = Array.from({ length: 12 }, () => phone());

   const hv = new tBenchHeavy();
   hv.seq = 42;
   hv.title = "bench heavy";
   hv.root = leaf(0);
   hv.items = Array.from({ length: 32 }, (_, i) => leaf(i + 1));
   hv.scores = Array.from({ length: 64 }, (_, i) => i + 1);
   hv.labels = Array.from({ length: 24 }, (_, i) => `lbl${i + 1}`);
   hv.chunks = Array.from({ length: 8 }, (_, i) => new Uint8Array([i + 1, i + 2, i + 3]));
   hv.attrs = new Map(Array.from({ length: 16 }, (_, i) => [`k${i + 1}`, i + 1]));
   hv.rev = new Map(Array.from({ length: 16 }, (_, i) => [i + 1, `v${i + 1}`]));
   hv.lookup = new Map(Array.from({ length: 8 }, (_, i) => [`L${i + 1}`, leaf(i + 1)]));
   hv.phones = new Map(Array.from({ length: 4 }, (_, i) => [`p${i + 1}`, phone()]));
   hv.reward = [1, 999];
   hv.meta = [2, "meta"];
   hv.payload = new Uint8Array(256);
   hv.extras = Array.from({ length: 16 }, (_, i) => {
      const c = new comboLeaf(); c.id = i + 1; c.tag = "x"; return c;
   });

   return [
      { category: "scalar", message: "tBenchScalar", msgId: 3002, sample: sc, decodeNew: () => new tBenchScalar() },
      { category: "list", message: "tBenchList", msgId: 3003, sample: li, decodeNew: () => new tBenchList() },
      { category: "map", message: "tBenchMap", msgId: 3004, sample: mp, decodeNew: () => new tBenchMap() },
      { category: "oneof", message: "tBenchOneof", msgId: 3005, sample: oo, decodeNew: () => new tBenchOneof() },
      { category: "nested", message: "tBenchNested", msgId: 3006, sample: nest, decodeNew: () => new tBenchNested() },
      { category: "heavy", message: "tBenchHeavy", msgId: 3007, sample: hv, decodeNew: () => new tBenchHeavy() },
   ];
}

function run(): void {
   const iters = iterations();
   const results = cases().map((c) => runCase(c, iters));
   const file: BenchFile = {
      language: "typescript",
      wire_format: "genproto",
      iterations: iters,
      cases: results,
   };
   mkdirSync(outDir, { recursive: true });
   const out = join(outDir, "typescript.json");
   writeFileSync(out, JSON.stringify(file, null, 2) + "\n");
   console.log(`bench_heavy.ts: ${out} (${results.length} cases, ${iters} iters)`);
   for (const r of results) {
      console.log(
         `  [${r.category}] ${r.message} msgId=${r.msgId} body=${r.body_bytes}B enc=${r.encode_ms} ms (${r.encode_ops_per_sec} ops/s) dec=${r.decode_ms} ms (${r.decode_ops_per_sec} ops/s)`
      );
   }
}

run();
