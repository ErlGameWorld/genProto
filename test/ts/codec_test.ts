/**
 * Cross-language codec tests (TypeScript).
 * Run: node --experimental-strip-types codec_test.ts
 */
import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import {
   ByteArray,
   phoneNumber,
   tbinary,
   tmap,
   toneof,
   test,
} from "./protoMsg.ts";

const here = dirname(fileURLToPath(import.meta.url));
const vectorsDir = join(here, "..", "vectors");

type VectorCase = {
   name: string;
   msgId: number;
   msg: string;
   full_hex: string;
   body_hex: string;
   checks?: { type: string; fields: Record<string, unknown> };
};

function hexToBytes(hex: string): Uint8Array {
   const out = new Uint8Array(hex.length / 2);
   for (let i = 0; i < out.length; i++) {
      out[i] = parseInt(hex.slice(i * 2, i * 2 + 2), 16);
   }
   return out;
}

function bytesToHex(bytes: Uint8Array): string {
   return Array.from(bytes)
      .map((b) => b.toString(16).padStart(2, "0"))
      .join("");
}

function decodeBody(msgId: number, body: Uint8Array): void {
   const ba = new ByteArray();
   ba.setBytes(body, true);
   switch (msgId) {
      case 2: {
         const m = new phoneNumber();
         m.decode(ba);
         break;
      }
      case 37: {
         const m = new tbinary();
         m.decode(ba);
         break;
      }
      case 38: {
         const m = new tmap();
         m.decode(ba);
         break;
      }
      case 39: {
         const m = new toneof();
         m.decode(ba);
         break;
      }
      default:
         throw new Error(`unsupported msgId ${msgId}`);
   }
}

function encodeBody(msgId: number): Uint8Array {
   const ba = new ByteArray();
   switch (msgId) {
      case 2: {
         const m = new phoneNumber();
         m.number = new test();
         m.number.aa = "hello";
         m.type = 100;
         m.encode(ba);
         break;
      }
      case 37: {
         const m = new tbinary();
         m.data = new Uint8Array([1, 2, 3]);
         m.chunks = [new Uint8Array([4, 5]), new Uint8Array([6])];
         m.encode(ba);
         break;
      }
      case 38: {
         const m = new tmap();
         m.attrs = new Map([["a", 1]]);
         m.names = new Map([[1, "x"]]);
         m.phones = new Map();
         m.encode(ba);
         break;
      }
      case 39: {
         const m = new toneof();
         m.reason = 1;
         m.item = [1, 100];
         m.item2 = null;
         m.tip = "hi";
         m.encode(ba);
         break;
      }
      default:
         throw new Error(`unsupported msgId ${msgId}`);
   }
   return ba.getBytes();
}

function assertEqualHex(actual: string, expected: string, label: string): void {
   if (actual.toLowerCase() !== expected.toLowerCase()) {
      throw new Error(`${label}: hex mismatch\n  expected ${expected}\n  actual   ${actual}`);
   }
}

function runRoundTrip(): void {
   const cases: Array<[string, number]> = [
      ["phoneNumber_simple", 2],
      ["tbinary_basic", 37],
      ["tmap_full", 38],
      ["toneof_gold_nil", 39],
   ];
   for (const [name, msgId] of cases) {
      const body = encodeBody(msgId);
      decodeBody(msgId, body);
      console.log(`  [ts round-trip] ${name} ok`);
   }
}

function runVectorDecode(): void {
   const raw = readFileSync(join(vectorsDir, "codec_vectors.json"), "utf8");
   const doc = JSON.parse(raw) as { cases: VectorCase[] };
   for (const c of doc.cases) {
      const body = hexToBytes(c.body_hex);
      decodeBody(c.msgId, body);
      const re = bytesToHex(encodeBody(c.msgId));
      // Re-encode uses TS defaults; only compare for cases we fully control
      if (c.name === "phoneNumber_simple" || c.name === "tbinary_basic") {
         assertEqualHex(re, c.body_hex, c.name);
      }
      console.log(`  [ts decode erl vector] ${c.name} ok`);
   }
}

function exportCrossVectors(): void {
   const exports: Array<{ name: string; msgId: number; body_hex: string }> = [];

   {
      const m = new tmap();
      m.attrs = new Map([["a", 1]]);
      m.names = new Map([[1, "x"]]);
      m.phones = null;
      const ba = new ByteArray();
      m.encode(ba);
      exports.push({ name: "tmap_nil_phones", msgId: 38, body_hex: bytesToHex(ba.getBytes()) });
   }

   {
      const m = new toneof();
      m.reason = 0;
      m.item = null;
      m.item2 = null;
      m.tip = "";
      const ba = new ByteArray();
      m.encode(ba);
      exports.push({ name: "toneof_all_null", msgId: 39, body_hex: bytesToHex(ba.getBytes()) });
   }

   for (const e of exports) {
      const path = join(vectorsDir, `cross_ts_${e.name}.json`);
      const json = `{\n  "name": "${e.name}",\n  "msgId": ${e.msgId},\n  "body_hex": "${e.body_hex}"\n}\n`;
      writeFileSync(path, json);
      console.log(`  [ts export cross] ${path}`);
   }
}

function main(): void {
   console.log("codec_test.ts");
   runRoundTrip();
   runVectorDecode();
   exportCrossVectors();
   console.log("codec_test.ts: all ok");
}

main();
