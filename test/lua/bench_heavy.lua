#!/usr/bin/env lua
-- Multi-category encode/decode benchmark (Lua)

dofile("ByteArray.lua")
dofile("protoMsg.lua")

local DEFAULT_ITERS = 50000
local WARMUP = 200
local encode_ba = ByteArray()
local decode_ba = ByteArray()

local function iterations()
   local v = os.getenv("BENCH_ITERS")
   if v and v ~= "" then return tonumber(v) end
   return DEFAULT_ITERS
end

local function leaf(id)
   local l = benchLeaf()
   l.id = id
   l.tag = "leaf"
   l.blob = string.char(1, 2, 3, 4)
   return l
end

local function phone()
   local ph = phoneNumber()
   ph.number = test()
   ph.number.aa = "bench"
   ph.type = 7
   return ph
end

local function encode_body(m)
   encode_ba.reset()
   m:encode(encode_ba)
   return encode_ba.getBytes()
end

local function decode_body(body, factory)
   decode_ba.setBytes(body)
   local m = factory()
   m:decode(decode_ba)
end

local function ops(n, ms)
   if ms <= 0 then return 0 end
   return math.floor(n / (ms / 1000))
end

local function sample_scalar()
   local m = tBenchScalar()
   m.a1=1; m.a2=-1; m.a3=100; m.a4=-100; m.a5=1000; m.a6=-1000; m.a7=100000; m.a8=-100000
   m.b1=1; m.b2=-1; m.b3=9007199254740991; m.b4=-9007199254740991
   m.s1="alpha"; m.s2="beta-beta"; m.s3="gamma-gamma-gamma"; m.s4="utf8-test"
   m.p1=string.char(0,1,2,3)
   m.p2=string.rep("\9", 32)
   m.p3=string.rep("\7", 64)
   m.p4=string.rep("\3", 128)
   m.f1=true; m.f2=false; m.f3=1.5; m.f4=2.5
   return m
end

local function sample_list()
   local m = tBenchList()
   m.scores = {}; for i=1,128 do m.scores[i]=i end
   m.labels = {}; for i=1,48 do m.labels[i]="lbl"..i end
   m.chunks = {}; for i=1,16 do m.chunks[i]=string.char(i,i+1,i+2,i+3) end
   m.items = {}; for i=1,48 do m.items[i]=leaf(i) end
   m.extras = {}; for i=1,32 do local c=comboLeaf(); c.id=i; c.tag="x"; m.extras[i]=c end
   return m
end

local function sample_map()
   local m = tBenchMap()
   m.attrs={}; for i=1,32 do m.attrs["k"..i]=i end
   m.rev={}; for i=1,32 do m.rev[i]="v"..i end
   m.lookup={}; for i=1,16 do m.lookup["L"..i]=leaf(i) end
   m.phones={}; for i=1,8 do m.phones["p"..i]=phone() end
   m.tags={}; for i=1,24 do m.tags["t"..i]="tag-"..i end
   return m
end

local function sample_oneof()
   local m = tBenchOneof()
   m.seq = 7
   m.reward = { 1, 999 }
   m.meta = { 2, "meta-note" }
   m.payload = { 1, string.rep(string.char(42), 64) }
   m.tip = "tip"
   return m
end

local function sample_nested()
   local m = tBenchNested()
   m.root = leaf(0)
   m.contact = phone()
   m.leaf = comboLeaf(); m.leaf.id=1; m.leaf.tag="root"
   m.children={}; for i=1,24 do m.children[i]=leaf(i) end
   m.contacts={}; for i=1,12 do m.contacts[i]=phone() end
   return m
end

local function sample_heavy()
   local m = tBenchHeavy()
   m.seq=42; m.title="bench heavy"; m.root=leaf(0)
   m.items={}; for i=1,32 do m.items[i]=leaf(i) end
   m.scores={}; for i=1,64 do m.scores[i]=i end
   m.labels={}; for i=1,24 do m.labels[i]="lbl"..i end
   m.chunks={}; for i=1,8 do m.chunks[i]=string.char(i,i+1,i+2) end
   m.attrs={}; for i=1,16 do m.attrs["k"..i]=i end
   m.rev={}; for i=1,16 do m.rev[i]="v"..i end
   m.lookup={}; for i=1,8 do m.lookup["L"..i]=leaf(i) end
   m.phones={}; for i=1,4 do m.phones["p"..i]=phone() end
   m.reward={1,999}; m.meta={2,"meta"}; m.payload=string.rep("\0",256)
   m.extras={}; for i=1,16 do local c=comboLeaf(); c.id=i; c.tag="x"; m.extras[i]=c end
   return m
end

local suite = {
   { category="scalar", message="tBenchScalar", msgId=3002, sample=sample_scalar, factory=tBenchScalar },
   { category="list", message="tBenchList", msgId=3003, sample=sample_list, factory=tBenchList },
   { category="map", message="tBenchMap", msgId=3004, sample=sample_map, factory=tBenchMap },
   { category="oneof", message="tBenchOneof", msgId=3005, sample=sample_oneof, factory=tBenchOneof },
   { category="nested", message="tBenchNested", msgId=3006, sample=sample_nested, factory=tBenchNested },
   { category="heavy", message="tBenchHeavy", msgId=3007, sample=sample_heavy, factory=tBenchHeavy },
}

local function run_case(c, iters)
   local m = c.sample()
   local body = encode_body(m)
   decode_body(body, c.factory)
   for _=1,WARMUP do encode_body(m); decode_body(body, c.factory) end
   local t0 = os.clock()
   for _=1,iters do body = encode_body(m) end
   local enc_ms = (os.clock() - t0) * 1000
   local t1 = os.clock()
   for _=1,iters do decode_body(body, c.factory) end
   local dec_ms = (os.clock() - t1) * 1000
   return {
      category = c.category,
      message = c.message,
      msgId = c.msgId,
      body_bytes = #body,
      encode_ms = enc_ms,
      decode_ms = dec_ms,
      encode_ops_per_sec = ops(iters, enc_ms),
      decode_ops_per_sec = ops(iters, dec_ms),
   }
end

local iters = iterations()
local results = {}
for i, c in ipairs(suite) do
   results[i] = run_case(c, iters)
end

local parts = {
   '{\n  "language": "lua",\n  "wire_format": "genproto",\n  "iterations": ' .. iters .. ',\n  "cases": [\n'
}
for i, r in ipairs(results) do
   parts[#parts+1] = string.format(
      '    {"category":"%s","message":"%s","msgId":%d,"body_bytes":%d,"encode_ms":%.2f,"decode_ms":%.2f,"encode_ops_per_sec":%d,"decode_ops_per_sec":%d}%s\n',
      r.category, r.message, r.msgId, r.body_bytes, r.encode_ms, r.decode_ms,
      r.encode_ops_per_sec, r.decode_ops_per_sec, i < #results and "," or "")
end
parts[#parts+1] = "  ]\n}\n"

local out = "../bench/results/lua.json"
local f = assert(io.open(out, "w"))
f:write(table.concat(parts))
f:close()

print(string.format("bench_heavy.lua: %s (%d cases, %d iters)", out, #results, iters))
for _, r in ipairs(results) do
   print(string.format("  [%s] %s msgId=%d body=%dB enc=%.2f ms (%d ops/s) dec=%.2f ms (%d ops/s)",
      r.category, r.message, r.msgId, r.body_bytes, r.encode_ms, r.encode_ops_per_sec, r.decode_ms, r.decode_ops_per_sec))
end
