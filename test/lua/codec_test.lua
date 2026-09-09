#!/usr/bin/env lua
-- Cross-language codec tests (Lua)
-- Run from test/lua: lua codec_test.lua

dofile("ByteArray.lua")
dofile("protoMsg.lua")

local encode_ba = ByteArray()

local function hex_to_bytes(hex)
   local t = {}
   for i = 1, #hex, 2 do
      t[#t + 1] = string.char(tonumber(hex:sub(i, i + 1), 16))
   end
   return table.concat(t)
end

local function bytes_to_hex(s)
   return (s:gsub('.', function(c)
      return string.format('%02x', string.byte(c))
   end))
end

local function decode_body(msgId, body)
   local ba = ByteArray()
   ba.setBytes(body)
   if msgId == 2 then
      local m = phoneNumber()
      m:decode(ba)
   elseif msgId == 37 then
      local m = tbinary()
      m:decode(ba)
   elseif msgId == 38 then
      local m = tmap()
      m:decode(ba)
   elseif msgId == 39 then
      local m = toneof()
      m:decode(ba)
   else
      error('unsupported msgId ' .. tostring(msgId))
   end
end

local function encode_body(_msgId, setup)
   encode_ba.reset()
   setup(encode_ba)
   return encode_ba.getBytes()
end

local function round_trip()
   local pn = phoneNumber()
   pn.number = test()
   pn.number.aa = 'hello'
   pn.type = 100
   local body = encode_body(2, function(ba) pn:encode(ba) end)
   decode_body(2, body)
   print('  [lua round-trip] phoneNumber_simple ok')

   local tm = tmap()
   tm.attrs = { a = 1 }
   tm.names = { [1] = 'x' }
   tm.phones = {}
   body = encode_body(38, function(ba) tm:encode(ba) end)
   decode_body(38, body)
   print('  [lua round-trip] tmap_full ok')

   local to = toneof()
   to.reason = 1
   to.item = { 1, 100 }
   to.item2 = nil
   to.tip = 'hi'
   body = encode_body(39, function(ba) to:encode(ba) end)
   decode_body(39, body)
   print('  [lua round-trip] toneof_gold_nil ok')
end

local function load_vectors()
   local path = '../vectors/codec_vectors.json'
   local f = io.open(path, 'r')
   if not f then
      print('  [lua vectors] skip: ' .. path .. ' not found')
      return
   end
   local content = f:read('*a')
   f:close()
   for name, bodyHex, msgId in content:gmatch('"name": "([%w_]+)".-"body_hex": "([0-9a-f]+)".-"msgId": (%d+)') do
      decode_body(tonumber(msgId), hex_to_bytes(bodyHex))
      print('  [lua decode erl vector] ' .. name .. ' ok')
   end
end

local function export_cross()
   local m = toneof()
   m.reason = 0
   m.item = nil
   m.item2 = nil
   m.tip = ''
   local body = encode_body(39, function(ba) m:encode(ba) end)
   local hex = bytes_to_hex(body)
   local out = '../vectors/cross_lua_toneof_all_null.json'
   local f = assert(io.open(out, 'w'))
   f:write(string.format('{\n  "name": "toneof_all_null",\n  "msgId": 39,\n  "body_hex": "%s"\n}\n', hex))
   f:close()
   print('  [lua export cross] ' .. out)
end

print('codec_test.lua')
round_trip()
load_vectors()
export_cross()
print('codec_test.lua: all ok')
