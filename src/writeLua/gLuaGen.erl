-module(gLuaGen).

-export([
   genLua/4
]).

spellMtHead(MsgName) ->
   <<MsgName/binary, "_mt = {}\n", MsgName/binary, "_mt.__index = ", MsgName/binary, "_mt\n\n">>.

spellFunHead(MsgName, MsgId) ->
   <<"function ", MsgName/binary, "()\n\treturn setmetatable({\n\t\tmsgId = ", (integer_to_binary(MsgId))/binary, ",\n">>.

spellMember(FieldList) ->
   <<<<(gLuaField:builtMemberStr(OneTypeName))/binary>> || OneTypeName <- FieldList>>.

spellFunEnd(MsgName) ->
   <<"\t}, ", MsgName/binary, "_mt)\nend\n\n">>.

spellEncode(MsgName, FieldList) ->
   EnHead = <<"function ", MsgName/binary, "_mt:encode(byteArray)\n">>,
   EnBody = <<<<(gLuaField:builtEncodeStr(OneTypeName))/binary>> || OneTypeName <- FieldList>>,
   EnEnd = <<"\treturn byteArray\nend\n\n">>,
   <<EnHead/binary, EnBody/binary, EnEnd/binary>>.

spellDecode(MsgName, FieldList) ->
   DeHead = <<"function ", MsgName/binary, "_mt:decode(byteArray)\n">>,
   DeBody = <<<<(gLuaField:builtDecodeStr(OneTypeName))/binary>> || OneTypeName <- FieldList>>,
   DeEnd = <<"end\n\n">>,
   <<DeHead/binary, DeBody/binary, DeEnd/binary>>.

spellBuild(MsgName, MsgId) ->
   <<"function ", MsgName/binary, "_mt:build(byteArray)\n",
     "\tbyteArray.reset()\n",
     "\tbyteArray.write_uint16(", (integer_to_binary(MsgId))/binary, ")\n",
     "\tself:encode(byteArray)\n",
     "\treturn byteArray.getBytes()\n",
     "end\n\n">>.

spellMsgName(MsgId, MsgName) ->
   <<"\t[", (integer_to_binary(MsgId))/binary, "] = \"", MsgName/binary, "\",\n">>.

genLua(SortedSProtoList, _SortedErrList, LuaDir, _) ->
   ok = write_byte_array(LuaDir),
   FunSpell =
      fun({MsgName, MsgId, FieldList}, {TableBinAcc, MsgNameBinAcc}) ->
         Mt = spellMtHead(MsgName),
         H = spellFunHead(MsgName, MsgId),
         M = spellMember(FieldList),
         FE = spellFunEnd(MsgName),
         E = spellEncode(MsgName, FieldList),
         D = spellDecode(MsgName, FieldList),
         B = spellBuild(MsgName, MsgId),
         MsgNameBin = spellMsgName(MsgId, MsgName),
         {<<TableBinAcc/binary, Mt/binary, H/binary, M/binary, FE/binary, E/binary, D/binary, B/binary>>, <<MsgNameBinAcc/binary, MsgNameBin/binary>>}
      end,
   {LastTableBinAcc, MsgNameBody} = lists:foldl(FunSpell, {<<>>, <<>>}, SortedSProtoList),
   LastMsgNameBody = <<"ProtoMsgName =\n{\n", MsgNameBody/binary, "}">>,

   LuaFilename = do_write_lua(LuaDir, protoMsg, LastTableBinAcc),
   MsgNameFile = do_write_lua(LuaDir, protoName, LastMsgNameBody),

   io:format("protoConvert lua dir : ~s ~n", [LuaDir]),
   io:format("protoConvert to lua file ~s ~s succ.~n", [LuaFilename, MsgNameFile]),
   ok.

do_write_lua(OutDir, Mod, BinStr) ->
   Filename = filename:join([OutDir, atom_to_list(Mod) ++ ".lua"]),
   case file:write_file(Filename, BinStr) of
      ok ->
         ok;
      _Ret ->
         io:format("write to lua file error:~p ~n", [_Ret])
   end,
   Filename.

write_byte_array(LuaDir) ->
   case file:read_file(byte_array_template_path()) of
      {ok, Bin} ->
         Dst = filename:join([LuaDir, "ByteArray.lua"]),
         file:write_file(Dst, Bin);
      {error, Reason} ->
         io:format("warning: skip ByteArray.lua (~p)~n", [Reason]),
         ok
   end.

byte_array_template_path() ->
   filename:join([repo_root(), "src", "writeLua", "gLuaCode.lua"]).

repo_root() ->
   Script = escript:script_name(),
   filename:absname(
      filename:join([filename:dirname(Script), "..", "..", ".."])
   ).
