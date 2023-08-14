-module(gLuaGen).

-export([
   genLua/4
]).

spellFunHead(MsgName, MsgId) ->
   <<"function ", MsgName/binary, "()\n\tlocal tb = {}\n\ttb.msgId =", (integer_to_binary(MsgId))/binary, "\n">>.

spellMember(FieldList) ->
   <<<<(gLuaField:builtMemberStr(OneTypeName))/binary>> || OneTypeName <- FieldList>>.

spellEncode(FieldList) ->
   EnHead = <<"\n\ttb.encode = function(byteArray)\n">>,
   EnBody = <<<<(gLuaField:builtEncodeStr(OneTypeName))/binary>> || OneTypeName <- FieldList>>,
   EnEnd = <<"\t\treturn byteArray.getBytes()\n\tend\n\n">>,
   <<EnHead/binary, EnBody/binary, EnEnd/binary>>.

spellDecode(FieldList) ->
   EnHead = <<"\ttb.decode = function(byteArray)\n">>,
   EnBody = <<<<(gLuaField:builtDecodeStr(OneTypeName))/binary>> || OneTypeName <- FieldList>>,
   EnEnd = <<"\tend\n\n">>,
   <<EnHead/binary, EnBody/binary, EnEnd/binary>>.

spellBuild(MsgId) ->
   <<"\ttb.build = function(byteArray)\n\t\tbyteArray.setBytes({})\n\t\tbyteArray.write_uint16(", (integer_to_binary(MsgId))/binary, ")\n\t\treturn tb.encode(byteArray)\n\tend\n\n">>.

spellEnd() ->
   <<"\treturn tb\nend\n\n">>.

spellMsgName(MsgId, MsgName) ->
   <<"\t[", (integer_to_binary(MsgId))/binary, "] = \"", MsgName/binary, "\",\n">>.

genLua(SortedSProtoList, _SortedErrList, LuaDir, _) ->
   FunSpell =
      fun({MsgName, MsgId, FieldList}, {TableBinAcc, MsgNameBinAcc}) ->
         H = spellFunHead(MsgName, MsgId),
         M = spellMember(FieldList),
         E = spellEncode(FieldList),
         D = spellDecode(FieldList),
         B = spellBuild(MsgId),
         End = spellEnd(),
         MsgNameBin = spellMsgName(MsgId, MsgName),
         {<<TableBinAcc/binary, H/binary, M/binary, E/binary, D/binary, B/binary, End/binary>>, <<MsgNameBinAcc/binary, MsgNameBin/binary>>}
      end,
   {LastTableBinAcc, MsgNameBody} = lists:foldl(FunSpell, {<<>>, <<>>}, SortedSProtoList),
   LastMsgNameBody = <<"ProtoMsgName =\n{\n", MsgNameBody/binary, "}">>,

   %% todo error code
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