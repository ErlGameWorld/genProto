-module(gCsField).

-compile([export_all, nowarn_export_all]).

-define(TypeValue, [
   {<<"bool">>, <<"bool">>, <<"ProtocolCore.BasicTypeEnum.Boolean">>}
   , {<<"int8">>, <<"sbyte">>, <<"ProtocolCore.BasicTypeEnum.Int8">>}
   , {<<"uint8">>, <<"byte">>, <<"ProtocolCore.BasicTypeEnum.UInt8">>}
   , {<<"int16">>, <<"short">>, <<"ProtocolCore.BasicTypeEnum.Int16">>}
   , {<<"uint16">>, <<"ushort">>, <<"ProtocolCore.BasicTypeEnum.UInt16">>}
   , {<<"int32">>, <<"int">>, <<"ProtocolCore.BasicTypeEnum.Int32">>}
   , {<<"uint32">>, <<"uint">>, <<"ProtocolCore.BasicTypeEnum.UInt32">>}
   ,    {<<"int64">>, <<"long">>, <<"ProtocolCore.BasicTypeEnum.Int64">>}
   , {<<"uint64">>, <<"ulong">>, <<"ProtocolCore.BasicTypeEnum.UInt64">>}
   , {<<"integer">>, <<"long">>, <<"ProtocolCore.BasicTypeEnum.Int64">>}
   , {<<"number">>, <<"double">>, <<"ProtocolCore.BasicTypeEnum.Double">>}
   , {<<"float">>, <<"float">>, <<"ProtocolCore.BasicTypeEnum.Float">>}
   , {<<"double">>, <<"double">>, <<"ProtocolCore.BasicTypeEnum.Double">>}
   , {<<"string">>, <<"string">>, <<"ProtocolCore.BasicTypeEnum.String">>}
   , {<<"binary">>, <<"byte[]">>, <<"ProtocolCore.BasicTypeEnum.Binary">>}
]).

parseMapType(<<"map<", Rest/binary>>) ->
   {KeyType, <<",", AfterComma/binary>>} = takeType(Rest),
   {ValType, <<">", _/binary>>} = takeTypeVal(AfterComma),
   {KeyType, ValType}.

takeTypeVal(Bin) ->
   takeTypeClose(Bin, 0, <<>>, $>).

takeType(Bin) ->
   takeTypeClose(Bin, 0, <<>>, $]).

takeTypeClose(<<>>, 0, Acc, _Close) ->
   {Acc, <<>>};
takeTypeClose(<<",", Rest/binary>>, 0, Acc, $]) when Acc =/= <<>> ->
   {Acc, <<",", Rest/binary>>};
takeTypeClose(<<"]", Rest/binary>>, 0, Acc, $]) when Acc =/= <<>> ->
   {Acc, <<"]", Rest/binary>>};
takeTypeClose(<<">", Rest/binary>>, 0, Acc, $>) when Acc =/= <<>> ->
   {Acc, <<">", Rest/binary>>};
takeTypeClose(<<"[", Rest/binary>>, Depth, Acc, Close) ->
   takeTypeClose(Rest, Depth + 1, <<Acc/binary, "[">>, Close);
takeTypeClose(<<"<", Rest/binary>>, Depth, Acc, Close) ->
   takeTypeClose(Rest, Depth + 1, <<Acc/binary, "<">>, Close);
takeTypeClose(<<"]", Rest/binary>>, Depth, Acc, Close) when Depth > 0 ->
   takeTypeClose(Rest, Depth - 1, <<Acc/binary, "]">>, Close);
takeTypeClose(<<">", Rest/binary>>, Depth, Acc, Close) when Depth > 0 ->
   takeTypeClose(Rest, Depth - 1, <<Acc/binary, ">">>, Close);
takeTypeClose(<<C, Rest/binary>>, Depth, Acc, Close) ->
   takeTypeClose(Rest, Depth, <<Acc/binary, C>>, Close).

csType(TypeStr) ->
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, CSType, _} ->
         CSType;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               <<"List<", (csType(SubTypeStr))/binary, ">">>;
            <<"map<", _/binary>> ->
               {K, V} = parseMapType(TypeStr),
               <<"Dictionary<", (csType(K))/binary, ", ", (csType(V))/binary, ">">>;
            _ ->
               TypeStr
         end
   end.

csIdent(NameStr) ->
   case is_cs_keyword(NameStr) of
      true -> <<"@", NameStr/binary>>;
      false -> NameStr
   end.

is_cs_keyword(<<"bool">>) -> true;
is_cs_keyword(<<"float">>) -> true;
is_cs_keyword(<<"double">>) -> true;
is_cs_keyword(<<"int">>) -> true;
is_cs_keyword(<<"uint">>) -> true;
is_cs_keyword(<<"long">>) -> true;
is_cs_keyword(<<"ulong">>) -> true;
is_cs_keyword(<<"short">>) -> true;
is_cs_keyword(<<"ushort">>) -> true;
is_cs_keyword(<<"byte">>) -> true;
is_cs_keyword(<<"sbyte">>) -> true;
is_cs_keyword(<<"string">>) -> true;
is_cs_keyword(<<"object">>) -> true;
is_cs_keyword(<<"void">>) -> true;
is_cs_keyword(_) -> false.

builtMemberStr({oneof, NameStr, _Branches}) ->
   Id = csIdent(NameStr),
   <<"\t\tpublic (byte tag, object value)? ", Id/binary, ";\n">>;
builtMemberStr({TypeStr, NameStr}) ->
   Id = csIdent(NameStr),
   <<"\t\tpublic ", (csType(TypeStr))/binary, " ", Id/binary, ";\n">>.

builtEncodeStr({oneof, NameStr, Branches}) ->
   Id = csIdent(NameStr),
   Cases = buildOneofEncode(Branches, 1, Id, <<>>),
   <<"\t\t\tif (", Id/binary, " == null || ", Id/binary, ".Value.tag == 0)\n\t\t\t{\n\t\t\t\tbinaryWriter.Write((byte)0);\n\t\t\t}\n\t\t\telse\n\t\t\t{\n\t\t\t\tswitch (", Id/binary, ".Value.tag)\n\t\t\t\t{\n", Cases/binary, "\t\t\t\t\tdefault: throw new InvalidOperationException(\"invalid oneof tag\");\n\t\t\t\t}\n\t\t\t}\n">>;
builtEncodeStr({TypeStr, NameStr}) ->
   Id = csIdent(NameStr),
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, _, _} ->
         <<"\t\t\tbinaryWriter.WriteValue(", Id/binary, ");\n">>;
      _ ->
         case TypeStr of
            <<"list[", _/binary>> ->
               <<"\t\t\tbinaryWriter.WriteValue(", Id/binary, ");\n">>;
            <<"map<", _/binary>> ->
               {KType, VType} = parseMapType(TypeStr),
               <<"\t\t\tbinaryWriter.Write((ushort)(", Id/binary, "?.Count ?? 0));\n",
                 "\t\t\tif (", Id/binary, " != null)\n\t\t\t{\n",
                 "\t\t\t\tforeach (var __kv in ", Id/binary, ")\n\t\t\t\t{\n",
                 (encodeValue(KType, <<"__kv.Key">>, <<"\t\t\t\t\t">>))/binary,
                 (encodeValue(VType, <<"__kv.Value">>, <<"\t\t\t\t\t">>))/binary,
                 "\t\t\t\t}\n\t\t\t}\n">>;
            _ ->
               <<"\t\t\tif (", Id/binary, " != null)\n\t\t\t{\n\t\t\t\tbinaryWriter.Write((byte)1);\n\t\t\t\t", Id/binary, ".Serialize(binaryWriter);\n\t\t\t}\n\t\t\telse\n\t\t\t{\n\t\t\t\tbinaryWriter.Write((byte)0);\n\t\t\t}\n">>
         end
   end.

encodeValue(TypeStr, Expr, Indent) ->
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, _, _} ->
         <<Indent/binary, "binaryWriter.WriteValue(", Expr/binary, ");\n">>;
      _ ->
         case TypeStr of
            <<"list[", _/binary>> ->
               <<Indent/binary, "binaryWriter.WriteValue(", Expr/binary, ");\n">>;
            <<"map<", _/binary>> ->
               %% rare nested map
               {_K, V} = parseMapType(TypeStr),
               <<Indent/binary, "binaryWriter.Write((ushort)(", Expr/binary, "?.Count ?? 0));\n",
                 Indent/binary, "if (", Expr/binary, " != null) foreach (var __nkv in ", Expr/binary, ") { ",
                 "binaryWriter.WriteValue(__nkv.Key); ",
                 (case lists:keyfind(V, 1, ?TypeValue) of
                     {_, _, _} -> <<"binaryWriter.WriteValue(__nkv.Value); ">>;
                     _ -> <<"__nkv.Value.Serialize(binaryWriter); ">>
                  end)/binary,
                 "}\n">>;
            _ ->
               <<Indent/binary, Expr/binary, ".Serialize(binaryWriter);\n">>
         end
   end.

buildOneofEncode([], _Tag, _Name, Acc) ->
   Acc;
buildOneofEncode([{TypeStr, _BName} | Rest], Tag, NameStr, Acc) ->
   TagBin = integer_to_binary(Tag),
   CSType = csType(TypeStr),
   Body =
      case lists:keyfind(TypeStr, 1, ?TypeValue) of
         {TypeStr, _, _} ->
            <<"\t\t\t\t\tcase ", TagBin/binary, ":\n\t\t\t\t\t\tbinaryWriter.Write((byte)", TagBin/binary, ");\n\t\t\t\t\t\tbinaryWriter.WriteValue((", CSType/binary, ")", NameStr/binary, ".Value.value);\n\t\t\t\t\t\tbreak;\n">>;
         _ ->
            case TypeStr of
               <<"list[", _/binary>> ->
                  <<"\t\t\t\t\tcase ", TagBin/binary, ":\n\t\t\t\t\t\tbinaryWriter.Write((byte)", TagBin/binary, ");\n\t\t\t\t\t\tbinaryWriter.WriteValue((", CSType/binary, ")", NameStr/binary, ".Value.value);\n\t\t\t\t\t\tbreak;\n">>;
               _ ->
                  <<"\t\t\t\t\tcase ", TagBin/binary, ":\n\t\t\t\t\t\tbinaryWriter.Write((byte)", TagBin/binary, ");\n\t\t\t\t\t\t((", TypeStr/binary, ")", NameStr/binary, ".Value.value).Serialize(binaryWriter);\n\t\t\t\t\t\tbreak;\n">>
            end
      end,
   buildOneofEncode(Rest, Tag + 1, NameStr, <<Acc/binary, Body/binary>>).

builtDecodeStr({oneof, NameStr, Branches}) ->
   Id = csIdent(NameStr),
   Cases = buildOneofDecode(Branches, 1, Id, <<>>),
   <<"\t\t\t{\n\t\t\t\tvar __tag = binaryReader.ReadByte();\n\t\t\t\tswitch (__tag)\n\t\t\t\t{\n\t\t\t\t\tcase 0:\n\t\t\t\t\t\t", Id/binary, " = null;\n\t\t\t\t\t\tbreak;\n", Cases/binary, "\t\t\t\t\tdefault: throw new InvalidOperationException($\"invalid oneof tag: {__tag}\");\n\t\t\t\t}\n\t\t\t}\n">>;
builtDecodeStr({TypeStr, NameStr}) ->
   Id = csIdent(NameStr),
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, _, _} ->
         <<"\t\t\tbinaryReader.ReadValue(out ", Id/binary, ");\n">>;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
                  {SubTypeStr, _, TypeEnum} ->
                     <<"\t\t\tbinaryReader.ReadValue(out ", Id/binary, ", ", TypeEnum/binary, ");\n">>;
                  _ ->
                     <<"\t\t\tbinaryReader.ReadValue(out ", Id/binary, ", ProtocolCore.BasicTypeEnum.Custom);\n">>
               end;
            <<"map<", _/binary>> ->
               {KType, VType} = parseMapType(TypeStr),
               KCS = csType(KType),
               VCS = csType(VType),
               <<"\t\t\t{\n\t\t\t\tvar __cnt = binaryReader.ReadUInt16();\n\t\t\t\t", Id/binary, " = new Dictionary<", KCS/binary, ", ", VCS/binary, ">(__cnt);\n",
                 "\t\t\t\tfor (var __i = 0; __i < __cnt; __i++)\n\t\t\t\t{\n",
                 (decodeValue(KType, <<"__k">>, <<"\t\t\t\t\t">>))/binary,
                 (decodeValue(VType, <<"__v">>, <<"\t\t\t\t\t">>))/binary,
                 "\t\t\t\t\t", Id/binary, "[__k] = __v;\n\t\t\t\t}\n\t\t\t}\n">>;
            _ ->
               <<"\t\t\tif (binaryReader.ReadByte() != 0)\n\t\t\t{\n\t\t\t\t", Id/binary, " = new ", TypeStr/binary, "();\n\t\t\t\t", Id/binary, ".Deserialize(binaryReader);\n\t\t\t}\n">>
         end
   end.

decodeValue(TypeStr, Var, Indent) ->
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, CSType, _} ->
         <<Indent/binary, CSType/binary, " ", Var/binary, "; binaryReader.ReadValue(out ", Var/binary, ");\n">>;
      _ ->
         case TypeStr of
            <<"list[", _/binary>> ->
               <<Indent/binary, (csType(TypeStr))/binary, " ", Var/binary, "; binaryReader.ReadValue(out ", Var/binary, ", ProtocolCore.BasicTypeEnum.Custom);\n">>;
            _ ->
               <<Indent/binary, "var ", Var/binary, " = new ", TypeStr/binary, "(); ", Var/binary, ".Deserialize(binaryReader);\n">>
         end
   end.

buildOneofDecode([], _Tag, _Name, Acc) ->
   Acc;
buildOneofDecode([{TypeStr, _BName} | Rest], Tag, NameStr, Acc) ->
   TagBin = integer_to_binary(Tag),
   Body =
      case lists:keyfind(TypeStr, 1, ?TypeValue) of
         {TypeStr, CSType, _} ->
            <<"\t\t\t\t\tcase ", TagBin/binary, ":\n\t\t\t\t\t{\n\t\t\t\t\t\t", CSType/binary, " __ov; binaryReader.ReadValue(out __ov);\n\t\t\t\t\t\t", NameStr/binary, " = ((byte)", TagBin/binary, ", (object)__ov);\n\t\t\t\t\t\tbreak;\n\t\t\t\t\t}\n">>;
         _ ->
            case TypeStr of
               <<"list[", _/binary>> ->
                  <<"\t\t\t\t\tcase ", TagBin/binary, ":\n\t\t\t\t\t{\n\t\t\t\t\t\t", (csType(TypeStr))/binary, " __ov; binaryReader.ReadValue(out __ov, ProtocolCore.BasicTypeEnum.Custom);\n\t\t\t\t\t\t", NameStr/binary, " = ((byte)", TagBin/binary, ", (object)__ov);\n\t\t\t\t\t\tbreak;\n\t\t\t\t\t}\n">>;
               _ ->
                  <<"\t\t\t\t\tcase ", TagBin/binary, ":\n\t\t\t\t\t{\n\t\t\t\t\t\tvar __ov = new ", TypeStr/binary, "(); __ov.Deserialize(binaryReader);\n\t\t\t\t\t\t", NameStr/binary, " = ((byte)", TagBin/binary, ", (object)__ov);\n\t\t\t\t\t\tbreak;\n\t\t\t\t\t}\n">>
            end
      end,
   buildOneofDecode(Rest, Tag + 1, NameStr, <<Acc/binary, Body/binary>>).
