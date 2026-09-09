-module(gErlField).

-compile([export_all, nowarn_export_all]).

-define(SimpleList, [
   <<"int8">>
   , <<"uint8">>
   , <<"int16">>
   , <<"uint16">>
   , <<"int32">>
   , <<"uint32">>
   , <<"int64">>
   , <<"uint64">>
   , <<"float">>
   , <<"double">>
]).

-define(TypeList, [
   <<"bool">>
   , <<"int8">>
   , <<"uint8">>
   , <<"int16">>
   , <<"uint16">>
   , <<"int32">>
   , <<"uint32">>
   , <<"int64">>
   , <<"uint64">>
   , <<"integer">>
   , <<"number">>
   , <<"float">>
   , <<"double">>
   , <<"string">>
   , <<"binary">>
]).

-define(TypeValue, [
   {<<"bool">>, <<"false">>, <<"boolean()">>}
   , {<<"int8">>, <<"0">>, <<"int8()">>}
   , {<<"uint8">>, <<"0">>, <<"uint8()">>}
   , {<<"int16">>, <<"0">>, <<"int16()">>}
   , {<<"uint16">>, <<"0">>, <<"uint16()">>}
   , {<<"int32">>, <<"0">>, <<"int32()">>}
   , {<<"uint32">>, <<"0">>, <<"uint32()">>}
   , {<<"int64">>, <<"0">>, <<"int64()">>}
   , {<<"uint64">>, <<"0">>, <<"uint64()">>}
   , {<<"integer">>, <<"0">>, <<"integer()">>}
   , {<<"number">>, <<"0">>, <<"number()">>}
   , {<<"float">>, <<"0.0">>, <<"float()">>}
   , {<<"double">>, <<"0.0">>, <<"double()">>}
   , {<<"string">>, <<"\"\"">>, <<"string()">>}
   , {<<"binary">>, <<"<<>>">>, <<"binary()">>}
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

mapKVVars(<<"V", Num/binary>>) ->
   {<<"MK", Num/binary>>, <<"MV", Num/binary>>};
mapKVVars(NumBin) ->
   {<<"MK", NumBin/binary>>, <<"MV", NumBin/binary>>}.

isMapType(<<"map<", _/binary>>) ->
   true;
isMapType(_) ->
   false.

mapPackBody(<<"map<", _/binary>> = MapType, Var) ->
   {KType, VType} = parseMapType(MapType),
   {MK, MV} = mapKVVars(Var),
   KExpr = innerPackExpr(KType, MK),
   VExpr = innerPackExpr(VType, MV),
   <<"(case ", Var/binary, " of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(", Var/binary, ")):16/big>>, [ [", KExpr/binary, ", ", VExpr/binary, "] || ", MK/binary, " := ", MV/binary, " <- ", Var/binary, "]] end)">>.

erlTypeSpec(TypeStr) ->
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, _, Spec} ->
         Spec;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               <<"[", (erlTypeSpec(SubTypeStr))/binary, "]">>;
            <<"map<", _/binary>> ->
               {KType, VType} = parseMapType(TypeStr),
               <<"undefined | #{", (erlTypeSpec(KType))/binary, " => ", (erlTypeSpec(VType))/binary, "}">>;
            _ ->
               <<"#", TypeStr/binary, "{}">>
         end
   end.

builtRecStr({oneof, NameStr, Branches}) ->
   BranchSpecs = buildOneofSpec(Branches, <<>>),
   <<NameStr/binary, " = undefined :: undefined", BranchSpecs/binary, "\n">>;
builtRecStr({TypeStr, NameStr}) ->
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, DefValueStr, DefTypeStr} ->
         <<NameStr/binary, " = ", DefValueStr/binary, " :: ", DefTypeStr/binary, "\n">>;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               <<NameStr/binary, " = [] :: ", (erlTypeSpec(<<"list[", LeftStr/binary>>))/binary, "\n">>;
            <<"map<", _/binary>> ->
               <<NameStr/binary, " = undefined :: ", (erlTypeSpec(TypeStr))/binary, "\n">>;
            _ ->
               <<NameStr/binary, " = undefined :: #", TypeStr/binary, "{}\n">>
         end
   end.

buildOneofSpec([], Acc) ->
   Acc;
buildOneofSpec([{TypeStr, BranchName} | Rest], Acc) ->
   Spec = <<" | {", BranchName/binary, ", ", (erlTypeSpec(TypeStr))/binary, "}">>,
   buildOneofSpec(Rest, <<Acc/binary, Spec/binary>>).

%% 返回完整的编码表达式（不再依赖外层补 ")"）
builtPackExpr({oneof, _Name, Branches}, Var) ->
   Cases = buildOneofEncodeCases(Branches, 1, <<>>),
   <<"(case ", Var/binary, " of undefined -> <<0:8>>; ", Cases/binary, "_ -> throw({invalid_oneof, ", Var/binary, "}) end)">>;
builtPackExpr({TypeStr, _Name}, Var) ->
   case lists:member(TypeStr, ?TypeList) of
      true ->
         <<"?", TypeStr/binary, "(", Var/binary, ")">>;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               SubStr =
                  case lists:member(SubTypeStr, ?TypeList) of
                     true ->
                        SubTypeStr;
                     _ ->
                        <<"record">>
                  end,
               <<"?list_", SubStr/binary, "(", Var/binary, ")">>;
            <<"map<", _/binary>> = MapType ->
               mapPackBody(MapType, Var);
            _ ->
               <<"?record(", Var/binary, ")">>
         end
   end.

innerPackExpr(TypeStr, Var) ->
   case lists:member(TypeStr, ?TypeList) of
      true ->
         <<"?", TypeStr/binary, "(", Var/binary, ")">>;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               SubStr =
                  case lists:member(SubTypeStr, ?TypeList) of
                     true ->
                        SubTypeStr;
                     _ ->
                        <<"record">>
                  end,
               <<"?list_", SubStr/binary, "(", Var/binary, ")">>;
            <<"map<", _/binary>> ->
               mapPackBody(TypeStr, Var);
            _ ->
               %% record 在 map/oneof/list 内不加 presence
               <<"subEncode(", Var/binary, ")">>
         end
   end.

buildOneofEncodeCases([], _Tag, Acc) ->
   Acc;
buildOneofEncodeCases([{TypeStr, BranchName} | Rest], Tag, Acc) ->
   TagBin = integer_to_binary(Tag),
   OvVar = <<"OV", TagBin/binary>>,
   Pack = innerPackExpr(TypeStr, OvVar),
   Case = <<"{", BranchName/binary, ", ", OvVar/binary, "} -> [<<", TagBin/binary, ":8>>, ", Pack/binary, "]; ">>,
   buildOneofEncodeCases(Rest, Tag + 1, <<Acc/binary, Case/binary>>).

builtPackStr(TypeStr) ->
   case lists:member(TypeStr, ?TypeList) of
      true ->
         <<"?", TypeStr/binary, "(">>;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               SubStr =
                  case lists:member(SubTypeStr, ?TypeList) of
                     true ->
                        SubTypeStr;
                     _ ->
                        <<"record">>
                  end,
               <<"?list_", SubStr/binary, "(">>;
            _ ->
               <<"?record(">>
         end
   end.

isBaseType(TypeStr) ->
   lists:member(TypeStr, ?TypeList).
