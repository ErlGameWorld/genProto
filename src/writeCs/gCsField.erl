-module(gCsField).

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
]).

builtRecStr({TypeStr, NameStr}) ->
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, DefValueStr, DefTypeStr} ->
         <<NameStr/binary, " = ", DefValueStr/binary, " :: ", DefTypeStr/binary, "\n">>;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
                  {SubTypeStr, _DefSubValueStr, DefSubTypeStr} ->
                     <<NameStr/binary, " = [] :: [", DefSubTypeStr/binary, "]\n">>;
                  _ ->
                     <<NameStr/binary, " = [] :: [#", SubTypeStr/binary, "{}]\n">>
               end;
            _ ->
               <<NameStr/binary, " = undefined :: #", TypeStr/binary, "{}\n">>
         end
   end.

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






