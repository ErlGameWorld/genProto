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

-define(TypeValue, [
   {<<"bool">>, <<"bool">>}
   , {<<"int8">>, <<"sbyte">>}
   , {<<"uint8">>, <<"byte">>}
   , {<<"int16">>, <<"short">>}
   , {<<"uint16">>, <<"ushort">>}
   , {<<"int32">>, <<"int">>}
   , {<<"uint32">>, <<"uint">>}
   , {<<"int64">>, <<"long">>}
   , {<<"uint64">>, <<"ulong">>}
   , {<<"float">>, <<"float">>}
   , {<<"double">>, <<"double">>}
   , {<<"string">>, <<"string">>}
]).

builtRecStr({TypeStr, NameStr}) ->
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, CSTypeStr} ->
         <<"\t\tpublic ", CSTypeStr/binary, " ", NameStr/binary, ";\n">>;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
                  {SubTypeStr, SubCSTypeStr} ->
                     <<"\t\tpublic List<", SubCSTypeStr/binary, "> ", NameStr/binary, ";\n">>;
                  _ ->
                     <<"\t\tpublic List<", SubTypeStr/binary, "> ", NameStr/binary, ";\n">>
               end;
            _ ->
               <<"\t\tpublic ", TypeStr/binary, " ", NameStr/binary, ";\n">>
         end
   end.
