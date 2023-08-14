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

-define(CustomTypeEnum, <<"ProtocolCore.BasicTypeEnum.Custom">>).
-define(TypeValue, [
   {<<"bool">>, <<"bool">>, <<"ProtocolCore.BasicTypeEnum.Boolean">>}
   , {<<"int8">>, <<"sbyte">>, <<"ProtocolCore.BasicTypeEnum.Int8">>}
   , {<<"uint8">>, <<"byte">>, <<"ProtocolCore.BasicTypeEnum.UInt8">>}
   , {<<"int16">>, <<"short">>, <<"ProtocolCore.BasicTypeEnum.Int16">>}
   , {<<"uint16">>, <<"ushort">>, <<"ProtocolCore.BasicTypeEnum.UInt16">>}
   , {<<"int32">>, <<"int">>, <<"ProtocolCore.BasicTypeEnum.Int32">>}
   , {<<"uint32">>, <<"uint">>, <<"ProtocolCore.BasicTypeEnum.UInt32">>}
   , {<<"int64">>, <<"long">>, <<"ProtocolCore.BasicTypeEnum.Int64">>}
   , {<<"uint64">>, <<"ulong">>, <<"ProtocolCore.BasicTypeEnum.UInt64">>}
   , {<<"float">>, <<"float">>, <<"ProtocolCore.BasicTypeEnum.Float">>}
   , {<<"double">>, <<"double">>, <<"ProtocolCore.BasicTypeEnum.Double">>}
   , {<<"string">>, <<"string">>, <<"ProtocolCore.BasicTypeEnum.String">>}
]).

builtMemberStr({TypeStr, NameStr}) ->
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, CSTypeStr, _TypeEnumVale} ->
         <<"\t\tpublic ", CSTypeStr/binary, " ", NameStr/binary, ";\n">>;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
                  {SubTypeStr, SubCSTypeStr, _TypeEnumVale} ->
                     <<"\t\tpublic List<", SubCSTypeStr/binary, "> ", NameStr/binary, ";\n">>;
                  _ ->
                     <<"\t\tpublic List<", SubTypeStr/binary, "> ", NameStr/binary, ";\n">>
               end;
            _ ->
               <<"\t\tpublic ", TypeStr/binary, " ", NameStr/binary, ";\n">>
         end
   end.

builtEncodeStr({TypeStr, NameStr}) ->
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, _CSTypeStr, _TypeEnumVale} ->
         <<"\t\t\tbinaryWriter.WriteValue(", NameStr/binary, ");\n">>;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
                  {SubTypeStr, _CSTypeStr, _TypeEnumVale} ->
                     <<"\t\t\tbinaryWriter.WriteValue(", NameStr/binary, ");\n">>;
                  _ ->
                     <<"\t\t\tbinaryWriter.WriteValue(", NameStr/binary, ");\n">>
               end;
            _ ->
               <<"\t\t\tif (", NameStr/binary, " != null)\n\t\t\t{\n\t\t\t\tbinaryWriter.WriteValue(1);\n\t\t\t\tbinaryWriter.WriteValue(", NameStr/binary, ");\n\t\t\t}\n\t\t\telse\n\t\t\t{\n\t\t\t\tbinaryWriter.WriteValue(0);\n\t\t\t}\n">>
         end
   end.

builtDecodeStr({TypeStr, NameStr}) ->
   case lists:keyfind(TypeStr, 1, ?TypeValue) of
      {TypeStr, _CSTypeStr, _TypeEnumVale} ->
         <<"\t\t\tbinaryReader.ReadValue(out ", NameStr/binary, ");\n">>;
      _ ->
         case TypeStr of
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
                  {SubTypeStr, _CSTypeStr, TypeEnumVale} ->
                     <<"\t\t\tbinaryReader.ReadValue(out ", NameStr/binary, ", ", TypeEnumVale/binary, ");\n">>;
                  _ ->
                     <<"\t\t\tbinaryReader.ReadValue(out ", NameStr/binary,", ", (?CustomTypeEnum)/binary, ");\n">>
               end;
            _ ->
               <<"\t\t\tif (binaryReader.ReadBoolean())\n\t\t\t{\n\t\t\t\tbinaryReader.ReadValue(out ", NameStr/binary, ");\n\t\t\t}\n">>
         end
   end.
