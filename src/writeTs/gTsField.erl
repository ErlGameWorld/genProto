-module(gTsField).

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
	{<<"bool">>, <<"boolean">>, <<"false">>}
	, {<<"int8">>, <<"number">>, <<"0">>}
	, {<<"uint8">>, <<"number">>, <<"0">>}
	, {<<"int16">>, <<"number">>, <<"0">>}
	, {<<"uint16">>, <<"number">>, <<"0">>}
	, {<<"int32">>, <<"number">>, <<"0">>}
	, {<<"uint32">>, <<"number">>, <<"0">>}
	, {<<"int64">>, <<"bigint">>, <<"0n">>}
	, {<<"uint64">>, <<"bigint">>, <<"0n">>}
	, {<<"float">>, <<"number">>, <<"0">>}
	, {<<"double">>, <<"number">>, <<"0">>}
	, {<<"integer">>, <<"number">>, <<"0">>}
	, {<<"number">>, <<"number">>, <<"0">>}
	, {<<"string">>, <<"string">>, <<"\"\"">>}
]).

builtMemberStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, _TsTypeStr, DefValue} ->
			<<"\t\tpublic ", NameStr/binary, ": ", _TsTypeStr/binary, " = ", DefValue/binary, ";\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
						{SubTypeStr, SubTsTypeStr, _DefValue} ->
							<<"\t\tpublic ", NameStr/binary, ": ", SubTsTypeStr/binary, "[] = [];\n">>;
						_ ->
							<<"\t\tpublic ", NameStr/binary, ": ", SubTypeStr/binary, "[] = [];\n">>
					end;
				_ ->
					<<"\t\tpublic ", NameStr/binary, ": ", TypeStr/binary, " | null = null;\n">>
			end
	end.

builtEncodeStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, _TsTypeStr, _DefValue} ->
			<<"\t\t\tbyteArray.write_", TypeStr/binary, "(this.", NameStr/binary, ");\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\]">>, [{return, binary}]),
					case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
						{SubTypeStr, _SubTsTypeStr, _DefValue} ->
							<<"\t\t\tbyteArray.write_list(this.", NameStr/binary, ", byteArray.write_", SubTypeStr/binary, ");\n">>;
						_ ->
							<<"\t\t\tbyteArray.write_list(this.", NameStr/binary, ", (item) => item.encode(byteArray));\n">>
					end;
				_ ->
					<<"\t\t\tif (this.", NameStr/binary, ") {\n",
						"\t\t\t\tbyteArray.write_uint8(1);\n",
						"\t\t\t\tthis.", NameStr/binary, ".encode(byteArray);\n",
						"\t\t\t} else {\n",
						"\t\t\t\tbyteArray.write_uint8(0);\n",
						"\t\t\t}\n">>
			end
	end.

builtDecodeStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, _TsTypeStr, _DefValue} ->
			<<"\t\t\tthis.", NameStr/binary, " = byteArray.read_", TypeStr/binary, "();\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\]">>, [{return, binary}]),
					case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
						{SubTypeStr, _SubTsTypeStr, _DefValue} ->
							<<"\t\t\tthis.", NameStr/binary, " = byteArray.read_list(byteArray.read_", SubTypeStr/binary, ");\n">>;
						_ ->
							<<"\t\t\tthis.", NameStr/binary, " = byteArray.read_list(() => {\n",
								"\t\t\t\tconst item = new ", SubTypeStr/binary, "();\n",
								"\t\t\t\titem.decode(byteArray);\n",
								"\t\t\t\treturn item;\n",
								"\t\t\t});\n">>
					end;
				_ ->
					<<"\t\t\tconst has", NameStr/binary, " = byteArray.read_uint8() > 0;\n",
						"\t\t\tif (has", NameStr/binary, ") {\n",
						"\t\t\t\tthis.", NameStr/binary, " = new ", TypeStr/binary, "();\n",
						"\t\t\t\tthis.", NameStr/binary, ".decode(byteArray);\n",
						"\t\t\t} else {\n",
						"\t\t\t\tthis.", NameStr/binary, " = null;\n",
						"\t\t\t}\n">>
			end
	end.