-module(gTsField).

-compile([export_all, nowarn_export_all]).

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
	, {<<"binary">>, <<"Uint8Array">>, <<"new Uint8Array()">>}
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

tsType(TypeStr) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, TsType, _} ->
			TsType;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					<<(tsType(SubTypeStr))/binary, "[]">>;
				<<"map<", _/binary>> ->
					{K, V} = parseMapType(TypeStr),
					<<"Map<", (tsType(K))/binary, ", ", (tsType(V))/binary, ">">>;
				_ ->
					TypeStr
			end
	end.

builtMemberStr({oneof, NameStr, _Branches}) ->
	<<"\t\tpublic ", NameStr/binary, ": [number, any] | null = null;\n">>;
builtMemberStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, TsType, DefValue} ->
			<<"\t\tpublic ", NameStr/binary, ": ", TsType/binary, " = ", DefValue/binary, ";\n">>;
		_ ->
			case TypeStr of
				<<"list[", _/binary>> ->
					<<"\t\tpublic ", NameStr/binary, ": ", (tsType(TypeStr))/binary, " = [];\n">>;
				<<"map<", _/binary>> ->
					<<"\t\tpublic ", NameStr/binary, ": ", (tsType(TypeStr))/binary, " | null = null;\n">>;
				_ ->
					<<"\t\tpublic ", NameStr/binary, ": ", TypeStr/binary, " | null = null;\n">>
			end
	end.

writeExpr(TypeStr, Expr, Indent) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, _, _} ->
			<<Indent/binary, "byteArray.write_", TypeStr/binary, "(", Expr/binary, ");\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
						{SubTypeStr, _, _} ->
							<<Indent/binary, "byteArray.write_list(", Expr/binary, ", (item) => byteArray.write_", SubTypeStr/binary, "(item));\n">>;
						_ ->
							<<Indent/binary, "byteArray.write_list(", Expr/binary, ", (item) => item.encode(byteArray));\n">>
					end;
				<<"map<", _/binary>> ->
					{KType, VType} = parseMapType(TypeStr),
					<<Indent/binary, "byteArray.write_map(", Expr/binary, ", (k) => {\n",
						(writeExpr(KType, <<"k">>, <<Indent/binary, "\t">>))/binary,
						Indent/binary, "}, (v) => {\n",
						(writeExpr(VType, <<"v">>, <<Indent/binary, "\t">>))/binary,
						Indent/binary, "});\n">>;
				_ ->
					<<Indent/binary, Expr/binary, ".encode(byteArray);\n">>
			end
	end.

readExpr(TypeStr, Dest, Indent) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, _, _} ->
			<<Indent/binary, Dest/binary, " = byteArray.read_", TypeStr/binary, "();\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
						{SubTypeStr, _, _} ->
							<<Indent/binary, Dest/binary, " = byteArray.read_list(() => byteArray.read_", SubTypeStr/binary, "());\n">>;
						_ ->
							<<Indent/binary, Dest/binary, " = byteArray.read_list(() => {\n",
								Indent/binary, "\tconst item = new ", SubTypeStr/binary, "();\n",
								Indent/binary, "\titem.decode(byteArray);\n",
								Indent/binary, "\treturn item;\n",
								Indent/binary, "});\n">>
					end;
				<<"map<", _/binary>> ->
					{KType, VType} = parseMapType(TypeStr),
					<<Indent/binary, Dest/binary, " = byteArray.read_map(() => {\n",
						Indent/binary, "\tlet __k: any;\n",
						(readExpr(KType, <<"__k">>, <<Indent/binary, "\t">>))/binary,
						Indent/binary, "\treturn __k;\n",
						Indent/binary, "}, () => {\n",
						Indent/binary, "\tlet __v: any;\n",
						(readExpr(VType, <<"__v">>, <<Indent/binary, "\t">>))/binary,
						Indent/binary, "\treturn __v;\n",
						Indent/binary, "});\n">>;
				_ ->
					<<Indent/binary, Dest/binary, " = new ", TypeStr/binary, "();\n",
						Indent/binary, Dest/binary, ".decode(byteArray);\n">>
			end
	end.

builtEncodeStr({oneof, NameStr, Branches}) ->
	Cases = buildOneofEncode(Branches, 1, NameStr, <<>>),
	<<"\t\t\tif (!this.", NameStr/binary, ") {\n\t\t\t\tbyteArray.write_uint8(0);\n\t\t\t} else {\n",
		"\t\t\t\tswitch (this.", NameStr/binary, "[0]) {\n", Cases/binary,
		"\t\t\t\t\tdefault: throw new Error('invalid oneof');\n\t\t\t\t}\n\t\t\t}\n">>;
builtEncodeStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, _, _} ->
			<<"\t\t\tbyteArray.write_", TypeStr/binary, "(this.", NameStr/binary, ");\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
						{SubTypeStr, _, _} ->
							<<"\t\t\tbyteArray.write_list(this.", NameStr/binary, ", (item) => byteArray.write_", SubTypeStr/binary, "(item));\n">>;
						_ ->
							<<"\t\t\tbyteArray.write_list(this.", NameStr/binary, ", (item) => item.encode(byteArray));\n">>
					end;
				<<"map<", _/binary>> ->
					writeExpr(TypeStr, <<"this.", NameStr/binary>>, <<"\t\t\t">>);
				_ ->
					<<"\t\t\tif (this.", NameStr/binary, ") {\n",
						"\t\t\t\tbyteArray.write_uint8(1);\n",
						"\t\t\t\tthis.", NameStr/binary, ".encode(byteArray);\n",
						"\t\t\t} else {\n",
						"\t\t\t\tbyteArray.write_uint8(0);\n",
						"\t\t\t}\n">>
			end
	end.

buildOneofEncode([], _Tag, _Name, Acc) ->
	Acc;
buildOneofEncode([{TypeStr, _BName} | Rest], Tag, NameStr, Acc) ->
	TagBin = integer_to_binary(Tag),
	Body = <<"\t\t\t\t\tcase ", TagBin/binary, ":\n\t\t\t\t\t\tbyteArray.write_uint8(", TagBin/binary, ");\n",
		(writeExpr(TypeStr, <<"this.", NameStr/binary, "[1]">>, <<"\t\t\t\t\t\t">>))/binary,
		"\t\t\t\t\t\tbreak;\n">>,
	buildOneofEncode(Rest, Tag + 1, NameStr, <<Acc/binary, Body/binary>>).

builtDecodeStr({oneof, NameStr, Branches}) ->
	Cases = buildOneofDecode(Branches, 1, NameStr, <<>>),
	<<"\t\t\t{\n\t\t\t\tconst __tag = byteArray.read_uint8();\n\t\t\t\tswitch (__tag) {\n",
		"\t\t\t\t\tcase 0:\n\t\t\t\t\t\tthis.", NameStr/binary, " = null;\n\t\t\t\t\t\tbreak;\n",
		Cases/binary,
		"\t\t\t\t\tdefault: throw new Error(`invalid oneof tag: ${__tag}`);\n\t\t\t\t}\n\t\t\t}\n">>;
builtDecodeStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, _, _} ->
			<<"\t\t\tthis.", NameStr/binary, " = byteArray.read_", TypeStr/binary, "();\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
						{SubTypeStr, _, _} ->
							<<"\t\t\tthis.", NameStr/binary, " = byteArray.read_list(() => byteArray.read_", SubTypeStr/binary, "());\n">>;
						_ ->
							<<"\t\t\tthis.", NameStr/binary, " = byteArray.read_list(() => {\n",
								"\t\t\t\tconst item = new ", SubTypeStr/binary, "();\n",
								"\t\t\t\titem.decode(byteArray);\n",
								"\t\t\t\treturn item;\n",
								"\t\t\t});\n">>
					end;
				<<"map<", _/binary>> ->
					readExpr(TypeStr, <<"this.", NameStr/binary>>, <<"\t\t\t">>);
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

buildOneofDecode([], _Tag, _Name, Acc) ->
	Acc;
buildOneofDecode([{TypeStr, _BName} | Rest], Tag, NameStr, Acc) ->
	TagBin = integer_to_binary(Tag),
	Body = <<"\t\t\t\t\tcase ", TagBin/binary, ":\n\t\t\t\t\t{\n\t\t\t\t\t\tlet __ov: any;\n",
		(readExpr(TypeStr, <<"__ov">>, <<"\t\t\t\t\t\t">>))/binary,
		"\t\t\t\t\t\tthis.", NameStr/binary, " = [", TagBin/binary, ", __ov];\n\t\t\t\t\t\tbreak;\n\t\t\t\t\t}\n">>,
	buildOneofDecode(Rest, Tag + 1, NameStr, <<Acc/binary, Body/binary>>).
