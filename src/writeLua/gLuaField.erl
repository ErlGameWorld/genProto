-module(gLuaField).

-compile([export_all, nowarn_export_all]).

-define(TypeValue, [
	{<<"bool">>, <<"bool">>, <<"false">>}
	, {<<"int8">>, <<"int8">>, <<"0">>}
	, {<<"uint8">>, <<"uint8">>, <<"0">>}
	, {<<"int16">>, <<"int16">>, <<"0">>}
	, {<<"uint16">>, <<"uint16">>, <<"0">>}
	, {<<"int32">>, <<"int32">>, <<"0">>}
	, {<<"uint32">>, <<"uint32">>, <<"0">>}
	, {<<"int64">>, <<"int64">>, <<"0">>}
	, {<<"uint64">>, <<"uint64">>, <<"0">>}
	, {<<"float">>, <<"float">>, <<"0">>}
	, {<<"double">>, <<"double">>, <<"0">>}
	, {<<"integer">>, <<"integer">>, <<"0">>}
	, {<<"number">>, <<"number">>, <<"0">>}
	, {<<"string">>, <<"string">>, <<"\"\"">>}
	, {<<"binary">>, <<"binary">>, <<"\"\"">>}
]).

-define(BulkListOps, [
	{<<"bool">>, <<"write_bool_list">>, <<"read_bool_list">>}
	, {<<"int8">>, <<"write_int8_list">>, <<"read_int8_list">>}
	, {<<"uint8">>, <<"write_uint8_list">>, <<"read_uint8_list">>}
	, {<<"int16">>, <<"write_int16_list">>, <<"read_int16_list">>}
	, {<<"uint16">>, <<"write_uint16_list">>, <<"read_uint16_list">>}
	, {<<"int32">>, <<"write_int32_list">>, <<"read_int32_list">>}
	, {<<"uint32">>, <<"write_uint32_list">>, <<"read_uint32_list">>}
	, {<<"int64">>, <<"write_int64_list">>, <<"read_int64_list">>}
	, {<<"uint64">>, <<"write_uint64_list">>, <<"read_uint64_list">>}
	, {<<"float">>, <<"write_float_list">>, <<"read_float_list">>}
	, {<<"double">>, <<"write_double_list">>, <<"read_double_list">>}
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

bulkListOps(TypeStr) ->
	case lists:keyfind(TypeStr, 1, ?BulkListOps) of
		{TypeStr, WriteFn, ReadFn} -> {WriteFn, ReadFn};
		_ -> error
	end.

builtMemberStr({oneof, NameStr, _Branches}) ->
	<<"\t\t", NameStr/binary, " = nil,\n">>;
builtMemberStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, _LuaTypeStr, DefValue} ->
			<<"\t\t", NameStr/binary, " = ", DefValue/binary, ",\n">>;
		_ ->
			case TypeStr of
				<<"map<", _/binary>> ->
					<<"\t\t", NameStr/binary, " = nil,\n">>;
				_ ->
					<<"\t\t", NameStr/binary, " = {},\n">>
			end
	end.

writeType(TypeStr, Expr, Indent) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, LuaTypeStr, _} ->
			<<Indent/binary, "byteArray.write_", LuaTypeStr/binary, "(", Expr/binary, ")\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					case bulkListOps(SubTypeStr) of
						{WriteFn, _} ->
							<<Indent/binary, "byteArray.write_uint16(#(", Expr/binary, "))\n",
								Indent/binary, "byteArray.", WriteFn/binary, "(", Expr/binary, ")\n">>;
						error ->
							case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
								{SubTypeStr, LuaTypeStr, _} ->
									<<Indent/binary, "byteArray.write_uint16(#(", Expr/binary, "))\n",
										Indent/binary, "for _, __v in ipairs(", Expr/binary, ") do\n",
										Indent/binary, "\tbyteArray.write_", LuaTypeStr/binary, "(__v)\n",
										Indent/binary, "end\n">>;
								_ ->
									<<Indent/binary, "byteArray.write_uint16(#(", Expr/binary, "))\n",
										Indent/binary, "for _, __v in ipairs(", Expr/binary, ") do\n",
										Indent/binary, "\tbyteArray = __v:encode(byteArray)\n",
										Indent/binary, "end\n">>
							end
					end;
				<<"map<", _/binary>> ->
					mapEncode(TypeStr, Expr, Indent);
				_ ->
					<<Indent/binary, "byteArray = ", Expr/binary, ":encode(byteArray)\n">>
			end
	end.

readType(TypeStr, Dest, Indent) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, LuaTypeStr, _} ->
			<<Indent/binary, Dest/binary, " = byteArray.read_", LuaTypeStr/binary, "()\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					case bulkListOps(SubTypeStr) of
						{_, ReadFn} ->
							<<Indent/binary, "do\n",
								Indent/binary, "\tlocal __cnt = byteArray.read_uint16()\n",
								Indent/binary, "\t", Dest/binary, " = byteArray.", ReadFn/binary, "(__cnt)\n",
								Indent/binary, "end\n">>;
						error ->
							case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
								{SubTypeStr, LuaTypeStr, _} ->
									<<Indent/binary, "do\n",
										Indent/binary, "\tlocal __cnt = byteArray.read_uint16()\n",
										Indent/binary, "\tlocal __arr = {}\n",
										Indent/binary, "\tfor __i = 1, __cnt do __arr[__i] = byteArray.read_", LuaTypeStr/binary, "() end\n",
										Indent/binary, "\t", Dest/binary, " = __arr\n",
										Indent/binary, "end\n">>;
								_ ->
									<<Indent/binary, "do\n",
										Indent/binary, "\tlocal __cnt = byteArray.read_uint16()\n",
										Indent/binary, "\tlocal __arr = {}\n",
										Indent/binary, "\tfor __i = 1, __cnt do\n",
										Indent/binary, "\t\tlocal __tmp = ", SubTypeStr/binary, "()\n",
										Indent/binary, "\t\t__tmp:decode(byteArray)\n",
										Indent/binary, "\t\t__arr[__i] = __tmp\n",
										Indent/binary, "\tend\n",
										Indent/binary, "\t", Dest/binary, " = __arr\n",
										Indent/binary, "end\n">>
							end
					end;
				<<"map<", _/binary>> ->
					{KType, VType} = parseMapType(TypeStr),
					<<Indent/binary, "do\n",
						Indent/binary, "\tlocal __cnt = byteArray.read_uint16()\n",
						Indent/binary, "\tlocal __map = {}\n",
						Indent/binary, "\tfor __i = 1, __cnt do\n",
						(readType(KType, <<"__k">>, <<Indent/binary, "\t\t">>))/binary,
						(readType(VType, <<"__v">>, <<Indent/binary, "\t\t">>))/binary,
						Indent/binary, "\t\t__map[__k] = __v\n",
						Indent/binary, "\tend\n",
						Indent/binary, "\t", Dest/binary, " = __map\n",
						Indent/binary, "end\n">>;
				_ ->
					<<Indent/binary, Dest/binary, " = ", TypeStr/binary, "()\n",
						Indent/binary, Dest/binary, ":decode(byteArray)\n">>
			end
	end.

mapEncode(TypeStr, Expr, Indent) ->
	{KType, VType} = parseMapType(TypeStr),
	<<Indent/binary, "do\n",
		Indent/binary, "\tlocal __map = ", Expr/binary, " or {}\n",
		Indent/binary, "\tlocal __keys = {}\n",
		Indent/binary, "\tlocal __n = 0\n",
		Indent/binary, "\tfor __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end\n",
		Indent/binary, "\tbyteArray.write_uint16(__n)\n",
		Indent/binary, "\tfor __i = 1, __n do\n",
		Indent/binary, "\t\tlocal __k = __keys[__i]\n",
		(writeType(KType, <<"__k">>, <<Indent/binary, "\t\t">>))/binary,
		(writeType(VType, <<Expr/binary, "[__k]">>, <<Indent/binary, "\t\t">>))/binary,
		Indent/binary, "\tend\n",
		Indent/binary, "end\n">>.

builtEncodeStr({oneof, NameStr, Branches}) ->
	Cases = buildOneofEncode(Branches, NameStr),
	<<"\tif not self.", NameStr/binary, " then\n\t\tbyteArray.write_uint8(0)\n\telse\n",
		Cases/binary,
		"\tend\n">>;
builtEncodeStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, LuaTypeStr, _} ->
			<<"\tbyteArray.write_", LuaTypeStr/binary, "(self.", NameStr/binary, ")\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					case bulkListOps(SubTypeStr) of
						{WriteFn, _} ->
							<<"\tbyteArray.write_uint16(#(self.", NameStr/binary, "))\n",
								"\tbyteArray.", WriteFn/binary, "(self.", NameStr/binary, ")\n">>;
						error ->
							case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
								{SubTypeStr, LuaTypeStr, _} ->
									<<"\tbyteArray.write_uint16(#(self.", NameStr/binary, "))\n",
										"\tfor _, v in ipairs(self.", NameStr/binary, ") do\n",
										"\t\tbyteArray.write_", LuaTypeStr/binary, "(v)\n",
										"\tend\n">>;
								_ ->
									<<"\tbyteArray.write_uint16(#(self.", NameStr/binary, "))\n",
										"\tfor _, v in ipairs(self.", NameStr/binary, ") do\n",
										"\t\tbyteArray = v:encode(byteArray)\n",
										"\tend\n">>
							end
					end;
				<<"map<", _/binary>> ->
					writeType(TypeStr, <<"self.", NameStr/binary>>, <<"\t">>);
				_ ->
					<<"\tif self.", NameStr/binary, " and next(self.", NameStr/binary, ") then\n",
						"\t\tbyteArray.write_uint8(1)\n",
						"\t\tbyteArray = self.", NameStr/binary, ":encode(byteArray)\n",
						"\telse\n\t\tbyteArray.write_uint8(0)\n\tend\n">>
			end
	end.

buildOneofEncode(Branches, NameStr) ->
	buildOneofEncode(Branches, 1, NameStr, <<>>, true).

buildOneofEncode([], _Tag, _Name, Acc, _First) ->
	<<Acc/binary, "\t\telse\n\t\t\terror(\"invalid oneof\")\n\t\tend\n">>;
buildOneofEncode([{TypeStr, _BName} | Rest], Tag, NameStr, Acc, First) ->
	TagBin = integer_to_binary(Tag),
	IfWord = case First of true -> <<"\t\tif">>; _ -> <<"\t\telseif">> end,
	Body = <<IfWord/binary, " self.", NameStr/binary, "[1] == ", TagBin/binary, " then\n",
		"\t\t\tbyteArray.write_uint8(", TagBin/binary, ")\n",
		(writeType(TypeStr, <<"self.", NameStr/binary, "[2]">>, <<"\t\t\t">>))/binary>>,
	buildOneofEncode(Rest, Tag + 1, NameStr, <<Acc/binary, Body/binary>>, false).

builtDecodeStr({oneof, NameStr, Branches}) ->
	Cases = buildOneofDecode(Branches, 1, NameStr, <<>>),
	<<"\tdo\n\t\tlocal __tag = byteArray.read_uint8()\n\t\tif __tag == 0 then\n\t\t\tself.", NameStr/binary, " = nil\n",
		Cases/binary,
		"\t\telse\n\t\t\terror(\"invalid oneof tag\")\n\t\tend\n\tend\n">>;
builtDecodeStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, LuaTypeStr, _} ->
			<<"\tself.", NameStr/binary, " = byteArray.read_", LuaTypeStr/binary, "()\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					case bulkListOps(SubTypeStr) of
						{_, ReadFn} ->
							CntName = <<"cntOf", NameStr/binary>>,
							<<"\tlocal ", CntName/binary, " = byteArray.read_uint16()\n",
								"\tself.", NameStr/binary, " = byteArray.", ReadFn/binary, "(", CntName/binary, ")\n">>;
						error ->
							case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
								{SubTypeStr, LuaTypeStr, _} ->
									CntName = <<"cntOf", NameStr/binary>>,
									<<"\tlocal ", CntName/binary, " = byteArray.read_uint16()\n",
										"\tself.", NameStr/binary, " = {}\n",
										"\tfor i = 1, ", CntName/binary, " do\n",
										"\t\tself.", NameStr/binary, "[i] = byteArray.read_", LuaTypeStr/binary, "()\n",
										"\tend\n">>;
								_ ->
									CntName = <<"cntOf", NameStr/binary>>,
									<<"\tlocal ", CntName/binary, " = byteArray.read_uint16()\n",
										"\tself.", NameStr/binary, " = {}\n",
										"\tfor i = 1, ", CntName/binary, " do\n",
										"\t\tlocal temp = ", SubTypeStr/binary, "()\n",
										"\t\t\ttemp:decode(byteArray)\n",
										"\t\tself.", NameStr/binary, "[i] = temp\n",
										"\tend\n">>
							end
					end;
				<<"map<", _/binary>> ->
					readType(TypeStr, <<"self.", NameStr/binary>>, <<"\t">>);
				_ ->
					NotNilName = <<"isNil", NameStr/binary>>,
					<<"\tlocal ", NotNilName/binary, " = byteArray.read_uint8()\n",
						"\tif ", NotNilName/binary, " > 0 then\n",
						"\t\tself.", NameStr/binary, " = ", TypeStr/binary, "()\n",
						"\t\tself.", NameStr/binary, ":decode(byteArray)\n",
						"\telse\n\t\tself.", NameStr/binary, " = {}\n\tend\n">>
			end
	end.

buildOneofDecode([], _Tag, _Name, Acc) ->
	Acc;
buildOneofDecode([{TypeStr, _BName} | Rest], Tag, NameStr, Acc) ->
	TagBin = integer_to_binary(Tag),
	Body = <<"\t\telseif __tag == ", TagBin/binary, " then\n",
		(readType(TypeStr, <<"__ov">>, <<"\t\t\t">>))/binary,
		"\t\t\tself.", NameStr/binary, " = {", TagBin/binary, ", __ov}\n">>,
	buildOneofDecode(Rest, Tag + 1, NameStr, <<Acc/binary, Body/binary>>).
