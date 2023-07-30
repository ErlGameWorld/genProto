-module(gLuaField).

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
	, {<<"string">>, <<"string">>, <<"\"\"">>}
]).

builtMemberStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, _LuaTypeStr, DefValue} ->
			<<"\tt.", NameStr/binary, " = ", DefValue/binary, "\n">>;
		_ ->
			<<"\tt.", NameStr/binary, " = {}", "\n">>
	end.

builtEncodeStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, LuaTypeStr, _DefValue} ->
			<<"\t\tbyteArray.write_", LuaTypeStr/binary, "(tb.", NameStr/binary, ")\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
						{SubTypeStr, LuaTypeStr, _DefValue} ->
							<<"\t\tbyteArray.write_uint16(#(tb.", NameStr/binary, "))\n",
								"\t\tfor k, v in pairs (tb.", NameStr/binary, ") do\n",
								"\t\t\tbyteArray.write_", LuaTypeStr/binary, "(v)\n",
								"\t\tend\n">>;
						_ ->
							<<"\t\tbyteArray.write_uint16(#(tb.", NameStr/binary, "))\n",
								"\t\tfor k, v in pairs(tb.", NameStr/binary, ") do\n",
								"\t\t\tbyteArray = v.encode(byteArray)\n"
								"\t\tend\n">>
					end;
				_ ->
					<<"\t\tif tb.", NameStr/binary, " and next(tb.", NameStr/binary, ") then\n\t\t\tbyteArray.write_uint8(1)\n\t\t\ttb.", NameStr/binary, ".encode(byteArray)\n\t\telse\n\t\t\tbyteArray.write_uint8(0)\n\t\tend\n">>
			end
	end.

builtDecodeStr({TypeStr, NameStr}) ->
	case lists:keyfind(TypeStr, 1, ?TypeValue) of
		{TypeStr, LuaTypeStr, _DefValue} ->
			<<"\t\ttb.", NameStr/binary, " = byteArray.read_", LuaTypeStr/binary, "()\n">>;
		_ ->
			case TypeStr of
				<<"list[", LeftStr/binary>> ->
					[SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
					case lists:keyfind(SubTypeStr, 1, ?TypeValue) of
						{SubTypeStr, LuaTypeStr, _DefValue} ->
							CntName = <<"cntOf", NameStr/binary>>,
							ReadCnt = <<"\t\tlocal ", CntName/binary, " = byteArray.read_uint16()\n\t\ttb.", NameStr/binary, " = {}\n">>,
							<<ReadCnt/binary, "\t\tfor i = 1, ", CntName/binary, " do\n\t\t\ttable.insert(tb.", NameStr/binary, ", byteArray.read_", LuaTypeStr/binary, "())\n\t\tend\n">>;
						_ ->
							CntName = <<"cntOf", NameStr/binary>>,
							ReadCnt = <<"\t\tlocal ", CntName/binary, " = byteArray.read_uint16()\n\t\ttb.", NameStr/binary, " = {}\n">>,
							<<ReadCnt/binary, "\t\tfor i = 1, ", CntName/binary, " do\n\t\t\tlocal temp = ", SubTypeStr/binary, "()\n\t\t\ttemp.decode(byteArray)\n\t\t\ttable.insert(tb.", NameStr/binary, ", temp)\n\t\tend\n">>
					end;
				_ ->
					NotNilName = <<"isNil", NameStr/binary>>,
					NotNil = <<"\t\tlocal ", NotNilName/binary, " = byteArray.read_uint8()\n">>,
					ReadTable = <<"\t\t\ttb.", NameStr/binary, " = ", TypeStr/binary, "()\n\t\t\ttb.", NameStr/binary, ".decode(byteArray)\n">>,
					<<NotNil/binary, "\t\tif ", NotNilName/binary, " > 0 then\n", ReadTable/binary, "\t\telse\n\t\t\ttb.", NameStr/binary, " = {}\n\t\tend\n">>
			end
	end.

