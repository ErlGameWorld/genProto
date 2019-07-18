-module(protoGen).

-export([
	convertFile/1
	, convert/1
	, convertDir/0
	, convertDir/1
	, convertDir/3
]).

protoHrlHeader() ->
"-opaque int8() :: -128..127.
-opaque int16() :: -32768..32767.
-opaque int32() :: -2147483648..2147483647.
-opaque int64() :: -9223372036854775808..9223372036854775807.
-opaque uint8() :: 0..255.
-opaque uint16() :: 0..65536.
-opaque uint32() :: 0..4294967295.
-opaque uint64() :: 0..18446744073709551615.
-opaque single() :: float().
-opaque double() :: float().\n\n".

protoErlHeader() ->
"-module(protoMsg).\n\n
-export([encode/1, decode/1, encodeRec/1, decodeRec/2]).

-define(bool(V), (case V of true -> <<1:8>>; _ -> <<0:8>> end)).
-define(int8(V), <<V:8>>).
-define(uint8(V), <<V:8>>).
-define(int16(V), <<V:16/big>>).
-define(uint16(V), <<V:16/big>>).
-define(int32(V), <<V:32/big>>).
-define(uint32(V), <<V:32/big>>).
-define(int64(V), <<V:64/big>>).
-define(uint64(V), <<V:64/big>>).
-define(float(V), <<V:32/big-float>>).
-define(double(V), <<V:64/big-float>>).
-define(string(V), (string(V))).
-define(record(V), (case V of undefined -> [<<0:8>>]; V -> [<<1:8>>, encodeRec(V)] end)).
-define(list_bool(List), [<<(length(List)):16/big>>, [?bool(V) || V <- List]]).
-define(list_int8(List), [<<(length(List)):16/big>>, [?int8(V) || V <- List]]).
-define(list_uint8(List), [<<(length(List)):16/big>>, [?uint8(V) || V <- List]]).
-define(list_int16(List), [<<(length(List)):16/big>>, [?int16(V) || V <- List]]).
-define(list_uint16(List), [<<(length(List)):16/big>>, [?uint16(V) || V <- List]]).
-define(list_int32(List), [<<(length(List)):16/big>>, [?int32(V) || V <- List]]).
-define(list_uint32(List), [<<(length(List)):16/big>>, [?uint32(V) || V <- List]]).
-define(list_int64(List), [<<(length(List)):16/big>>, [?int64(V) || V <- List]]).
-define(list_uint64(List), [<<(length(List)):16/big>>, [?uint64(V) || V <- List]]).
-define(list_float(List), [<<(length(List)):16/big>>, [?float(V) || V <- List]]).
-define(list_double(List), [<<(length(List)):16/big>>, [?double(V) || V <- List]]).
-define(list_string(List), [<<(length(List)):16/big>>, [string(V) || V <- List]]).
-define(list_record(List), [<<(length(List)):16/big>>, [encodeRec(V) || V <- List]]).

string(Str) when is_binary(Str) ->
	[<<(byte_size(Str)):16/big>>, Str];
string(Str) ->
	Str2 = unicode:characters_to_binary(Str, utf8),
	[<<(byte_size(Str2)):16/big>>, Str2].

encode(Record) ->
	MsgBin = encodeRec(Record),
	MsgId = getMsgId(element(1, Record)),
	[<<MsgId:16/big>>, MsgBin].

decode(Bin) ->
	<<MsgId:16/big, MsgBin/binary>> = Bin,
	SchList = getMsgSchema(MsgId),
	{<<>>, ResultList} = decodeField(SchList, MsgBin, [getMsgType(MsgId)]),
	list_to_tuple(ResultList).

decodeRec(RecordName, Bin) ->
	SchList = getMsgSchema(RecordName),
	{LeftBin, Result} = decodeField(SchList, Bin, [RecordName]),
	{LeftBin, list_to_tuple(Result)}.

decodeField([], LeftBin, Result) ->
	{LeftBin, lists:reverse(Result)};
decodeField([Type | SchList], MsgBin, Result) ->
	case Type of
		int32 ->
			<<Int:32/big-signed, LeftBin/binary>> = MsgBin,
			decodeField(SchList, LeftBin, [Int | Result]);
		uint32 ->
			<<Int:32/big-unsigned, LeftBin/binary>> = MsgBin,
			decodeField(SchList, LeftBin, [Int | Result]);
		string ->
			<<Len:16/big, StrBin:Len/binary, LeftBin/binary>> = MsgBin,
			decodeField(SchList, LeftBin, [StrBin | Result]);
		int16 ->
			<<Int:16/big-signed, LeftBin/binary>> = MsgBin,
			decodeField(SchList, LeftBin, [Int | Result]);
		uint16 ->
			<<Int:16/big-unsigned, LeftBin/binary>> = MsgBin,
			decodeField(SchList, LeftBin, [Int | Result]);
		int8 ->
			<<Int:8/big-signed, LeftBin/binary>> = MsgBin,
			decodeField(SchList, LeftBin, [Int | Result]);
		uint8 ->
			<<Int:8/big-unsigned, LeftBin/binary>> = MsgBin,
			decodeField(SchList, LeftBin, [Int | Result]);
		int64 ->
			<<Int:64/big-signed, LeftBin/binary>> = MsgBin,
			decodeField(SchList, LeftBin, [Int | Result]);
		uint64 ->
			<<Int:64/big-unsigned, LeftBin/binary>> = MsgBin,
			decodeField(SchList, LeftBin, [Int | Result]);
		bool ->
			<<Bool:8/big-unsigned, LeftBin/binary>> = MsgBin,
			case Bool =:= 1 of
				true ->
					decodeField(SchList, LeftBin, [true | Result]);
				_ ->
					decodeField(SchList, LeftBin, [false | Result])
			end;
		falat ->
			<<Float:32/big-float, LeftBin/binary>> = MsgBin,
			decodeField(SchList, LeftBin, [Float | Result]);
		double ->
			<<Float:64/big-float, LeftBin/binary>> = MsgBin,
			decodeField(SchList, LeftBin, [Float | Result]);
		{list, int32} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deInt32List(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, uint32} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deUint32List(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, int16} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deInt16List(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, uint16} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deUint16List(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, int8} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deInt8List(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, uint8} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deUint8List(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, string} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deStringList(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, int64} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deInt64List(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, uint64} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deUint64List(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, bool} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deBoolList(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, flat} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deFloatList(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, double} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deDoubleList(Len, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
		{list, RecordName} ->
			<<Len:16/big, LeftListBin/binary>> = MsgBin,
			{LeftBin, RetList} = deRecordList(Len, RecordName, LeftListBin, []),
			decodeField(SchList, LeftBin, [RetList | Result]);
      RecordName ->
          <<IsUndef:8, LeftBin/binary>> = MsgBin,
          case IsUndef of
              0 ->
                  decodeField(SchList, LeftBin, [undefined | Result]);
              _ ->
                  SubSchList = getMsgSchema(RecordName),
                  {SubLeftBin, SubResultList} = decodeField(SubSchList, LeftBin, [RecordName]),
                  decodeField(SchList, SubLeftBin, [list_to_tuple(SubResultList) | Result])
          end
	end.

deBoolList(0, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deBoolList(N, <<Bool:8, LeftBin/binary>>, RetList) ->
	case Bool =:= 1 of
		true ->
			deBoolList(N - 1, LeftBin, [true | RetList]);
		_ ->
			deBoolList(N - 1, LeftBin, [false | RetList])
	end.
deInt8List(0, MsgBin, RetList) ->
	{MsgBin, RetList};
deInt8List(N, <<Int:8/big-signed, LeftBin/binary>>, RetList) ->
	deInt8List(N - 1, LeftBin, [Int | RetList]).

deUint8List(0, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deUint8List(N, <<Int:8/big-unsigned, LeftBin/binary>>, RetList) ->
	deUint8List(N - 1, LeftBin, [Int | RetList]).

deInt16List(0, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deInt16List(N, <<Int:16/big-signed, LeftBin/binary>>, RetList) ->
	deInt16List(N - 1, LeftBin, [Int | RetList]).

deUint16List(0, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deUint16List(N, <<Int:16/big-unsigned, LeftBin/binary>>, RetList) ->
	deUint16List(N - 1, LeftBin, [Int | RetList]).

deInt32List(0, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deInt32List(N, <<Int:32/big-signed, LeftBin/binary>>, RetList) ->
	deInt32List(N - 1, LeftBin, [Int | RetList]).

deUint32List(0, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deUint32List(N, <<Int:32/big-unsigned, LeftBin/binary>>, RetList) ->
	deUint32List(N - 1, LeftBin, [Int | RetList]).

deInt64List(0, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deInt64List(N, <<Int:64/big-signed, LeftBin/binary>>, RetList) ->
	deInt64List(N - 1, LeftBin, [Int | RetList]).

deUint64List(0, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deUint64List(N, <<Int:64/big-unsigned, LeftBin/binary>>, RetList) ->
	deUint64List(N - 1, LeftBin, [Int | RetList]).

deFloatList(0, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deFloatList(N, <<Float:32/big-float, LeftBin/binary>>, RetList) ->
	deFloatList(N - 1, LeftBin, [Float | RetList]).

deDoubleList(0, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deDoubleList(N, <<Float:64/big-float, LeftBin/binary>>, RetList) ->
	deDoubleList(N - 1, LeftBin, [Float | RetList]).

deStringList(0, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deStringList(N, <<Len:16/big, StrBin:Len/binary-unit:8, LeftBin/binary>>, RetList) ->
	deStringList(N - 1, LeftBin, [StrBin | RetList]).

deRecordList(0, _RecordName, MsgBin, RetList) ->
	{MsgBin, lists:reverse(RetList)};
deRecordList(N, RecordName, MsgBin, RetList) ->
	{LeftBin, Tuple} = decodeRec(RecordName, MsgBin),
	deRecordList(N - 1, RecordName, LeftBin, [Tuple | RetList]).\n\n".

genSchema({FieldType, _FieldName}, AccList) ->
	SchemaStr = protoField:getSchemaInfo(FieldType),
	[SchemaStr | AccList].

genMsgHrl(FieldInfo, {Index, AccList}) ->
	TemStr =
		case Index of
			1 ->
				"";
			_ ->
				","
		end,
	RecStr = TemStr ++ protoField:builtRecStr(FieldInfo),
	{Index - 1, [RecStr | AccList]}.

genEncodeRec({MsgName, _MsgId, FieldList}) ->
	FieldLen = length(FieldList),
	FunHead =
		fun(_, {Index, StrAcc}) ->
			{Index - 1, ", V" ++ integer_to_list(Index) ++ StrAcc}
		end,
	{_, TemStr} = lists:foldr(FunHead, {FieldLen, "}) ->\n\t"}, FieldList),
	HeadStr = "encodeRec({" ++ MsgName ++ TemStr,
	
	FunBody =
		fun({FieldType, _FieldName}, {Index, PStrAcc}) ->
			TemV = "V" ++ integer_to_list(Index),
			PackStr = protoField:builtPackStr(FieldType) ++ TemV ++ ")",
			case Index == 1 of
				true ->
					{Index - 1, PackStr ++ PStrAcc};
				_ ->
					{Index - 1, ", " ++ PackStr ++ PStrAcc}
			
			end
		end,
	{_, LastStr} = lists:foldr(FunBody, {FieldLen, ""}, FieldList),
	case FieldLen > 0 of
		true ->
			HeadStr ++ "[" ++ LastStr ++ "];\n";
		_ ->
			HeadStr ++ "[];\n"
	end.

convertFile(File) ->
	protoParse:parseFile(File).

convert([ProtoDir, HrlDir, ErlDir]) ->
	convertDir(atom_to_list(ProtoDir), atom_to_list(HrlDir), atom_to_list(ErlDir)).

convertDir() ->
	convertDir("./", "./", "./").
convertDir(ProtoDir) ->
	convertDir(ProtoDir, "./", "./").
convertDir(ProtoDir, HrlDir, ErlDir) ->
	FunRead =
		fun(File, Acc) ->
			case filename:extension(File) == ".msg" of
				true ->
					io:format("Convert proto msg file: ~s ~n", [File]),
					BaseName = filename:basename(File, ".msg"),
					[ModIndex | _ModName] = re:split(BaseName, "_"),
					Index = binary_to_integer(ModIndex),
					put(messageid, Index * 1000 + 1),
					SProto = protoParse:parseFile(File),
					[SProto | Acc];
				_ ->
					Acc
			
			end
		end,
	%% 下面文件帅选并不能准确的帅选出文件名为.msg结尾的文件 在FunRead函数中纠正处理一下
	SProtoListOfList = filelib:fold_files(ProtoDir, "\\.msg", true, FunRead, []),
	SProtoList = lists:append(SProtoListOfList),
	SortedSProtoList = lists:sort(fun({_Name1, MessageId1, _FieldList1}, {_Name2, MessageId2, _FieldList2}) ->
		MessageId1 > MessageId2 end, SProtoList),
	FunSpell =
		fun({MsgName, MsgId, FieldList} = MsgInfo, {MsgHrlAcc, MsgTypeAcc, MsgIdAcc, MsgEndStr, MsgSchemaAcc}) ->
			TypeStr = "getMsgType(" ++ integer_to_list(MsgId) ++ ")->\n\t" ++ MsgName ++ ";\n",
			IdStr = "getMsgId(" ++ MsgName ++ ")->\n\t" ++ integer_to_list(MsgId) ++ ";\n",
			EncodeStr = genEncodeRec(MsgInfo),
			Len = erlang:length(FieldList),
			SchemaList = lists:foldr(fun genSchema/2, [], FieldList),
			SchemaStr = "getMsgSchema(" ++ MsgName ++ ")->\n\t" ++ "getMsgSchema(" ++ integer_to_list(MsgId) ++ ");\n" ++
				"getMsgSchema(" ++ integer_to_list(MsgId) ++ ")->\n\t" ++ io_lib:format("~p", [SchemaList]) ++ ";\n",
			{_, LastFieldStr} = lists:foldr(fun genMsgHrl/2, {Len, ""}, FieldList),
			HrlStr = "-record(" ++ MsgName ++ " ,{\n\t" ++ LastFieldStr ++ "}).\n",
			{[HrlStr | MsgHrlAcc], [TypeStr | MsgTypeAcc], [IdStr | MsgIdAcc], [EncodeStr | MsgEndStr], [SchemaStr | MsgSchemaAcc]}
		end,
	{MsgHrlStr, MsgTypeStr, MsgIdStr, MsgEndStr, MsgSchemaStr} = lists:foldl(FunSpell, {[], ["getMsgType(_) ->\n\tundefined.\n\n"], ["getMsgId(_) ->\n\t0.\n\n"], ["encodeRec(_) ->\n\t[].\n\n"], ["getMsgSchema(_) ->\n\t[].\n\n"]}, SortedSProtoList),
	
	ModStr = protoErlHeader(),
	OutputStr = ModStr ++ MsgTypeStr ++ MsgIdStr ++ MsgEndStr ++ MsgSchemaStr,
	HrlFilename = do_write_hrl(HrlDir, protoMsg, protoHrlHeader() ++ MsgHrlStr),
	ErlFilename = do_write_erl(ErlDir, protoMsg, OutputStr),
	
	io:format("protoConvert hrl dir : ~s ~n", [HrlDir]),
	io:format("protoConvert erl dir : ~s ~n", [ErlDir]),
	io:format("protoConvert to hrl file ~s succ.~n", [HrlFilename]),
	io:format("protoConvert to erl file ~s succ.~n", [ErlFilename]),
	ok.

do_write_hrl(OutDir, Mod, Str) when is_list(OutDir) ->
	Filename = filename:join([OutDir, atom_to_list(Mod) ++ ".hrl"]),
	ok = file:write_file(Filename, Str),
	Filename.

do_write_erl(OutDir, Mod, Str) when is_list(OutDir) ->
	Filename = filename:join([OutDir, atom_to_list(Mod) ++ ".erl"]),
	ok = file:write_file(Filename, Str),
	Filename.