-module(protoMsg).


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
	deRecordList(N - 1, RecordName, LeftBin, [Tuple | RetList]).

getMsgType(1)->
	test;
getMsgType(2)->
	phoneNumber;
getMsgType(3)->
	person;
getMsgType(4)->
	addressBook;
getMsgType(_) ->
	undefined.

getMsgId(test)->
	1;
getMsgId(phoneNumber)->
	2;
getMsgId(person)->
	3;
getMsgId(addressBook)->
	4;
getMsgId(_) ->
	0.

encodeRec({test, V1}) ->
	[?string(V1)];
encodeRec({phoneNumber, V1, V2}) ->
	[?record(V1), ?int32(V2)];
encodeRec({person, V1, V2, V3, V4}) ->
	[?string(V1), ?int32(V2), ?string(V3), ?list_record(V4)];
encodeRec({addressBook, V1, V2}) ->
	[?list_record(V1), ?list_record(V2)];
encodeRec(_) ->
	[].

getMsgSchema(test)->
	getMsgSchema(1);
getMsgSchema(1)->
	[string];
getMsgSchema(phoneNumber)->
	getMsgSchema(2);
getMsgSchema(2)->
	[test,int32];
getMsgSchema(person)->
	getMsgSchema(3);
getMsgSchema(3)->
	[string,int32,string,{list,phoneNumber}];
getMsgSchema(addressBook)->
	getMsgSchema(4);
getMsgSchema(4)->
	[{list,person},{list,person}];
getMsgSchema(_) ->
	[].

