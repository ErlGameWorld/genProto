-module(protoMsg).


-export([encode/1, decode/1, encodeRec/1, decodeRec/2]).

-define(min8, -128).
-define(max8, 127).
-define(min16, -32768).
-define(max16, 32767).
-define(min32, -2147483648).
-define(max32, 2147483647).
-define(min64, -9223372036854775808).
-define(max64, 9223372036854775807).

-define(minF32, -3.4E+38).
-define(maxF32, 3.4E+38).
-define(minF64, -1.797E-308).
-define(maxF64, 1.797E+308).

-define(int8(V), <<V:8>>).
-define(uint8(V), <<V:8>>).
-define(int16(V), <<V:16/big>>).
-define(uint16(V), <<V:16/big>>).
-define(int32(V), <<V:32/big>>).
-define(uint32(V), <<V:32/big>>).
-define(int64(V), <<V:64/big>>).
-define(uint64(V), <<V:64/big>>).
-define(integer(V), (integer(V))).
-define(number(V), (number(V))).
-define(string(V), (string(V))).
-define(float(V), <<V:32/big-float>>).
-define(double(V), <<V:64/big-float>>).
-define(bool(V), (case V of true -> <<1:8>>; _ -> <<0:8>> end)).
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
-define(list_integer(List), [<<(length(List)):16/big>>, [integer(V) || V <- List]]).
-define(list_number(List), [<<(length(List)):16/big>>, [number(V) || V <- List]]).
-define(list_string(List), [<<(length(List)):16/big>>, [string(V) || V <- List]]).
-define(list_record(List), [<<(length(List)):16/big>>, [encodeRec(V) || V <- List]]).

integer(V) ->
   if
      V >= ?min8 andalso V =< ?max8 ->
         <<8:8, <<V:8>>/binary>>;
      V >= ?min16 andalso V =< ?max16 ->
         <<16:8, <<V:16/big>>/binary>>;
      V >= ?min32 andalso V =< ?max32 ->
         <<32:8, <<V:32/big>>/binary>>;
      V >= ?min64 andalso V =< ?max64 ->
         <<64:8, <<V:64/big>>/binary>>;
      true ->
         throw(exceeded_the_integer)
   end.

numInteger(V) ->
   if
      V >= ?min8 andalso V =< ?max8 ->
         <<8:8, <<V:8>>/binary>>;
      V >= ?min16 andalso V =< ?max16 ->
         <<16:8, <<V:16/big>>/binary>>;
      V >= ?min32 andalso V =< ?max32 ->
         <<32:8, <<V:32/big>>/binary>>;
      V >= ?min64 andalso V =< ?max64 ->
         <<64:8, <<V:64/big>>/binary>>;
      true ->
         throw(exceeded_the_integer)
   end.

numFloat(V) ->
   if
      V >= ?minF32 andalso V =< ?maxF32 ->
         <<33:8, <<V:32/big-float>>/binary>>;
      V >= ?minF64 andalso V =< ?maxF64 ->
         <<65:8, <<V:64/big-float>>/binary>>;
      true ->
         throw(exceeded_the_float)
   end.

number(V) ->
   if
      erlang:is_integer(V) == true ->
         numInteger(V);
      erlang:is_float(V) == true ->
         numFloat(V);
      true ->
         throw(is_not_number)
   end.

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
      integer ->
         <<IntBits:8, Int:IntBits/big-signed, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Int | Result]);
      number ->
         <<NumBits:8, NumBin/binary>> = MsgBin,
         case NumBits of
            33 ->
               <<Float:32/big-float, LeftBin/binary>> = NumBin,
               decodeField(SchList, LeftBin, [Float | Result]);
            65 ->
               <<Float:64/big-float, LeftBin/binary>> = NumBin,
               decodeField(SchList, LeftBin, [Float | Result]);
            _ ->
               <<Int:NumBits/big-signed, LeftBin/binary>> = NumBin,
               decodeField(SchList, LeftBin, [Int | Result])
         end;
      bool ->
         <<Bool:8/big-unsigned, LeftBin/binary>> = MsgBin,
         case Bool =:= 1 of
            true ->
               decodeField(SchList, LeftBin, [true | Result]);
            _ ->
               decodeField(SchList, LeftBin, [false | Result])
         end;
      float ->
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
      {list, integer} ->
         <<Len:16/big, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deIntegerList(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, number} ->
         <<Len:16/big, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deNumberList(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, bool} ->
         <<Len:16/big, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deBoolList(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, float} ->
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
deBoolList(N, MsgBin, RetList) ->
   <<Bool:8, LeftBin/binary>> = MsgBin,
   case Bool =:= 1 of
      true ->
         deBoolList(N - 1, LeftBin, [true | RetList]);
      _ ->
         deBoolList(N - 1, LeftBin, [false | RetList])
   end.
deInt8List(0, MsgBin, RetList) ->
   {MsgBin, RetList};
deInt8List(N, MsgBin, RetList) ->
   <<Int:8/big-signed, LeftBin/binary>> = MsgBin,
   deInt8List(N - 1, LeftBin, [Int | RetList]).

deUint8List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deUint8List(N, MsgBin, RetList) ->
   <<Int:8/big-unsigned, LeftBin/binary>> = MsgBin,
   deUint8List(N - 1, LeftBin, [Int | RetList]).

deInt16List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deInt16List(N, MsgBin, RetList) ->
   <<Int:16/big-signed, LeftBin/binary>> = MsgBin,
   deInt16List(N - 1, LeftBin, [Int | RetList]).

deUint16List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deUint16List(N, MsgBin, RetList) ->
   <<Int:16/big-unsigned, LeftBin/binary>> = MsgBin,
   deUint16List(N - 1, LeftBin, [Int | RetList]).

deInt32List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deInt32List(N, MsgBin, RetList) ->
   <<Int:32/big-signed, LeftBin/binary>> = MsgBin,
   deInt32List(N - 1, LeftBin, [Int | RetList]).

deUint32List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deUint32List(N, MsgBin, RetList) ->
   <<Int:32/big-unsigned, LeftBin/binary>> = MsgBin,
   deUint32List(N - 1, LeftBin, [Int | RetList]).

deInt64List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deInt64List(N, MsgBin, RetList) ->
   <<Int:64/big-signed, LeftBin/binary>> = MsgBin,
   deInt64List(N - 1, LeftBin, [Int | RetList]).

deUint64List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deUint64List(N, MsgBin, RetList) ->
   <<Int:64/big-unsigned, LeftBin/binary>> = MsgBin,
   deUint64List(N - 1, LeftBin, [Int | RetList]).

deIntegerList(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deIntegerList(N, MsgBin, RetList) ->
   <<IntBits:8, Int:IntBits/big-signed, LeftBin/binary>> = MsgBin,
   deIntegerList(N - 1, LeftBin, [Int | RetList]).

deNumberList(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deNumberList(N, MsgBin, RetList) ->
   <<NumBits:8, NumBin/binary>> = MsgBin,
   case NumBits of
      33 ->
         <<Float:32/big-float, LeftBin/binary>> = NumBin,
         deNumberList(N - 1, LeftBin, [Float | RetList]);
      65 ->
         <<Float:64/big-float, LeftBin/binary>> = NumBin,
         deNumberList(N - 1, LeftBin, [Float | RetList]);
      _ ->
         <<Int:NumBits/big-signed, LeftBin/binary>> = NumBin,
         deNumberList(N - 1, LeftBin, [Int | RetList])
   end.

deFloatList(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deFloatList(N, MsgBin, RetList) ->
   <<Float:32/big-float, LeftBin/binary>> = MsgBin,
   deFloatList(N - 1, LeftBin, [Float | RetList]).

deDoubleList(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deDoubleList(N, MsgBin, RetList) ->
   <<Float:64/big-float, LeftBin/binary>> = MsgBin,
   deDoubleList(N - 1, LeftBin, [Float | RetList]).

deStringList(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deStringList(N, MsgBin, RetList) ->
   <<Len:16/big, StrBin:Len/binary-unit:8, LeftBin/binary>> = MsgBin,
   deStringList(N - 1, LeftBin, [StrBin | RetList]).

deRecordList(0, _RecordName, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deRecordList(N, RecordName, MsgBin, RetList) ->
   {LeftBin, Tuple} = decodeRec(RecordName, MsgBin),
   deRecordList(N - 1, RecordName, LeftBin, [Tuple | RetList]).

getMsgType(1)-> test;
getMsgType(2)-> phoneNumber;
getMsgType(3)-> person;
getMsgType(4)-> addressBook;
getMsgType(5)-> union;
getMsgType(6)-> tbool;
getMsgType(7)-> tint8;
getMsgType(8)-> tuint8;
getMsgType(9)-> tint16;
getMsgType(10)-> tuint16;
getMsgType(11)-> tint32;
getMsgType(12)-> tuint32;
getMsgType(13)-> tint64;
getMsgType(14)-> tuint64;
getMsgType(15)-> tinteger;
getMsgType(16)-> tnumber;
getMsgType(17)-> tfloat;
getMsgType(18)-> tdouble;
getMsgType(19)-> tstring;
getMsgType(20)-> tlistbool;
getMsgType(21)-> tlistint8;
getMsgType(22)-> tlistuint8;
getMsgType(23)-> tlistint16;
getMsgType(24)-> tlistuint16;
getMsgType(25)-> tlistint32;
getMsgType(26)-> tlistuint32;
getMsgType(27)-> tlistint64;
getMsgType(28)-> tlistuint64;
getMsgType(29)-> tlistinteger;
getMsgType(30)-> tlistnumber;
getMsgType(31)-> tlistfloat;
getMsgType(32)-> tlistdouble;
getMsgType(33)-> tliststring;
getMsgType(34)-> tlistunion;
getMsgType(35)-> allType;
getMsgType(_) -> undefined.

getMsgId(test)-> 1;
getMsgId(phoneNumber)-> 2;
getMsgId(person)-> 3;
getMsgId(addressBook)-> 4;
getMsgId(union)-> 5;
getMsgId(tbool)-> 6;
getMsgId(tint8)-> 7;
getMsgId(tuint8)-> 8;
getMsgId(tint16)-> 9;
getMsgId(tuint16)-> 10;
getMsgId(tint32)-> 11;
getMsgId(tuint32)-> 12;
getMsgId(tint64)-> 13;
getMsgId(tuint64)-> 14;
getMsgId(tinteger)-> 15;
getMsgId(tnumber)-> 16;
getMsgId(tfloat)-> 17;
getMsgId(tdouble)-> 18;
getMsgId(tstring)-> 19;
getMsgId(tlistbool)-> 20;
getMsgId(tlistint8)-> 21;
getMsgId(tlistuint8)-> 22;
getMsgId(tlistint16)-> 23;
getMsgId(tlistuint16)-> 24;
getMsgId(tlistint32)-> 25;
getMsgId(tlistuint32)-> 26;
getMsgId(tlistint64)-> 27;
getMsgId(tlistuint64)-> 28;
getMsgId(tlistinteger)-> 29;
getMsgId(tlistnumber)-> 30;
getMsgId(tlistfloat)-> 31;
getMsgId(tlistdouble)-> 32;
getMsgId(tliststring)-> 33;
getMsgId(tlistunion)-> 34;
getMsgId(allType)-> 35;
getMsgId(_) -> 0.

encodeRec({test, V1}) ->
	[?string(V1)];
encodeRec({phoneNumber, V1, V2}) ->
	[?record(V1), ?int32(V2)];
encodeRec({person, V1, V2, V3, V4}) ->
	[?string(V1), ?int32(V2), ?string(V3), ?list_record(V4)];
encodeRec({addressBook, V1, V2}) ->
	[?list_record(V1), ?list_record(V2)];
encodeRec({union, V1, V2}) ->
	[?string(V1), ?int32(V2)];
encodeRec({tbool, V1}) ->
	[?bool(V1)];
encodeRec({tint8, V1, V2}) ->
	[?int8(V1), ?int8(V2)];
encodeRec({tuint8, V1, V2}) ->
	[?uint8(V1), ?uint8(V2)];
encodeRec({tint16, V1, V2}) ->
	[?int16(V1), ?int16(V2)];
encodeRec({tuint16, V1, V2}) ->
	[?uint16(V1), ?uint16(V2)];
encodeRec({tint32, V1, V2}) ->
	[?int32(V1), ?int32(V2)];
encodeRec({tuint32, V1, V2}) ->
	[?uint32(V1), ?uint32(V2)];
encodeRec({tint64, V1, V2}) ->
	[?int64(V1), ?int64(V2)];
encodeRec({tuint64, V1, V2}) ->
	[?uint64(V1), ?uint64(V2)];
encodeRec({tinteger, V1, V2, V3, V4, V5, V6, V7, V8}) ->
	[?integer(V1), ?integer(V2), ?integer(V3), ?integer(V4), ?integer(V5), ?integer(V6), ?integer(V7), ?integer(V8)];
encodeRec({tnumber, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10}) ->
	[?number(V1), ?number(V2), ?number(V3), ?number(V4), ?number(V5), ?number(V6), ?number(V7), ?number(V8), ?number(V9), ?number(V10)];
encodeRec({tfloat, V1, V2}) ->
	[?float(V1), ?float(V2)];
encodeRec({tdouble, V1, V2}) ->
	[?double(V1), ?double(V2)];
encodeRec({tstring, V1, V2}) ->
	[?string(V1), ?string(V2)];
encodeRec({tlistbool, V1}) ->
	[?list_bool(V1)];
encodeRec({tlistint8, V1}) ->
	[?list_int8(V1)];
encodeRec({tlistuint8, V1}) ->
	[?list_uint8(V1)];
encodeRec({tlistint16, V1}) ->
	[?list_int16(V1)];
encodeRec({tlistuint16, V1}) ->
	[?list_uint16(V1)];
encodeRec({tlistint32, V1}) ->
	[?list_int32(V1)];
encodeRec({tlistuint32, V1}) ->
	[?list_uint32(V1)];
encodeRec({tlistint64, V1}) ->
	[?list_int64(V1)];
encodeRec({tlistuint64, V1}) ->
	[?list_uint64(V1)];
encodeRec({tlistinteger, V1}) ->
	[?list_integer(V1)];
encodeRec({tlistnumber, V1}) ->
	[?list_number(V1)];
encodeRec({tlistfloat, V1}) ->
	[?list_float(V1)];
encodeRec({tlistdouble, V1}) ->
	[?list_double(V1)];
encodeRec({tliststring, V1}) ->
	[?list_string(V1)];
encodeRec({tlistunion, V1}) ->
	[?list_record(V1)];
encodeRec({allType, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54, V55}) ->
	[?bool(V1), ?int8(V2), ?uint8(V3), ?int16(V4), ?uint16(V5), ?int32(V6), ?uint32(V7), ?int64(V8), ?uint64(V9), ?integer(V10), ?integer(V11), ?integer(V12), ?integer(V13), ?integer(V14), ?integer(V15), ?integer(V16), ?integer(V17), ?number(V18), ?number(V19), ?number(V20), ?number(V21), ?number(V22), ?number(V23), ?number(V24), ?number(V25), ?number(V26), ?number(V27), ?float(V28), ?double(V29), ?string(V30), ?string(V31), ?record(V32), ?list_bool(V33), ?list_int8(V34), ?list_uint8(V35), ?list_int16(V36), ?list_uint16(V37), ?list_int32(V38), ?list_uint32(V39), ?list_int64(V40), ?list_uint64(V41), ?list_integer(V42), ?list_integer(V43), ?list_integer(V44), ?list_integer(V45), ?list_number(V46), ?list_number(V47), ?list_number(V48), ?list_number(V49), ?list_number(V50), ?list_number(V51), ?list_float(V52), ?list_double(V53), ?list_string(V54), ?list_record(V55)];
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
getMsgSchema(union)->
	getMsgSchema(5);
getMsgSchema(5)->
	[string,int32];
getMsgSchema(tbool)->
	getMsgSchema(6);
getMsgSchema(6)->
	[bool];
getMsgSchema(tint8)->
	getMsgSchema(7);
getMsgSchema(7)->
	[int8,int8];
getMsgSchema(tuint8)->
	getMsgSchema(8);
getMsgSchema(8)->
	[uint8,uint8];
getMsgSchema(tint16)->
	getMsgSchema(9);
getMsgSchema(9)->
	[int16,int16];
getMsgSchema(tuint16)->
	getMsgSchema(10);
getMsgSchema(10)->
	[uint16,uint16];
getMsgSchema(tint32)->
	getMsgSchema(11);
getMsgSchema(11)->
	[int32,int32];
getMsgSchema(tuint32)->
	getMsgSchema(12);
getMsgSchema(12)->
	[uint32,uint32];
getMsgSchema(tint64)->
	getMsgSchema(13);
getMsgSchema(13)->
	[int64,int64];
getMsgSchema(tuint64)->
	getMsgSchema(14);
getMsgSchema(14)->
	[uint64,uint64];
getMsgSchema(tinteger)->
	getMsgSchema(15);
getMsgSchema(15)->
	[integer,integer,integer,integer,integer,integer,integer,integer];
getMsgSchema(tnumber)->
	getMsgSchema(16);
getMsgSchema(16)->
	[number,number,number,number,number,number,number,number,number,number];
getMsgSchema(tfloat)->
	getMsgSchema(17);
getMsgSchema(17)->
	[float,float];
getMsgSchema(tdouble)->
	getMsgSchema(18);
getMsgSchema(18)->
	[double,double];
getMsgSchema(tstring)->
	getMsgSchema(19);
getMsgSchema(19)->
	[string,string];
getMsgSchema(tlistbool)->
	getMsgSchema(20);
getMsgSchema(20)->
	[{list,bool}];
getMsgSchema(tlistint8)->
	getMsgSchema(21);
getMsgSchema(21)->
	[{list,int8}];
getMsgSchema(tlistuint8)->
	getMsgSchema(22);
getMsgSchema(22)->
	[{list,uint8}];
getMsgSchema(tlistint16)->
	getMsgSchema(23);
getMsgSchema(23)->
	[{list,int16}];
getMsgSchema(tlistuint16)->
	getMsgSchema(24);
getMsgSchema(24)->
	[{list,uint16}];
getMsgSchema(tlistint32)->
	getMsgSchema(25);
getMsgSchema(25)->
	[{list,int32}];
getMsgSchema(tlistuint32)->
	getMsgSchema(26);
getMsgSchema(26)->
	[{list,uint32}];
getMsgSchema(tlistint64)->
	getMsgSchema(27);
getMsgSchema(27)->
	[{list,int64}];
getMsgSchema(tlistuint64)->
	getMsgSchema(28);
getMsgSchema(28)->
	[{list,uint64}];
getMsgSchema(tlistinteger)->
	getMsgSchema(29);
getMsgSchema(29)->
	[{list,integer}];
getMsgSchema(tlistnumber)->
	getMsgSchema(30);
getMsgSchema(30)->
	[{list,number}];
getMsgSchema(tlistfloat)->
	getMsgSchema(31);
getMsgSchema(31)->
	[{list,float}];
getMsgSchema(tlistdouble)->
	getMsgSchema(32);
getMsgSchema(32)->
	[{list,double}];
getMsgSchema(tliststring)->
	getMsgSchema(33);
getMsgSchema(33)->
	[{list,string}];
getMsgSchema(tlistunion)->
	getMsgSchema(34);
getMsgSchema(34)->
	[{list,union}];
getMsgSchema(allType)->
	getMsgSchema(35);
getMsgSchema(35)->
	[bool,int8,uint8,int16,uint16,int32,uint32,int64,uint64,integer,integer,
 integer,integer,integer,integer,integer,integer,number,number,number,number,
 number,number,number,number,number,number,float,double,string,string,union,
 {list,bool},
 {list,int8},
 {list,uint8},
 {list,int16},
 {list,uint16},
 {list,int32},
 {list,uint32},
 {list,int64},
 {list,uint64},
 {list,integer},
 {list,integer},
 {list,integer},
 {list,integer},
 {list,number},
 {list,number},
 {list,number},
 {list,number},
 {list,number},
 {list,number},
 {list,float},
 {list,double},
 {list,string},
 {list,union}];
getMsgSchema(_) ->
	[].

