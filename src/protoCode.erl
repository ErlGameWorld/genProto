-module(protoCode).
-compile(nowarn_unused_function).


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
-define(int16(V), <<V:16/little>>).
-define(uint16(V), <<V:16/little>>).
-define(int32(V), <<V:32/little>>).
-define(uint32(V), <<V:32/little>>).
-define(int64(V), <<V:64/little>>).
-define(uint64(V), <<V:64/little>>).
-define(integer(V), (integer(V))).
-define(number(V), (number(V))).
-define(string(V), (string(V))).
-define(float(V), <<V:32/little-float>>).
-define(double(V), <<V:64/little-float>>).
-define(bool(V), (case V of true -> <<1:8>>; _ -> <<0:8>> end)).
-define(record(V), (case V of undefined -> [<<0:8>>]; V -> [<<1:8>>, encodeRec(V)] end)).
-define(list_bool(List), [<<(length(List)):16/little>>, [?bool(V) || V <- List]]).
-define(list_int8(List), [<<(length(List)):16/little>>, [?int8(V) || V <- List]]).
-define(list_uint8(List), [<<(length(List)):16/little>>, [?uint8(V) || V <- List]]).
-define(list_int16(List), [<<(length(List)):16/little>>, [?int16(V) || V <- List]]).
-define(list_uint16(List), [<<(length(List)):16/little>>, [?uint16(V) || V <- List]]).
-define(list_int32(List), [<<(length(List)):16/little>>, [?int32(V) || V <- List]]).
-define(list_uint32(List), [<<(length(List)):16/little>>, [?uint32(V) || V <- List]]).
-define(list_int64(List), [<<(length(List)):16/little>>, [?int64(V) || V <- List]]).
-define(list_uint64(List), [<<(length(List)):16/little>>, [?uint64(V) || V <- List]]).
-define(list_float(List), [<<(length(List)):16/little>>, [?float(V) || V <- List]]).
-define(list_double(List), [<<(length(List)):16/little>>, [?double(V) || V <- List]]).
-define(list_integer(List), [<<(length(List)):16/little>>, [integer(V) || V <- List]]).
-define(list_number(List), [<<(length(List)):16/little>>, [number(V) || V <- List]]).
-define(list_string(List), [<<(length(List)):16/little>>, [string(V) || V <- List]]).
-define(list_record(List), [<<(length(List)):16/little>>, [encodeRec(V) || V <- List]]).

integer(V) ->
   if
      V >= ?min8 andalso V =< ?max8 ->
         <<8:8, <<V:8>>/binary>>;
      V >= ?min16 andalso V =< ?max16 ->
         <<16:8, <<V:16/little>>/binary>>;
      V >= ?min32 andalso V =< ?max32 ->
         <<32:8, <<V:32/little>>/binary>>;
      V >= ?min64 andalso V =< ?max64 ->
         <<64:8, <<V:64/little>>/binary>>;
      true ->
         throw(exceeded_the_integer)
   end.

numInteger(V) ->
   if
      V >= ?min8 andalso V =< ?max8 ->
         <<8:8, <<V:8>>/binary>>;
      V >= ?min16 andalso V =< ?max16 ->
         <<16:8, <<V:16/little>>/binary>>;
      V >= ?min32 andalso V =< ?max32 ->
         <<32:8, <<V:32/little>>/binary>>;
      V >= ?min64 andalso V =< ?max64 ->
         <<64:8, <<V:64/little>>/binary>>;
      true ->
         throw(exceeded_the_integer)
   end.

numFloat(V) ->
   if
      V >= ?minF32 andalso V =< ?maxF32 ->
         <<33:8, <<V:32/little-float>>/binary>>;
      V >= ?minF64 andalso V =< ?maxF64 ->
         <<65:8, <<V:64/little-float>>/binary>>;
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
   [<<(byte_size(Str)):16/little>>, Str];
string(Str) ->
   Str2 = unicode:characters_to_binary(Str, utf8),
   [<<(byte_size(Str2)):16/little>>, Str2].

encode(Record) ->
   MsgBin = encodeRec(Record),
   MsgId = getMsgId(element(1, Record)),
   [<<MsgId:16/little>>, MsgBin].

decode(Bin) ->
   <<MsgId:16/little, MsgBin/binary>> = Bin,
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
         <<Int:32/little-signed, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Int | Result]);
      uint32 ->
         <<Int:32/little-unsigned, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Int | Result]);
      string ->
         <<Len:16/little, StrBin:Len/binary, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [StrBin | Result]);
      int16 ->
         <<Int:16/little-signed, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Int | Result]);
      uint16 ->
         <<Int:16/little-unsigned, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Int | Result]);
      int8 ->
         <<Int:8/little-signed, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Int | Result]);
      uint8 ->
         <<Int:8/little-unsigned, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Int | Result]);
      int64 ->
         <<Int:64/little-signed, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Int | Result]);
      uint64 ->
         <<Int:64/little-unsigned, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Int | Result]);
      integer ->
         <<IntBits:8, Int:IntBits/little-signed, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Int | Result]);
      number ->
         <<NumBits:8, NumBin/binary>> = MsgBin,
         case NumBits of
            33 ->
               <<Float:32/little-float, LeftBin/binary>> = NumBin,
               decodeField(SchList, LeftBin, [Float | Result]);
            65 ->
               <<Float:64/little-float, LeftBin/binary>> = NumBin,
               decodeField(SchList, LeftBin, [Float | Result]);
            _ ->
               <<Int:NumBits/little-signed, LeftBin/binary>> = NumBin,
               decodeField(SchList, LeftBin, [Int | Result])
         end;
      bool ->
         <<Bool:8/little-unsigned, LeftBin/binary>> = MsgBin,
         case Bool =:= 1 of
            true ->
               decodeField(SchList, LeftBin, [true | Result]);
            _ ->
               decodeField(SchList, LeftBin, [false | Result])
         end;
      float ->
         <<Float:32/little-float, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Float | Result]);
      double ->
         <<Float:64/little-float, LeftBin/binary>> = MsgBin,
         decodeField(SchList, LeftBin, [Float | Result]);
      {list, int32} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deInt32List(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, uint32} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deUint32List(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, int16} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deInt16List(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, uint16} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deUint16List(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, int8} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deInt8List(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, uint8} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deUint8List(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, string} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deStringList(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, int64} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deInt64List(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, uint64} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deUint64List(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, integer} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deIntegerList(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, number} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deNumberList(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, bool} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deBoolList(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, float} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deFloatList(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, double} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
         {LeftBin, RetList} = deDoubleList(Len, LeftListBin, []),
         decodeField(SchList, LeftBin, [RetList | Result]);
      {list, RecordName} ->
         <<Len:16/little, LeftListBin/binary>> = MsgBin,
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
   <<Int:8/little-signed, LeftBin/binary>> = MsgBin,
   deInt8List(N - 1, LeftBin, [Int | RetList]).

deUint8List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deUint8List(N, MsgBin, RetList) ->
   <<Int:8/little-unsigned, LeftBin/binary>> = MsgBin,
   deUint8List(N - 1, LeftBin, [Int | RetList]).

deInt16List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deInt16List(N, MsgBin, RetList) ->
   <<Int:16/little-signed, LeftBin/binary>> = MsgBin,
   deInt16List(N - 1, LeftBin, [Int | RetList]).

deUint16List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deUint16List(N, MsgBin, RetList) ->
   <<Int:16/little-unsigned, LeftBin/binary>> = MsgBin,
   deUint16List(N - 1, LeftBin, [Int | RetList]).

deInt32List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deInt32List(N, MsgBin, RetList) ->
   <<Int:32/little-signed, LeftBin/binary>> = MsgBin,
   deInt32List(N - 1, LeftBin, [Int | RetList]).

deUint32List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deUint32List(N, MsgBin, RetList) ->
   <<Int:32/little-unsigned, LeftBin/binary>> = MsgBin,
   deUint32List(N - 1, LeftBin, [Int | RetList]).

deInt64List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deInt64List(N, MsgBin, RetList) ->
   <<Int:64/little-signed, LeftBin/binary>> = MsgBin,
   deInt64List(N - 1, LeftBin, [Int | RetList]).

deUint64List(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deUint64List(N, MsgBin, RetList) ->
   <<Int:64/little-unsigned, LeftBin/binary>> = MsgBin,
   deUint64List(N - 1, LeftBin, [Int | RetList]).

deIntegerList(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deIntegerList(N, MsgBin, RetList) ->
   <<IntBits:8, Int:IntBits/little-signed, LeftBin/binary>> = MsgBin,
   deIntegerList(N - 1, LeftBin, [Int | RetList]).

deNumberList(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deNumberList(N, MsgBin, RetList) ->
   <<NumBits:8, NumBin/binary>> = MsgBin,
   case NumBits of
      33 ->
         <<Float:32/little-float, LeftBin/binary>> = NumBin,
         deNumberList(N - 1, LeftBin, [Float | RetList]);
      65 ->
         <<Float:64/little-float, LeftBin/binary>> = NumBin,
         deNumberList(N - 1, LeftBin, [Float | RetList]);
      _ ->
         <<Int:NumBits/little-signed, LeftBin/binary>> = NumBin,
         deNumberList(N - 1, LeftBin, [Int | RetList])
   end.

deFloatList(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deFloatList(N, MsgBin, RetList) ->
   <<Float:32/little-float, LeftBin/binary>> = MsgBin,
   deFloatList(N - 1, LeftBin, [Float | RetList]).

deDoubleList(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deDoubleList(N, MsgBin, RetList) ->
   <<Float:64/little-float, LeftBin/binary>> = MsgBin,
   deDoubleList(N - 1, LeftBin, [Float | RetList]).

deStringList(0, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deStringList(N, MsgBin, RetList) ->
   <<Len:16/little, StrBin:Len/binary-unit:8, LeftBin/binary>> = MsgBin,
   deStringList(N - 1, LeftBin, [StrBin | RetList]).

deRecordList(0, _RecordName, MsgBin, RetList) ->
   {MsgBin, lists:reverse(RetList)};
deRecordList(N, RecordName, MsgBin, RetList) ->
   {LeftBin, Tuple} = decodeRec(RecordName, MsgBin),
   deRecordList(N - 1, RecordName, LeftBin, [Tuple | RetList]).


%%%%%%%%%%%%%%%%%%%%%%%%%% 防止编译报错占位 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encodeRec(_) ->
   ok.
getMsgId(_) ->
   ok.
getMsgType(_) ->
   ok.
getMsgSchema(_) ->
   ok.



