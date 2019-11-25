-module(protoCode).

-compile([export_all, nowarn_unused_function, nowarn_export_all]).


-export([encode/1, decode/1, encodeRec/1, decodeBin/2]).

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

number(V) ->
   if
      erlang:is_integer(V) ->
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
         end;
      erlang:is_float(V) ->
         if
            V >= ?minF32 andalso V =< ?maxF32 ->
               <<33:8, <<V:32/big-float>>/binary>>;
            V >= ?minF64 andalso V =< ?maxF64 ->
               <<65:8, <<V:64/big-float>>/binary>>;
            true ->
               throw(exceeded_the_float)
         end;
      true ->
         throw(is_not_number)
   end.

string(Str) when is_binary(Str) ->
   [<<(byte_size(Str)):16/big>>, Str];
string(Str) ->
   Str2 = unicode:characters_to_binary(Str, utf8),
   [<<(byte_size(Str2)):16/big>>, Str2].

decode(Bin) ->
   <<MsgId:16/big, MsgBin/binary>> = Bin,
   decodeBin(MsgId, MsgBin).

deBoolList(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deBoolList(N, MsgBin, RetList) ->
   <<Bool:8, LeftBin/binary>> = MsgBin,
   case Bool =:= 1 of
      true ->
         deBoolList(N - 1, LeftBin, [true | RetList]);
      _ ->
         deBoolList(N - 1, LeftBin, [false | RetList])
   end.

deInt8List(0, MsgBin, RetList) ->
   {RetList, MsgBin};
deInt8List(N, MsgBin, RetList) ->
   <<Int:8/big-signed, LeftBin/binary>> = MsgBin,
   deInt8List(N - 1, LeftBin, [Int | RetList]).

deUint8List(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deUint8List(N, MsgBin, RetList) ->
   <<Int:8/big-unsigned, LeftBin/binary>> = MsgBin,
   deUint8List(N - 1, LeftBin, [Int | RetList]).

deInt16List(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deInt16List(N, MsgBin, RetList) ->
   <<Int:16/big-signed, LeftBin/binary>> = MsgBin,
   deInt16List(N - 1, LeftBin, [Int | RetList]).

deUint16List(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deUint16List(N, MsgBin, RetList) ->
   <<Int:16/big-unsigned, LeftBin/binary>> = MsgBin,
   deUint16List(N - 1, LeftBin, [Int | RetList]).

deInt32List(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deInt32List(N, MsgBin, RetList) ->
   <<Int:32/big-signed, LeftBin/binary>> = MsgBin,
   deInt32List(N - 1, LeftBin, [Int | RetList]).

deUint32List(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deUint32List(N, MsgBin, RetList) ->
   <<Int:32/big-unsigned, LeftBin/binary>> = MsgBin,
   deUint32List(N - 1, LeftBin, [Int | RetList]).

deInt64List(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deInt64List(N, MsgBin, RetList) ->
   <<Int:64/big-signed, LeftBin/binary>> = MsgBin,
   deInt64List(N - 1, LeftBin, [Int | RetList]).

deUint64List(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deUint64List(N, MsgBin, RetList) ->
   <<Int:64/big-unsigned, LeftBin/binary>> = MsgBin,
   deUint64List(N - 1, LeftBin, [Int | RetList]).

deIntegerList(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deIntegerList(N, MsgBin, RetList) ->
   <<IntBits:8, Int:IntBits/big-signed, LeftBin/binary>> = MsgBin,
   deIntegerList(N - 1, LeftBin, [Int | RetList]).

deNumberList(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
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
   {lists:reverse(RetList), MsgBin};
deFloatList(N, MsgBin, RetList) ->
   <<Float:32/big-float, LeftBin/binary>> = MsgBin,
   deFloatList(N - 1, LeftBin, [Float | RetList]).

deDoubleList(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deDoubleList(N, MsgBin, RetList) ->
   <<Float:64/big-float, LeftBin/binary>> = MsgBin,
   deDoubleList(N - 1, LeftBin, [Float | RetList]).

deStringList(0, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deStringList(N, MsgBin, RetList) ->
   <<Len:16/big, StrBin:Len/binary-unit:8, LeftBin/binary>> = MsgBin,
   deStringList(N - 1, LeftBin, [StrBin | RetList]).

deRecordList(0, _MsgId, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deRecordList(N, MsgId, MsgBin, RetList) ->
   {Tuple, LeftBin} = decodeRec(MsgId, MsgBin),
   deRecordList(N - 1, MsgId, LeftBin, [Tuple | RetList]).

%%%%%%%%%%%%%%%%%%%%%%%%%% 防止编译报错占位 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encode(_) ->
   ok.
encodeRec(_) ->
   ok.
decodeBin(_, _) ->
   ok.
decodeRec(_, _) ->
   ok.
getMsgId(_) ->
   ok.



