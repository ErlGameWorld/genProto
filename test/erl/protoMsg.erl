-module(protoMsg).


-compile([nowarn_unused_vars, nowarn_unused_function]).

-export([encodeIol/1, encodeBin/1,  encodeIol/2, subEncode/1, subEncode/2, decode/1,  decodeBin/2, getMsgName/1]).

-define(min8, -128).
-define(max8, 127).
-define(min16, -32768).
-define(max16, 32767).
-define(min32, -2147483648).
-define(max32, 2147483647).
-define(min64, -9223372036854775808).
-define(max64, 9223372036854775807).

-define(minF32, 1.175494351E-38).
-define(maxF32, 3.402823466E+38).
-define(minF64, 2.2250738585072014E-308).
-define(maxF64, 1.7976931348623158E+308).

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
-define(binary(V), (binary(V))).
-define(float(V), <<V:32/big-float>>).
-define(double(V), <<V:64/big-float>>).
-define(bool(V), (case V of true -> <<1:8>>; _ -> <<0:8>> end)).
-define(record(V), (case V of undefined -> [<<0:8>>]; V -> [<<1:8>>, subEncode(V)] end)).
-define(list_bool(List), [<<(listLen(List)):16/big>>, [?bool(V) || V <- List]]).
-define(list_int8(List), [<<(listLen(List)):16/big>>, [?int8(V) || V <- List]]).
-define(list_uint8(List), [<<(listLen(List)):16/big>>, [?uint8(V) || V <- List]]).
-define(list_int16(List), [<<(listLen(List)):16/big>>, [?int16(V) || V <- List]]).
-define(list_uint16(List), [<<(listLen(List)):16/big>>, [?uint16(V) || V <- List]]).
-define(list_int32(List), [<<(listLen(List)):16/big>>, [?int32(V) || V <- List]]).
-define(list_uint32(List), [<<(listLen(List)):16/big>>, [?uint32(V) || V <- List]]).
-define(list_int64(List), [<<(listLen(List)):16/big>>, [?int64(V) || V <- List]]).
-define(list_uint64(List), [<<(listLen(List)):16/big>>, [?uint64(V) || V <- List]]).
-define(list_float(List), [<<(listLen(List)):16/big>>, [?float(V) || V <- List]]).
-define(list_double(List), [<<(listLen(List)):16/big>>, [?double(V) || V <- List]]).
-define(list_integer(List), [<<(listLen(List)):16/big>>, [integer(V) || V <- List]]).
-define(list_number(List), [<<(listLen(List)):16/big>>, [number(V) || V <- List]]).
-define(list_string(List), [<<(listLen(List)):16/big>>, [string(V) || V <- List]]).
-define(list_binary(List), [<<(listLen(List)):16/big>>, [binary(V) || V <- List]]).
-define(list_record(List), [<<(listLen(List)):16/big>>, [subEncode(V) || V <- List]]).

listLen(List) ->
   Len = length(List),
   case Len > 65535 of
      true ->
         throw({list_too_long, Len, List});
      _ ->
         Len
   end.

-define(BinaryShareSize, 65).
-define(BinaryCopyRatio, 1.2).

integer(V) when is_integer(V) ->
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
         throw({exceeded_the_integer, V})
   end;
integer(V) ->
   throw({not_an_integer, V}).

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
               throw({exceeded_the_integer, V})
         end;
      erlang:is_float(V) ->
         if
            V >= ?minF32 andalso V =< ?maxF32 ->
               <<33:8, <<V:32/big-float>>/binary>>;
            V >= ?minF64 andalso V =< ?maxF64 ->
               <<65:8, <<V:64/big-float>>/binary>>;
            true ->
               throw({exceeded_the_float, V})
         end;
      true ->
         throw({is_not_number, V})
   end.

string(Str) when is_binary(Str) ->
   StrLen = byte_size(Str),
   case StrLen > 65535 of
      true ->
         throw({string_too_long, StrLen, Str});
      _ ->
         [<<StrLen:16/big>>, Str]
   end;
string(Str) ->
   case unicode:characters_to_binary(Str, utf8) of
      {error, Encoded, _Rest} ->
         throw({invalid_utf8_string, Str, Encoded});
      {incomplete, Encoded, _Rest} ->
         throw({incomplete_utf8_string, Str, Encoded});
      Str2 ->
         StrLen = byte_size(Str2),
         case StrLen > 65535 of
            true ->
               throw({string_too_long, StrLen, Str});
            _ ->
               [<<StrLen:16/big>>, Str2]
         end
   end.

binary(Bin) when is_binary(Bin) ->
   BinLen = byte_size(Bin),
   case BinLen > 65535 of
      true ->
         throw({binary_too_long, BinLen});
      _ ->
         [<<BinLen:16/big>>, Bin]
   end;
binary(Bin) when is_list(Bin) ->
   binary(erlang:iolist_to_binary(Bin));
binary(Bin) ->
   throw({not_a_binary, Bin}).

decode(Bin) ->
   <<MsgId:16/big, MsgBin/binary>> = Bin,
   decodeBin(MsgId, MsgBin).

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

deStringList(0, MsgBin, _RefSize, RetList) ->
   {lists:reverse(RetList), MsgBin};
deStringList(N, MsgBin, RefSize, RetList) ->
   <<Len:16/big, StrBin:Len/binary-unit:8, LeftBin/binary>> = MsgBin,
   case Len < ?BinaryShareSize of
      true ->
         deStringList(N - 1, LeftBin, RefSize, [StrBin | RetList]);
      _ ->
         case RefSize / Len > ?BinaryCopyRatio of
            true ->
               StrBinCopy = binary:copy(StrBin),
               deStringList(N - 1, LeftBin, RefSize, [StrBinCopy | RetList]);
            _ ->
               deStringList(N - 1, LeftBin, RefSize, [StrBin | RetList])
         end
   end.

deRecordList(0, _MsgId, MsgBin, RetList) ->
   {lists:reverse(RetList), MsgBin};
deRecordList(N, MsgId, MsgBin, RetList) ->
   {Tuple, LeftBin} = decodeRec(MsgId, MsgBin),
   deRecordList(N - 1, MsgId, LeftBin, [Tuple | RetList]).

encodeIol(RecMsg) ->
   encodeIol(erlang:element(1, RecMsg), RecMsg).

encodeBin(RecMsg) ->
   erlang:iolist_to_binary(encodeIol(RecMsg)).

subEncode(RecMsg) ->
   subEncode(erlang:element(1, RecMsg), RecMsg).

subEncode(test, {_, V1}) ->
	[?string(V1)];
subEncode(phoneNumber, {_, V1, V2}) ->
	[?record(V1), ?int32(V2)];
subEncode(person, {_, V1, V2, V3, V4}) ->
	[?string(V1), ?int32(V2), ?string(V3), ?list_record(V4)];
subEncode(union, {_, V1, V2}) ->
	[?string(V1), ?int32(V2)];
subEncode(comboLeaf, {_, V1, V2}) ->
	[?int32(V1), ?string(V2)];
subEncode(comboNode, {_, V1, V2, V3}) ->
	[?record(V1), ?list_record(V2), (case V3 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V3)):16/big>>, [ [?string(MK3), ?int32(MV3)] || MK3 := MV3 <- V3]] end)];
subEncode(comboWrap, {_, V1, V2, V3}) ->
	[?string(V1), ?record(V2), (case V3 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V3)):16/big>>, [ [?int32(MK3), ?string(MV3)] || MK3 := MV3 <- V3]] end)];
subEncode(benchLeaf, {_, V1, V2, V3}) ->
	[?int32(V1), ?string(V2), ?binary(V3)];
subEncode(_, _) ->
	[].

encodeIol(test, {_, V1}) ->
	[<<1:16/big-unsigned>>, ?string(V1)];
encodeIol(phoneNumber, {_, V1, V2}) ->
	[<<2:16/big-unsigned>>, ?record(V1), ?int32(V2)];
encodeIol(person, {_, V1, V2, V3, V4}) ->
	[<<3:16/big-unsigned>>, ?string(V1), ?int32(V2), ?string(V3), ?list_record(V4)];
encodeIol(addressBook, {_, V1, V2}) ->
	[<<4:16/big-unsigned>>, ?list_record(V1), ?list_record(V2)];
encodeIol(union, {_, V1, V2}) ->
	[<<5:16/big-unsigned>>, ?string(V1), ?int32(V2)];
encodeIol(tbool, {_, V1}) ->
	[<<6:16/big-unsigned>>, ?bool(V1)];
encodeIol(tint8, {_, V1, V2}) ->
	[<<7:16/big-unsigned>>, ?int8(V1), ?int8(V2)];
encodeIol(tuint8, {_, V1, V2}) ->
	[<<8:16/big-unsigned>>, ?uint8(V1), ?uint8(V2)];
encodeIol(tint16, {_, V1, V2}) ->
	[<<9:16/big-unsigned>>, ?int16(V1), ?int16(V2)];
encodeIol(tuint16, {_, V1, V2}) ->
	[<<10:16/big-unsigned>>, ?uint16(V1), ?uint16(V2)];
encodeIol(tint32, {_, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10}) ->
	[<<11:16/big-unsigned>>, ?int32(V1), ?int32(V2), ?int32(V3), ?int32(V4), ?int32(V5), ?int32(V6), ?int32(V7), ?int32(V8), ?int32(V9), ?int32(V10)];
encodeIol(tuint32, {_, V1, V2}) ->
	[<<12:16/big-unsigned>>, ?uint32(V1), ?uint32(V2)];
encodeIol(tint64, {_, V1, V2}) ->
	[<<13:16/big-unsigned>>, ?int64(V1), ?int64(V2)];
encodeIol(tuint64, {_, V1, V2}) ->
	[<<14:16/big-unsigned>>, ?uint64(V1), ?uint64(V2)];
encodeIol(tinteger, {_, V1, V2, V3, V4, V5, V6, V7, V8}) ->
	[<<15:16/big-unsigned>>, ?integer(V1), ?integer(V2), ?integer(V3), ?integer(V4), ?integer(V5), ?integer(V6), ?integer(V7), ?integer(V8)];
encodeIol(tnumber, {_, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10}) ->
	[<<16:16/big-unsigned>>, ?number(V1), ?number(V2), ?number(V3), ?number(V4), ?number(V5), ?number(V6), ?number(V7), ?number(V8), ?number(V9), ?number(V10)];
encodeIol(tfloat, {_, V1, V2}) ->
	[<<17:16/big-unsigned>>, ?float(V1), ?float(V2)];
encodeIol(tdouble, {_, V1, V2}) ->
	[<<18:16/big-unsigned>>, ?double(V1), ?double(V2)];
encodeIol(tstring, {_, V1, V2}) ->
	[<<19:16/big-unsigned>>, ?string(V1), ?string(V2)];
encodeIol(tlistbool, {_, V1}) ->
	[<<20:16/big-unsigned>>, ?list_bool(V1)];
encodeIol(tlistint8, {_, V1}) ->
	[<<21:16/big-unsigned>>, ?list_int8(V1)];
encodeIol(tlistuint8, {_, V1}) ->
	[<<22:16/big-unsigned>>, ?list_uint8(V1)];
encodeIol(tlistint16, {_, V1}) ->
	[<<23:16/big-unsigned>>, ?list_int16(V1)];
encodeIol(tlistuint16, {_, V1}) ->
	[<<24:16/big-unsigned>>, ?list_uint16(V1)];
encodeIol(tlistint32, {_, V1}) ->
	[<<25:16/big-unsigned>>, ?list_int32(V1)];
encodeIol(tlistuint32, {_, V1}) ->
	[<<26:16/big-unsigned>>, ?list_uint32(V1)];
encodeIol(tlistint64, {_, V1}) ->
	[<<27:16/big-unsigned>>, ?list_int64(V1)];
encodeIol(tlistuint64, {_, V1}) ->
	[<<28:16/big-unsigned>>, ?list_uint64(V1)];
encodeIol(tlistinteger, {_, V1}) ->
	[<<29:16/big-unsigned>>, ?list_integer(V1)];
encodeIol(tlistnumber, {_, V1}) ->
	[<<30:16/big-unsigned>>, ?list_number(V1)];
encodeIol(tlistfloat, {_, V1}) ->
	[<<31:16/big-unsigned>>, ?list_float(V1)];
encodeIol(tlistdouble, {_, V1}) ->
	[<<32:16/big-unsigned>>, ?list_double(V1)];
encodeIol(tliststring, {_, V1}) ->
	[<<33:16/big-unsigned>>, ?list_string(V1)];
encodeIol(tlistunion, {_, V1}) ->
	[<<34:16/big-unsigned>>, ?list_record(V1)];
encodeIol(allType, {_, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54, V55}) ->
	[<<35:16/big-unsigned>>, ?bool(V1), ?int8(V2), ?uint8(V3), ?int16(V4), ?uint16(V5), ?int32(V6), ?uint32(V7), ?int64(V8), ?uint64(V9), ?integer(V10), ?integer(V11), ?integer(V12), ?integer(V13), ?integer(V14), ?integer(V15), ?integer(V16), ?integer(V17), ?number(V18), ?number(V19), ?number(V20), ?number(V21), ?number(V22), ?number(V23), ?number(V24), ?number(V25), ?number(V26), ?number(V27), ?float(V28), ?double(V29), ?string(V30), ?string(V31), ?record(V32), ?list_bool(V33), ?list_int8(V34), ?list_uint8(V35), ?list_int16(V36), ?list_uint16(V37), ?list_int32(V38), ?list_uint32(V39), ?list_int64(V40), ?list_uint64(V41), ?list_integer(V42), ?list_integer(V43), ?list_integer(V44), ?list_integer(V45), ?list_number(V46), ?list_number(V47), ?list_number(V48), ?list_number(V49), ?list_number(V50), ?list_number(V51), ?list_float(V52), ?list_double(V53), ?list_string(V54), ?list_record(V55)];
encodeIol(testnull, {_}) ->
	[<<36:16/big-unsigned>>];
encodeIol(tbinary, {_, V1, V2}) ->
	[<<37:16/big-unsigned>>, ?binary(V1), ?list_binary(V2)];
encodeIol(tmap, {_, V1, V2, V3}) ->
	[<<38:16/big-unsigned>>, (case V1 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V1)):16/big>>, [ [?string(MK1), ?int32(MV1)] || MK1 := MV1 <- V1]] end), (case V2 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V2)):16/big>>, [ [?int32(MK2), ?string(MV2)] || MK2 := MV2 <- V2]] end), (case V3 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V3)):16/big>>, [ [?string(MK3), subEncode(MV3)] || MK3 := MV3 <- V3]] end)];
encodeIol(toneof, {_, V1, V2, V3, V4}) ->
	[<<39:16/big-unsigned>>, ?int32(V1), (case V2 of undefined -> <<0:8>>; {gold, OV1} -> [<<1:8>>, ?int32(OV1)]; {coupon, OV2} -> [<<2:8>>, ?string(OV2)]; {phone, OV3} -> [<<3:8>>, subEncode(OV3)]; _ -> throw({invalid_oneof, V2}) end), (case V3 of undefined -> <<0:8>>; {gold, OV1} -> [<<1:8>>, ?int32(OV1)]; {coupon, OV2} -> [<<2:8>>, ?string(OV2)]; {phone, OV3} -> [<<3:8>>, subEncode(OV3)]; _ -> throw({invalid_oneof, V3}) end), ?string(V4)];
encodeIol(person1, {_, V1, V2, V3, V4}) ->
	[<<1001:16/big-unsigned>>, ?string(V1), ?int32(V2), ?string(V3), ?list_record(V4)];
encodeIol(comboLeaf, {_, V1, V2}) ->
	[<<2001:16/big-unsigned>>, ?int32(V1), ?string(V2)];
encodeIol(comboNode, {_, V1, V2, V3}) ->
	[<<2002:16/big-unsigned>>, ?record(V1), ?list_record(V2), (case V3 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V3)):16/big>>, [ [?string(MK3), ?int32(MV3)] || MK3 := MV3 <- V3]] end)];
encodeIol(comboWrap, {_, V1, V2, V3}) ->
	[<<2003:16/big-unsigned>>, ?string(V1), ?record(V2), (case V3 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V3)):16/big>>, [ [?int32(MK3), ?string(MV3)] || MK3 := MV3 <- V3]] end)];
encodeIol(tMapMatrix, {_, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10}) ->
	[<<2004:16/big-unsigned>>, (case V1 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V1)):16/big>>, [ [?string(MK1), ?int32(MV1)] || MK1 := MV1 <- V1]] end), (case V2 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V2)):16/big>>, [ [?int32(MK2), ?string(MV2)] || MK2 := MV2 <- V2]] end), (case V3 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V3)):16/big>>, [ [?string(MK3), ?string(MV3)] || MK3 := MV3 <- V3]] end), (case V4 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V4)):16/big>>, [ [?int32(MK4), ?int32(MV4)] || MK4 := MV4 <- V4]] end), (case V5 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V5)):16/big>>, [ [?int64(MK5), ?int32(MV5)] || MK5 := MV5 <- V5]] end), (case V6 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V6)):16/big>>, [ [?uint32(MK6), ?string(MV6)] || MK6 := MV6 <- V6]] end), (case V7 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V7)):16/big>>, [ [?string(MK7), ?binary(MV7)] || MK7 := MV7 <- V7]] end), (case V8 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V8)):16/big>>, [ [?int32(MK8), ?binary(MV8)] || MK8 := MV8 <- V8]] end), (case V9 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V9)):16/big>>, [ [?string(MK9), subEncode(MV9)] || MK9 := MV9 <- V9]] end), (case V10 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V10)):16/big>>, [ [?int32(MK10), subEncode(MV10)] || MK10 := MV10 <- V10]] end)];
encodeIol(tListMatrix, {_, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24, V25}) ->
	[<<2005:16/big-unsigned>>, ?list_bool(V1), ?list_int8(V2), ?list_uint8(V3), ?list_int16(V4), ?list_uint16(V5), ?list_int32(V6), ?list_uint32(V7), ?list_int64(V8), ?list_uint64(V9), ?list_integer(V10), ?list_number(V11), ?list_float(V12), ?list_double(V13), ?list_string(V14), ?list_binary(V15), ?list_record(V16), ?list_record(V17), ?list_record(V18), ?list_record(V19), ?list_int32(V20), ?list_int32(V21), ?list_int32(V22), (case V23 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V23)):16/big>>, [ [?string(MK23), ?int32(MV23)] || MK23 := MV23 <- V23]] end), (case V24 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V24)):16/big>>, [ [?string(MK24), ?int32(MV24)] || MK24 := MV24 <- V24]] end), (case V25 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V25)):16/big>>, [ [?string(MK25), ?int32(MV25)] || MK25 := MV25 <- V25]] end)];
encodeIol(tOneofMatrix, {_, V1}) ->
	[<<2006:16/big-unsigned>>, (case V1 of undefined -> <<0:8>>; {flag, OV1} -> [<<1:8>>, ?bool(OV1)]; {num, OV2} -> [<<2:8>>, ?int32(OV2)]; {text, OV3} -> [<<3:8>>, ?string(OV3)]; {raw, OV4} -> [<<4:8>>, ?binary(OV4)]; {phone, OV5} -> [<<5:8>>, subEncode(OV5)]; {leaf, OV6} -> [<<6:8>>, subEncode(OV6)]; {node, OV7} -> [<<7:8>>, subEncode(OV7)]; {ids, OV8} -> [<<8:8>>, ?list_int32(OV8)]; {tags, OV9} -> [<<9:8>>, ?list_string(OV9)]; _ -> throw({invalid_oneof, V1}) end)];
encodeIol(tNestDeep, {_, V1, V2, V3, V4, V5, V6}) ->
	[<<2007:16/big-unsigned>>, (case V1 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V1)):16/big>>, [ [?string(MK1), subEncode(MV1)] || MK1 := MV1 <- V1]] end), ?record(V2), (case V3 of undefined -> <<0:8>>; {as_node, OV1} -> [<<1:8>>, subEncode(OV1)]; {as_leaf, OV2} -> [<<2:8>>, subEncode(OV2)]; _ -> throw({invalid_oneof, V3}) end), ?list_record(V4), ?list_record(V5), ?list_record(V6)];
encodeIol(tInterleave, {_, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18}) ->
	[<<2008:16/big-unsigned>>, ?int32(V1), ?string(V2), (case V3 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V3)):16/big>>, [ [?string(MK3), ?int32(MV3)] || MK3 := MV3 <- V3]] end), ?list_int32(V4), (case V5 of undefined -> <<0:8>>; {a, OV1} -> [<<1:8>>, ?int32(OV1)]; {b, OV2} -> [<<2:8>>, ?string(OV2)]; _ -> throw({invalid_oneof, V5}) end), ?int32(V6), ?string(V7), (case V8 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V8)):16/big>>, [ [?string(MK8), ?int32(MV8)] || MK8 := MV8 <- V8]] end), ?list_int32(V9), (case V10 of undefined -> <<0:8>>; {a, OV1} -> [<<1:8>>, ?int32(OV1)]; {b, OV2} -> [<<2:8>>, ?string(OV2)]; _ -> throw({invalid_oneof, V10}) end), ?int32(V11), ?string(V12), (case V13 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V13)):16/big>>, [ [?string(MK13), ?int32(MV13)] || MK13 := MV13 <- V13]] end), ?list_int32(V14), (case V15 of undefined -> <<0:8>>; {a, OV1} -> [<<1:8>>, ?int32(OV1)]; {b, OV2} -> [<<2:8>>, ?string(OV2)]; _ -> throw({invalid_oneof, V15}) end), ?binary(V16), ?binary(V17), ?binary(V18)];
encodeIol(tMultiOneof, {_, V1, V2, V3, V4, V5, V6, V7, V8, V9}) ->
	[<<2009:16/big-unsigned>>, (case V1 of undefined -> <<0:8>>; {x, OV1} -> [<<1:8>>, ?int32(OV1)]; {y, OV2} -> [<<2:8>>, ?string(OV2)]; _ -> throw({invalid_oneof, V1}) end), ?int32(V2), (case V3 of undefined -> <<0:8>>; {x, OV1} -> [<<1:8>>, ?int32(OV1)]; {y, OV2} -> [<<2:8>>, ?string(OV2)]; {z, OV3} -> [<<3:8>>, ?binary(OV3)]; _ -> throw({invalid_oneof, V3}) end), ?int32(V4), (case V5 of undefined -> <<0:8>>; {x, OV1} -> [<<1:8>>, ?int32(OV1)]; {p, OV2} -> [<<2:8>>, subEncode(OV2)]; _ -> throw({invalid_oneof, V5}) end), ?int32(V6), (case V7 of undefined -> <<0:8>>; {l, OV1} -> [<<1:8>>, subEncode(OV1)]; {ids, OV2} -> [<<2:8>>, ?list_int32(OV2)]; _ -> throw({invalid_oneof, V7}) end), ?int32(V8), (case V9 of undefined -> <<0:8>>; {s, OV1} -> [<<1:8>>, ?string(OV1)]; {x, OV2} -> [<<2:8>>, ?int32(OV2)]; _ -> throw({invalid_oneof, V9}) end)];
encodeIol(tEmptyEdge, {_, V1, V2, V3, V4, V5, V6, V7, V8}) ->
	[<<2010:16/big-unsigned>>, (case V1 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V1)):16/big>>, [ [?string(MK1), ?int32(MV1)] || MK1 := MV1 <- V1]] end), ?list_int32(V2), ?list_string(V3), ?binary(V4), ?string(V5), (case V6 of undefined -> <<0:8>>; {x, OV1} -> [<<1:8>>, ?int32(OV1)]; {y, OV2} -> [<<2:8>>, ?string(OV2)]; _ -> throw({invalid_oneof, V6}) end), ?record(V7), ?record(V8)];
encodeIol(tScalarRun, {_, V1, V2, V3, V4, V5, V6, V7, V8, V9}) ->
	[<<2011:16/big-unsigned>>, ?int32(V1), ?int32(V2), ?int32(V3), ?string(V4), ?string(V5), ?string(V6), (case V7 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V7)):16/big>>, [ [?string(MK7), ?int32(MV7)] || MK7 := MV7 <- V7]] end), (case V8 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V8)):16/big>>, [ [?string(MK8), ?int32(MV8)] || MK8 := MV8 <- V8]] end), (case V9 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V9)):16/big>>, [ [?string(MK9), ?int32(MV9)] || MK9 := MV9 <- V9]] end)];
encodeIol(tRepeatAll, {_, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54, V55, V56, V57, V58, V59, V60, V61, V62, V63, V64, V65, V66, V67, V68, V69, V70, V71, V72, V73, V74, V75, V76, V77, V78, V79, V80, V81, V82, V83, V84, V85, V86, V87, V88, V89, V90, V91, V92, V93, V94, V95, V96, V97, V98, V99, V100, V101, V102, V103, V104, V105, V106, V107, V108, V109, V110, V111, V112, V113, V114, V115, V116, V117, V118, V119, V120, V121, V122, V123, V124, V125, V126, V127}) ->
	[<<2012:16/big-unsigned>>, ?bool(V1), ?bool(V2), ?bool(V3), ?int32(V4), ?int32(V5), ?int32(V6), ?string(V7), ?int32(V8), ?int8(V9), ?int8(V10), ?uint8(V11), ?int8(V12), ?uint8(V13), ?uint8(V14), ?int16(V15), ?int16(V16), ?int16(V17), ?uint16(V18), ?string(V19), ?uint16(V20), ?uint16(V21), ?int32(V22), ?int32(V23), ?int32(V24), ?uint32(V25), ?uint32(V26), ?uint32(V27), ?int64(V28), ?int64(V29), ?string(V30), ?int64(V31), ?uint64(V32), ?uint64(V33), ?uint64(V34), ?integer(V35), ?integer(V36), ?integer(V37), ?number(V38), ?string(V39), ?number(V40), ?number(V41), ?float(V42), ?float(V43), ?float(V44), ?double(V45), ?double(V46), ?double(V47), ?string(V48), ?string(V49), ?string(V50), ?int32(V51), ?string(V52), ?binary(V53), ?binary(V54), ?binary(V55), ?record(V56), ?record(V57), ?record(V58), ?list_int32(V59), ?list_int32(V60), ?list_int32(V61), ?list_string(V62), ?int32(V63), ?list_string(V64), ?list_string(V65), ?list_binary(V66), ?list_binary(V67), ?list_binary(V68), ?list_bool(V69), ?list_bool(V70), ?list_bool(V71), ?list_integer(V72), ?list_integer(V73), ?list_integer(V74), ?list_number(V75), ?list_number(V76), ?list_number(V77), ?list_record(V78), ?list_record(V79), ?string(V80), ?list_record(V81), ?list_record(V82), ?list_record(V83), ?list_record(V84), (case V85 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V85)):16/big>>, [ [?string(MK85), ?int32(MV85)] || MK85 := MV85 <- V85]] end), (case V86 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V86)):16/big>>, [ [?string(MK86), ?int32(MV86)] || MK86 := MV86 <- V86]] end), (case V87 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V87)):16/big>>, [ [?string(MK87), ?int32(MV87)] || MK87 := MV87 <- V87]] end), (case V88 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V88)):16/big>>, [ [?int32(MK88), ?string(MV88)] || MK88 := MV88 <- V88]] end), ?int32(V89), (case V90 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V90)):16/big>>, [ [?int32(MK90), ?string(MV90)] || MK90 := MV90 <- V90]] end), (case V91 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V91)):16/big>>, [ [?int32(MK91), ?string(MV91)] || MK91 := MV91 <- V91]] end), (case V92 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V92)):16/big>>, [ [?string(MK92), ?string(MV92)] || MK92 := MV92 <- V92]] end), (case V93 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V93)):16/big>>, [ [?string(MK93), ?string(MV93)] || MK93 := MV93 <- V93]] end), (case V94 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V94)):16/big>>, [ [?string(MK94), ?string(MV94)] || MK94 := MV94 <- V94]] end), (case V95 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V95)):16/big>>, [ [?int32(MK95), ?int32(MV95)] || MK95 := MV95 <- V95]] end), (case V96 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V96)):16/big>>, [ [?int32(MK96), ?int32(MV96)] || MK96 := MV96 <- V96]] end), (case V97 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V97)):16/big>>, [ [?int32(MK97), ?int32(MV97)] || MK97 := MV97 <- V97]] end), (case V98 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V98)):16/big>>, [ [?string(MK98), ?binary(MV98)] || MK98 := MV98 <- V98]] end), (case V99 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V99)):16/big>>, [ [?string(MK99), ?binary(MV99)] || MK99 := MV99 <- V99]] end), (case V100 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V100)):16/big>>, [ [?string(MK100), ?binary(MV100)] || MK100 := MV100 <- V100]] end), (case V101 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V101)):16/big>>, [ [?int32(MK101), ?binary(MV101)] || MK101 := MV101 <- V101]] end), (case V102 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V102)):16/big>>, [ [?int32(MK102), ?binary(MV102)] || MK102 := MV102 <- V102]] end), (case V103 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V103)):16/big>>, [ [?int32(MK103), ?binary(MV103)] || MK103 := MV103 <- V103]] end), (case V104 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V104)):16/big>>, [ [?string(MK104), subEncode(MV104)] || MK104 := MV104 <- V104]] end), (case V105 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V105)):16/big>>, [ [?string(MK105), subEncode(MV105)] || MK105 := MV105 <- V105]] end), (case V106 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V106)):16/big>>, [ [?string(MK106), subEncode(MV106)] || MK106 := MV106 <- V106]] end), (case V107 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V107)):16/big>>, [ [?int32(MK107), subEncode(MV107)] || MK107 := MV107 <- V107]] end), (case V108 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V108)):16/big>>, [ [?int32(MK108), subEncode(MV108)] || MK108 := MV108 <- V108]] end), (case V109 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V109)):16/big>>, [ [?int32(MK109), subEncode(MV109)] || MK109 := MV109 <- V109]] end), (case V110 of undefined -> <<0:8>>; {gold, OV1} -> [<<1:8>>, ?int32(OV1)]; {coupon, OV2} -> [<<2:8>>, ?string(OV2)]; {voucher, OV3} -> [<<3:8>>, ?binary(OV3)]; _ -> throw({invalid_oneof, V110}) end), ?int32(V111), (case V112 of undefined -> <<0:8>>; {gold, OV1} -> [<<1:8>>, ?int32(OV1)]; {coupon, OV2} -> [<<2:8>>, ?string(OV2)]; {phone, OV3} -> [<<3:8>>, subEncode(OV3)]; _ -> throw({invalid_oneof, V112}) end), ?string(V113), (case V114 of undefined -> <<0:8>>; {gold, OV1} -> [<<1:8>>, ?int32(OV1)]; {leaf, OV2} -> [<<2:8>>, subEncode(OV2)]; {ids, OV3} -> [<<3:8>>, ?list_int32(OV3)]; _ -> throw({invalid_oneof, V114}) end), ?record(V115), ?record(V116), ?string(V117), ?record(V118), ?record(V119), ?record(V120), ?record(V121), ?record(V122), ?record(V123), ?record(V124), ?record(V125), ?record(V126), ?record(V127)];
encodeIol(benchLeaf, {_, V1, V2, V3}) ->
	[<<3001:16/big-unsigned>>, ?int32(V1), ?string(V2), ?binary(V3)];
encodeIol(tBenchScalar, {_, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24}) ->
	[<<3002:16/big-unsigned>>, ?int32(V1), ?int32(V2), ?int32(V3), ?int32(V4), ?int32(V5), ?int32(V6), ?int32(V7), ?int32(V8), ?int64(V9), ?int64(V10), ?int64(V11), ?int64(V12), ?string(V13), ?string(V14), ?string(V15), ?string(V16), ?binary(V17), ?binary(V18), ?binary(V19), ?binary(V20), ?bool(V21), ?bool(V22), ?float(V23), ?double(V24)];
encodeIol(tBenchList, {_, V1, V2, V3, V4, V5}) ->
	[<<3003:16/big-unsigned>>, ?list_int32(V1), ?list_string(V2), ?list_binary(V3), ?list_record(V4), ?list_record(V5)];
encodeIol(tBenchMap, {_, V1, V2, V3, V4, V5}) ->
	[<<3004:16/big-unsigned>>, (case V1 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V1)):16/big>>, [ [?string(MK1), ?int32(MV1)] || MK1 := MV1 <- V1]] end), (case V2 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V2)):16/big>>, [ [?int32(MK2), ?string(MV2)] || MK2 := MV2 <- V2]] end), (case V3 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V3)):16/big>>, [ [?string(MK3), subEncode(MV3)] || MK3 := MV3 <- V3]] end), (case V4 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V4)):16/big>>, [ [?string(MK4), subEncode(MV4)] || MK4 := MV4 <- V4]] end), (case V5 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V5)):16/big>>, [ [?string(MK5), ?string(MV5)] || MK5 := MV5 <- V5]] end)];
encodeIol(tBenchOneof, {_, V1, V2, V3, V4, V5}) ->
	[<<3005:16/big-unsigned>>, ?int32(V1), (case V2 of undefined -> <<0:8>>; {gold, OV1} -> [<<1:8>>, ?int32(OV1)]; {coupon, OV2} -> [<<2:8>>, ?string(OV2)]; {leaf, OV3} -> [<<3:8>>, subEncode(OV3)]; {phone, OV4} -> [<<4:8>>, subEncode(OV4)]; _ -> throw({invalid_oneof, V2}) end), (case V3 of undefined -> <<0:8>>; {code, OV1} -> [<<1:8>>, ?int32(OV1)]; {note, OV2} -> [<<2:8>>, ?string(OV2)]; _ -> throw({invalid_oneof, V3}) end), (case V4 of undefined -> <<0:8>>; {raw, OV1} -> [<<1:8>>, ?binary(OV1)]; {text, OV2} -> [<<2:8>>, ?string(OV2)]; {blob, OV3} -> [<<3:8>>, subEncode(OV3)]; _ -> throw({invalid_oneof, V4}) end), ?string(V5)];
encodeIol(tBenchNested, {_, V1, V2, V3, V4, V5}) ->
	[<<3006:16/big-unsigned>>, ?record(V1), ?record(V2), ?record(V3), ?list_record(V4), ?list_record(V5)];
encodeIol(tBenchHeavy, {_, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15}) ->
	[<<3007:16/big-unsigned>>, ?int32(V1), ?string(V2), ?record(V3), ?list_record(V4), ?list_int32(V5), ?list_string(V6), ?list_binary(V7), (case V8 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V8)):16/big>>, [ [?string(MK8), ?int32(MV8)] || MK8 := MV8 <- V8]] end), (case V9 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V9)):16/big>>, [ [?int32(MK9), ?string(MV9)] || MK9 := MV9 <- V9]] end), (case V10 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V10)):16/big>>, [ [?string(MK10), subEncode(MV10)] || MK10 := MV10 <- V10]] end), (case V11 of undefined -> [<<0:16/big>>]; _ -> [<<(map_size(V11)):16/big>>, [ [?string(MK11), subEncode(MV11)] || MK11 := MV11 <- V11]] end), (case V12 of undefined -> <<0:8>>; {gold, OV1} -> [<<1:8>>, ?int32(OV1)]; {coupon, OV2} -> [<<2:8>>, ?string(OV2)]; {leaf, OV3} -> [<<3:8>>, subEncode(OV3)]; {phone, OV4} -> [<<4:8>>, subEncode(OV4)]; _ -> throw({invalid_oneof, V12}) end), (case V13 of undefined -> <<0:8>>; {code, OV1} -> [<<1:8>>, ?int32(OV1)]; {note, OV2} -> [<<2:8>>, ?string(OV2)]; _ -> throw({invalid_oneof, V13}) end), ?binary(V14), ?list_record(V15)];
encodeIol(_, _) ->
	[].

decodeRec(1, LeftBin0) ->
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	case Len1 < ?BinaryShareSize of
		true ->
			V1 = TemStrV1;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V1 = binary:copy(TemStrV1);
				_ ->
					V1 = TemStrV1
			end
	end,
	MsgRec = {test, V1},
	{MsgRec, LeftBin1};
decodeRec(2, LeftBin0) ->
	<<IsUndef1:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	case IsUndef1 of
		0 ->
			V1 = undefined,
			LeftBin2 = LeftBin1 ;
		_ ->
			{V1, LeftBin2} = decodeRec(1, LeftBin1)
	end,
	<<V2:32/big-signed, LeftBin3/binary>> = LeftBin2,
	MsgRec = {phoneNumber, V1, V2},
	{MsgRec, LeftBin3};
decodeRec(3, LeftBin0) ->
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	case Len1 < ?BinaryShareSize of
		true ->
			V1 = TemStrV1;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V1 = binary:copy(TemStrV1);
				_ ->
					V1 = TemStrV1
			end
	end,
	<<V2:32/big-signed, LeftBin2/binary>> = LeftBin1,
	<<Len2:16/big-unsigned, TemStrV3:Len2/binary, LeftBin3/binary>> = LeftBin2,
	case Len2 < ?BinaryShareSize of
		true ->
			V3 = TemStrV3;
		_ ->
			case RefSize / Len2 > ?BinaryCopyRatio of
				true ->
					V3 = binary:copy(TemStrV3);
				_ ->
					V3 = TemStrV3
			end
	end,
	<<Len3:16/big-unsigned, LeftBin4/binary>> = LeftBin3,
	{V4, LeftBin5} = deRecordList(Len3, 2, LeftBin4, []),
	MsgRec = {person, V1, V2, V3, V4},
	{MsgRec, LeftBin5};
decodeRec(5, LeftBin0) ->
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	case Len1 < ?BinaryShareSize of
		true ->
			V1 = TemStrV1;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V1 = binary:copy(TemStrV1);
				_ ->
					V1 = TemStrV1
			end
	end,
	<<V2:32/big-signed, LeftBin2/binary>> = LeftBin1,
	MsgRec = {union, V1, V2},
	{MsgRec, LeftBin2};
decodeRec(2001, LeftBin0) ->
	<<V1:32/big-signed, LeftBin1/binary>> = LeftBin0,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV2:Len1/binary, LeftBin2/binary>> = LeftBin1,
	case Len1 < ?BinaryShareSize of
		true ->
			V2 = TemStrV2;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V2 = binary:copy(TemStrV2);
				_ ->
					V2 = TemStrV2
			end
	end,
	MsgRec = {comboLeaf, V1, V2},
	{MsgRec, LeftBin2};
decodeRec(2002, LeftBin0) ->
	<<IsUndef1:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	case IsUndef1 of
		0 ->
			V1 = undefined,
			LeftBin2 = LeftBin1 ;
		_ ->
			{V1, LeftBin2} = decodeRec(2001, LeftBin1)
	end,
	<<Len1:16/big-unsigned, LeftBin3/binary>> = LeftBin2,
	{V2, LeftBin4} = deRecordList(Len1, 2001, LeftBin3, []),
	<<Len2:16/big-unsigned, LeftBin5/binary>> = LeftBin4,
	{V3, LeftBin6} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len3:16/big-unsigned, MK3:Len3/binary, MapBin1/binary>> = MapBin, <<MV3:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK3, MV3, MapAcc)) end)(Len2, LeftBin5, #{}),
	MsgRec = {comboNode, V1, V2, V3},
	{MsgRec, LeftBin6};
decodeRec(2003, LeftBin0) ->
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	case Len1 < ?BinaryShareSize of
		true ->
			V1 = TemStrV1;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V1 = binary:copy(TemStrV1);
				_ ->
					V1 = TemStrV1
			end
	end,
	<<IsUndef1:8/big-unsigned, LeftBin2/binary>> = LeftBin1,
	case IsUndef1 of
		0 ->
			V2 = undefined,
			LeftBin3 = LeftBin2 ;
		_ ->
			{V2, LeftBin3} = decodeRec(2002, LeftBin2)
	end,
	<<Len2:16/big-unsigned, LeftBin4/binary>> = LeftBin3,
	{V3, LeftBin5} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK3:32/big-signed, MapBin1/binary>> = MapBin, <<Len3:16/big-unsigned, MV3:Len3/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK3, MV3, MapAcc)) end)(Len2, LeftBin4, #{}),
	MsgRec = {comboWrap, V1, V2, V3},
	{MsgRec, LeftBin5};
decodeRec(3001, LeftBin0) ->
	<<V1:32/big-signed, LeftBin1/binary>> = LeftBin0,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV2:Len1/binary, LeftBin2/binary>> = LeftBin1,
	case Len1 < ?BinaryShareSize of
		true ->
			V2 = TemStrV2;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V2 = binary:copy(TemStrV2);
				_ ->
					V2 = TemStrV2
			end
	end,
	<<Len2:16/big-unsigned, TemStrV3:Len2/binary, LeftBin3/binary>> = LeftBin2,
	case Len2 < ?BinaryShareSize of
		true ->
			V3 = TemStrV3;
		_ ->
			case RefSize / Len2 > ?BinaryCopyRatio of
				true ->
					V3 = binary:copy(TemStrV3);
				_ ->
					V3 = TemStrV3
			end
	end,
	MsgRec = {benchLeaf, V1, V2, V3},
	{MsgRec, LeftBin3};
decodeRec(_, _) ->
	{{}, <<>>}.

decodeBin(1, LeftBin0) ->
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	case Len1 < ?BinaryShareSize of
		true ->
			V1 = TemStrV1;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V1 = binary:copy(TemStrV1);
				_ ->
					V1 = TemStrV1
			end
	end,
	{testHer, {test, V1}};
decodeBin(2, LeftBin0) ->
	<<IsUndef1:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	case IsUndef1 of
		0 ->
			V1 = undefined,
			LeftBin2 = LeftBin1 ;
		_ ->
			{V1, LeftBin2} = decodeRec(1, LeftBin1)
	end,
	<<V2:32/big-signed, LeftBin3/binary>> = LeftBin2,
	{testHer, {phoneNumber, V1, V2}};
decodeBin(3, LeftBin0) ->
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	case Len1 < ?BinaryShareSize of
		true ->
			V1 = TemStrV1;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V1 = binary:copy(TemStrV1);
				_ ->
					V1 = TemStrV1
			end
	end,
	<<V2:32/big-signed, LeftBin2/binary>> = LeftBin1,
	<<Len2:16/big-unsigned, TemStrV3:Len2/binary, LeftBin3/binary>> = LeftBin2,
	case Len2 < ?BinaryShareSize of
		true ->
			V3 = TemStrV3;
		_ ->
			case RefSize / Len2 > ?BinaryCopyRatio of
				true ->
					V3 = binary:copy(TemStrV3);
				_ ->
					V3 = TemStrV3
			end
	end,
	<<Len3:16/big-unsigned, LeftBin4/binary>> = LeftBin3,
	{V4, LeftBin5} = deRecordList(Len3, 2, LeftBin4, []),
	{testHer, {person, V1, V2, V3, V4}};
decodeBin(4, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = deRecordList(Len1, 3, LeftBin1, []),
	<<Len2:16/big-unsigned, LeftBin3/binary>> = LeftBin2,
	{V2, LeftBin4} = deRecordList(Len2, 3, LeftBin3, []),
	{testHer, {addressBook, V1, V2}};
decodeBin(5, LeftBin0) ->
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	case Len1 < ?BinaryShareSize of
		true ->
			V1 = TemStrV1;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V1 = binary:copy(TemStrV1);
				_ ->
					V1 = TemStrV1
			end
	end,
	<<V2:32/big-signed, LeftBin2/binary>> = LeftBin1,
	{testHer, {union, V1, V2}};
decodeBin(6, LeftBin0) ->
	<<Bool1:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	V1 = Bool1 =:= 1,
	{testHer, {tbool, V1}};
decodeBin(7, LeftBin0) ->
	<<V1:8/big-signed, V2:8/big-signed, LeftBin1/binary>> = LeftBin0,
	{testHer, {tint8, V1, V2}};
decodeBin(8, LeftBin0) ->
	<<V1:8/big-unsigned, V2:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{testHer, {tuint8, V1, V2}};
decodeBin(9, LeftBin0) ->
	<<V1:16/big-signed, V2:16/big-signed, LeftBin1/binary>> = LeftBin0,
	{testHer, {tint16, V1, V2}};
decodeBin(10, LeftBin0) ->
	<<V1:16/big-unsigned, V2:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{testHer, {tuint16, V1, V2}};
decodeBin(11, LeftBin0) ->
	<<V1:32/big-signed, V2:32/big-signed, V3:32/big-signed, V4:32/big-signed, V5:32/big-signed, V6:32/big-signed, V7:32/big-signed, V8:32/big-signed, V9:32/big-signed, V10:32/big-signed, LeftBin1/binary>> = LeftBin0,
	{testHer, {tint32, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10}};
decodeBin(12, LeftBin0) ->
	<<V1:32/big-unsigned, V2:32/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{testHer, {tuint32, V1, V2}};
decodeBin(13, LeftBin0) ->
	<<V1:64/big-signed, V2:64/big-signed, LeftBin1/binary>> = LeftBin0,
	{testHer, {tint64, V1, V2}};
decodeBin(14, LeftBin0) ->
	<<V1:64/big-unsigned, V2:64/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{testHer, {tuint64, V1, V2}};
decodeBin(15, LeftBin0) ->
	<<IntBits1:8, V1:IntBits1/big-signed, IntBits2:8, V2:IntBits2/big-signed, IntBits3:8, V3:IntBits3/big-signed, IntBits4:8, V4:IntBits4/big-signed, IntBits5:8, V5:IntBits5/big-signed, IntBits6:8, V6:IntBits6/big-signed, IntBits7:8, V7:IntBits7/big-signed, IntBits8:8, V8:IntBits8/big-signed, LeftBin1/binary>> = LeftBin0,
	{testHer, {tinteger, V1, V2, V3, V4, V5, V6, V7, V8}};
decodeBin(16, LeftBin0) ->
	<<NumBits1:8, LeftBin1/binary>> = LeftBin0,
	case NumBits1 of
		33-> 
			<<V1:32/big-float, LeftBin2/binary>> = LeftBin1;
		65 ->
			<<V1:64/big-float, LeftBin2/binary>> = LeftBin1;
		_ ->
			<<V1:NumBits1/big-signed, LeftBin2/binary>> = LeftBin1
	end,
	<<NumBits2:8, LeftBin3/binary>> = LeftBin2,
	case NumBits2 of
		33-> 
			<<V2:32/big-float, LeftBin4/binary>> = LeftBin3;
		65 ->
			<<V2:64/big-float, LeftBin4/binary>> = LeftBin3;
		_ ->
			<<V2:NumBits2/big-signed, LeftBin4/binary>> = LeftBin3
	end,
	<<NumBits3:8, LeftBin5/binary>> = LeftBin4,
	case NumBits3 of
		33-> 
			<<V3:32/big-float, LeftBin6/binary>> = LeftBin5;
		65 ->
			<<V3:64/big-float, LeftBin6/binary>> = LeftBin5;
		_ ->
			<<V3:NumBits3/big-signed, LeftBin6/binary>> = LeftBin5
	end,
	<<NumBits4:8, LeftBin7/binary>> = LeftBin6,
	case NumBits4 of
		33-> 
			<<V4:32/big-float, LeftBin8/binary>> = LeftBin7;
		65 ->
			<<V4:64/big-float, LeftBin8/binary>> = LeftBin7;
		_ ->
			<<V4:NumBits4/big-signed, LeftBin8/binary>> = LeftBin7
	end,
	<<NumBits5:8, LeftBin9/binary>> = LeftBin8,
	case NumBits5 of
		33-> 
			<<V5:32/big-float, LeftBin10/binary>> = LeftBin9;
		65 ->
			<<V5:64/big-float, LeftBin10/binary>> = LeftBin9;
		_ ->
			<<V5:NumBits5/big-signed, LeftBin10/binary>> = LeftBin9
	end,
	<<NumBits6:8, LeftBin11/binary>> = LeftBin10,
	case NumBits6 of
		33-> 
			<<V6:32/big-float, LeftBin12/binary>> = LeftBin11;
		65 ->
			<<V6:64/big-float, LeftBin12/binary>> = LeftBin11;
		_ ->
			<<V6:NumBits6/big-signed, LeftBin12/binary>> = LeftBin11
	end,
	<<NumBits7:8, LeftBin13/binary>> = LeftBin12,
	case NumBits7 of
		33-> 
			<<V7:32/big-float, LeftBin14/binary>> = LeftBin13;
		65 ->
			<<V7:64/big-float, LeftBin14/binary>> = LeftBin13;
		_ ->
			<<V7:NumBits7/big-signed, LeftBin14/binary>> = LeftBin13
	end,
	<<NumBits8:8, LeftBin15/binary>> = LeftBin14,
	case NumBits8 of
		33-> 
			<<V8:32/big-float, LeftBin16/binary>> = LeftBin15;
		65 ->
			<<V8:64/big-float, LeftBin16/binary>> = LeftBin15;
		_ ->
			<<V8:NumBits8/big-signed, LeftBin16/binary>> = LeftBin15
	end,
	<<NumBits9:8, LeftBin17/binary>> = LeftBin16,
	case NumBits9 of
		33-> 
			<<V9:32/big-float, LeftBin18/binary>> = LeftBin17;
		65 ->
			<<V9:64/big-float, LeftBin18/binary>> = LeftBin17;
		_ ->
			<<V9:NumBits9/big-signed, LeftBin18/binary>> = LeftBin17
	end,
	<<NumBits10:8, LeftBin19/binary>> = LeftBin18,
	case NumBits10 of
		33-> 
			<<V10:32/big-float, LeftBin20/binary>> = LeftBin19;
		65 ->
			<<V10:64/big-float, LeftBin20/binary>> = LeftBin19;
		_ ->
			<<V10:NumBits10/big-signed, LeftBin20/binary>> = LeftBin19
	end,
	{testHer, {tnumber, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10}};
decodeBin(17, LeftBin0) ->
	<<V1:32/big-float, V2:32/big-float, LeftBin1/binary>> = LeftBin0,
	{testHer, {tfloat, V1, V2}};
decodeBin(18, LeftBin0) ->
	<<V1:64/big-float, V2:64/big-float, LeftBin1/binary>> = LeftBin0,
	{testHer, {tdouble, V1, V2}};
decodeBin(19, LeftBin0) ->
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	case Len1 < ?BinaryShareSize of
		true ->
			V1 = TemStrV1;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V1 = binary:copy(TemStrV1);
				_ ->
					V1 = TemStrV1
			end
	end,
	<<Len2:16/big-unsigned, TemStrV2:Len2/binary, LeftBin2/binary>> = LeftBin1,
	case Len2 < ?BinaryShareSize of
		true ->
			V2 = TemStrV2;
		_ ->
			case RefSize / Len2 > ?BinaryCopyRatio of
				true ->
					V2 = binary:copy(TemStrV2);
				_ ->
					V2 = TemStrV2
			end
	end,
	{testHer, {tstring, V1, V2}};
decodeBin(20, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:8, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV =:= 1 || <<TemV:8/big-unsigned>> <= ListBin1],
	{testHer, {tlistbool, V1}};
decodeBin(21, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:8, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:8/big-signed>> <= ListBin1],
	{testHer, {tlistint8, V1}};
decodeBin(22, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:8, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:8/big-unsigned>> <= ListBin1],
	{testHer, {tlistuint8, V1}};
decodeBin(23, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:16, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:16/big-signed>> <= ListBin1],
	{testHer, {tlistint16, V1}};
decodeBin(24, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:16, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:16/big-unsigned>> <= ListBin1],
	{testHer, {tlistuint16, V1}};
decodeBin(25, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:32, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:32/big-signed>> <= ListBin1],
	{testHer, {tlistint32, V1}};
decodeBin(26, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:32, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:32/big-unsigned>> <= ListBin1],
	{testHer, {tlistuint32, V1}};
decodeBin(27, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:64, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:64/big-signed>> <= ListBin1],
	{testHer, {tlistint64, V1}};
decodeBin(28, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:64, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:64/big-unsigned>> <= ListBin1],
	{testHer, {tlistuint64, V1}};
decodeBin(29, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = deIntegerList(Len1, LeftBin1, []),
	{testHer, {tlistinteger, V1}};
decodeBin(30, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = deNumberList(Len1, LeftBin1, []),
	{testHer, {tlistnumber, V1}};
decodeBin(31, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:32, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:32/big-float>> <= ListBin1],
	{testHer, {tlistfloat, V1}};
decodeBin(32, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:64, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:64/big-float>> <= ListBin1],
	{testHer, {tlistdouble, V1}};
decodeBin(33, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	RefSize = binary:referenced_byte_size(LeftBin0),
	{V1, LeftBin2} = deStringList(Len1, LeftBin1, RefSize, []),
	{testHer, {tliststring, V1}};
decodeBin(34, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = deRecordList(Len1, 5, LeftBin1, []),
	{testHer, {tlistunion, V1}};
decodeBin(35, LeftBin0) ->
	<<Bool1:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	V1 = Bool1 =:= 1,
	<<V2:8/big-signed, V3:8/big-unsigned, V4:16/big-signed, V5:16/big-unsigned, V6:32/big-signed, V7:32/big-unsigned, V8:64/big-signed, V9:64/big-unsigned, IntBits1:8, V10:IntBits1/big-signed, IntBits2:8, V11:IntBits2/big-signed, IntBits3:8, V12:IntBits3/big-signed, IntBits4:8, V13:IntBits4/big-signed, IntBits5:8, V14:IntBits5/big-signed, IntBits6:8, V15:IntBits6/big-signed, IntBits7:8, V16:IntBits7/big-signed, IntBits8:8, V17:IntBits8/big-signed, LeftBin2/binary>> = LeftBin1,
	<<NumBits1:8, LeftBin3/binary>> = LeftBin2,
	case NumBits1 of
		33-> 
			<<V18:32/big-float, LeftBin4/binary>> = LeftBin3;
		65 ->
			<<V18:64/big-float, LeftBin4/binary>> = LeftBin3;
		_ ->
			<<V18:NumBits1/big-signed, LeftBin4/binary>> = LeftBin3
	end,
	<<NumBits2:8, LeftBin5/binary>> = LeftBin4,
	case NumBits2 of
		33-> 
			<<V19:32/big-float, LeftBin6/binary>> = LeftBin5;
		65 ->
			<<V19:64/big-float, LeftBin6/binary>> = LeftBin5;
		_ ->
			<<V19:NumBits2/big-signed, LeftBin6/binary>> = LeftBin5
	end,
	<<NumBits3:8, LeftBin7/binary>> = LeftBin6,
	case NumBits3 of
		33-> 
			<<V20:32/big-float, LeftBin8/binary>> = LeftBin7;
		65 ->
			<<V20:64/big-float, LeftBin8/binary>> = LeftBin7;
		_ ->
			<<V20:NumBits3/big-signed, LeftBin8/binary>> = LeftBin7
	end,
	<<NumBits4:8, LeftBin9/binary>> = LeftBin8,
	case NumBits4 of
		33-> 
			<<V21:32/big-float, LeftBin10/binary>> = LeftBin9;
		65 ->
			<<V21:64/big-float, LeftBin10/binary>> = LeftBin9;
		_ ->
			<<V21:NumBits4/big-signed, LeftBin10/binary>> = LeftBin9
	end,
	<<NumBits5:8, LeftBin11/binary>> = LeftBin10,
	case NumBits5 of
		33-> 
			<<V22:32/big-float, LeftBin12/binary>> = LeftBin11;
		65 ->
			<<V22:64/big-float, LeftBin12/binary>> = LeftBin11;
		_ ->
			<<V22:NumBits5/big-signed, LeftBin12/binary>> = LeftBin11
	end,
	<<NumBits6:8, LeftBin13/binary>> = LeftBin12,
	case NumBits6 of
		33-> 
			<<V23:32/big-float, LeftBin14/binary>> = LeftBin13;
		65 ->
			<<V23:64/big-float, LeftBin14/binary>> = LeftBin13;
		_ ->
			<<V23:NumBits6/big-signed, LeftBin14/binary>> = LeftBin13
	end,
	<<NumBits7:8, LeftBin15/binary>> = LeftBin14,
	case NumBits7 of
		33-> 
			<<V24:32/big-float, LeftBin16/binary>> = LeftBin15;
		65 ->
			<<V24:64/big-float, LeftBin16/binary>> = LeftBin15;
		_ ->
			<<V24:NumBits7/big-signed, LeftBin16/binary>> = LeftBin15
	end,
	<<NumBits8:8, LeftBin17/binary>> = LeftBin16,
	case NumBits8 of
		33-> 
			<<V25:32/big-float, LeftBin18/binary>> = LeftBin17;
		65 ->
			<<V25:64/big-float, LeftBin18/binary>> = LeftBin17;
		_ ->
			<<V25:NumBits8/big-signed, LeftBin18/binary>> = LeftBin17
	end,
	<<NumBits9:8, LeftBin19/binary>> = LeftBin18,
	case NumBits9 of
		33-> 
			<<V26:32/big-float, LeftBin20/binary>> = LeftBin19;
		65 ->
			<<V26:64/big-float, LeftBin20/binary>> = LeftBin19;
		_ ->
			<<V26:NumBits9/big-signed, LeftBin20/binary>> = LeftBin19
	end,
	<<NumBits10:8, LeftBin21/binary>> = LeftBin20,
	case NumBits10 of
		33-> 
			<<V27:32/big-float, LeftBin22/binary>> = LeftBin21;
		65 ->
			<<V27:64/big-float, LeftBin22/binary>> = LeftBin21;
		_ ->
			<<V27:NumBits10/big-signed, LeftBin22/binary>> = LeftBin21
	end,
	<<V28:32/big-float, V29:64/big-float, LeftBin23/binary>> = LeftBin22,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV30:Len1/binary, LeftBin24/binary>> = LeftBin23,
	case Len1 < ?BinaryShareSize of
		true ->
			V30 = TemStrV30;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V30 = binary:copy(TemStrV30);
				_ ->
					V30 = TemStrV30
			end
	end,
	<<Len2:16/big-unsigned, TemStrV31:Len2/binary, LeftBin25/binary>> = LeftBin24,
	case Len2 < ?BinaryShareSize of
		true ->
			V31 = TemStrV31;
		_ ->
			case RefSize / Len2 > ?BinaryCopyRatio of
				true ->
					V31 = binary:copy(TemStrV31);
				_ ->
					V31 = TemStrV31
			end
	end,
	<<IsUndef1:8/big-unsigned, LeftBin26/binary>> = LeftBin25,
	case IsUndef1 of
		0 ->
			V32 = undefined,
			LeftBin27 = LeftBin26 ;
		_ ->
			{V32, LeftBin27} = decodeRec(5, LeftBin26)
	end,
	<<Len3:16/big-unsigned, LeftBin28/binary>> = LeftBin27,
	<<ListBin1:Len3/big-binary-unit:8, LeftBin29/binary>> = LeftBin28,
	V33 = [TemV =:= 1 || <<TemV:8/big-unsigned>> <= ListBin1],
	<<Len4:16/big-unsigned, LeftBin30/binary>> = LeftBin29,
	<<ListBin2:Len4/big-binary-unit:8, LeftBin31/binary>> = LeftBin30,
	V34 = [TemV || <<TemV:8/big-signed>> <= ListBin2],
	<<Len5:16/big-unsigned, LeftBin32/binary>> = LeftBin31,
	<<ListBin3:Len5/big-binary-unit:8, LeftBin33/binary>> = LeftBin32,
	V35 = [TemV || <<TemV:8/big-unsigned>> <= ListBin3],
	<<Len6:16/big-unsigned, LeftBin34/binary>> = LeftBin33,
	<<ListBin4:Len6/big-binary-unit:16, LeftBin35/binary>> = LeftBin34,
	V36 = [TemV || <<TemV:16/big-signed>> <= ListBin4],
	<<Len7:16/big-unsigned, LeftBin36/binary>> = LeftBin35,
	<<ListBin5:Len7/big-binary-unit:16, LeftBin37/binary>> = LeftBin36,
	V37 = [TemV || <<TemV:16/big-unsigned>> <= ListBin5],
	<<Len8:16/big-unsigned, LeftBin38/binary>> = LeftBin37,
	<<ListBin6:Len8/big-binary-unit:32, LeftBin39/binary>> = LeftBin38,
	V38 = [TemV || <<TemV:32/big-signed>> <= ListBin6],
	<<Len9:16/big-unsigned, LeftBin40/binary>> = LeftBin39,
	<<ListBin7:Len9/big-binary-unit:32, LeftBin41/binary>> = LeftBin40,
	V39 = [TemV || <<TemV:32/big-unsigned>> <= ListBin7],
	<<Len10:16/big-unsigned, LeftBin42/binary>> = LeftBin41,
	<<ListBin8:Len10/big-binary-unit:64, LeftBin43/binary>> = LeftBin42,
	V40 = [TemV || <<TemV:64/big-signed>> <= ListBin8],
	<<Len11:16/big-unsigned, LeftBin44/binary>> = LeftBin43,
	<<ListBin9:Len11/big-binary-unit:64, LeftBin45/binary>> = LeftBin44,
	V41 = [TemV || <<TemV:64/big-unsigned>> <= ListBin9],
	<<Len12:16/big-unsigned, LeftBin46/binary>> = LeftBin45,
	{V42, LeftBin47} = deIntegerList(Len12, LeftBin46, []),
	<<Len13:16/big-unsigned, LeftBin48/binary>> = LeftBin47,
	{V43, LeftBin49} = deIntegerList(Len13, LeftBin48, []),
	<<Len14:16/big-unsigned, LeftBin50/binary>> = LeftBin49,
	{V44, LeftBin51} = deIntegerList(Len14, LeftBin50, []),
	<<Len15:16/big-unsigned, LeftBin52/binary>> = LeftBin51,
	{V45, LeftBin53} = deIntegerList(Len15, LeftBin52, []),
	<<Len16:16/big-unsigned, LeftBin54/binary>> = LeftBin53,
	{V46, LeftBin55} = deNumberList(Len16, LeftBin54, []),
	<<Len17:16/big-unsigned, LeftBin56/binary>> = LeftBin55,
	{V47, LeftBin57} = deNumberList(Len17, LeftBin56, []),
	<<Len18:16/big-unsigned, LeftBin58/binary>> = LeftBin57,
	{V48, LeftBin59} = deNumberList(Len18, LeftBin58, []),
	<<Len19:16/big-unsigned, LeftBin60/binary>> = LeftBin59,
	{V49, LeftBin61} = deNumberList(Len19, LeftBin60, []),
	<<Len20:16/big-unsigned, LeftBin62/binary>> = LeftBin61,
	{V50, LeftBin63} = deNumberList(Len20, LeftBin62, []),
	<<Len21:16/big-unsigned, LeftBin64/binary>> = LeftBin63,
	{V51, LeftBin65} = deNumberList(Len21, LeftBin64, []),
	<<Len22:16/big-unsigned, LeftBin66/binary>> = LeftBin65,
	<<ListBin20:Len22/big-binary-unit:32, LeftBin67/binary>> = LeftBin66,
	V52 = [TemV || <<TemV:32/big-float>> <= ListBin20],
	<<Len23:16/big-unsigned, LeftBin68/binary>> = LeftBin67,
	<<ListBin21:Len23/big-binary-unit:64, LeftBin69/binary>> = LeftBin68,
	V53 = [TemV || <<TemV:64/big-float>> <= ListBin21],
	<<Len24:16/big-unsigned, LeftBin70/binary>> = LeftBin69,
	{V54, LeftBin71} = deStringList(Len24, LeftBin70, RefSize, []),
	<<Len25:16/big-unsigned, LeftBin72/binary>> = LeftBin71,
	{V55, LeftBin73} = deRecordList(Len25, 5, LeftBin72, []),
	{testHer, {allType, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54, V55}};
decodeBin(36, LeftBin0) ->
	{testHer, {testnull}};
decodeBin(37, LeftBin0) ->
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	case Len1 < ?BinaryShareSize of
		true ->
			V1 = TemStrV1;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V1 = binary:copy(TemStrV1);
				_ ->
					V1 = TemStrV1
			end
	end,
	<<Len2:16/big-unsigned, LeftBin2/binary>> = LeftBin1,
	{V2, LeftBin3} = deStringList(Len2, LeftBin2, RefSize, []),
	{testHer, {tbinary, V1, V2}};
decodeBin(38, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len2:16/big-unsigned, MK1:Len2/binary, MapBin1/binary>> = MapBin, <<MV1:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK1, MV1, MapAcc)) end)(Len1, LeftBin1, #{}),
	<<Len3:16/big-unsigned, LeftBin3/binary>> = LeftBin2,
	{V2, LeftBin4} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK2:32/big-signed, MapBin1/binary>> = MapBin, <<Len4:16/big-unsigned, MV2:Len4/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK2, MV2, MapAcc)) end)(Len3, LeftBin3, #{}),
	<<Len5:16/big-unsigned, LeftBin5/binary>> = LeftBin4,
	{V3, LeftBin6} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len6:16/big-unsigned, MK3:Len6/binary, MapBin1/binary>> = MapBin, {MV3, MapBin2} = decodeRec(2, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK3, MV3, MapAcc)) end)(Len5, LeftBin5, #{}),
	{testHer, {tmap, V1, V2, V3}};
decodeBin(39, LeftBin0) ->
	<<V1:32/big-signed, LeftBin1/binary>> = LeftBin0,
	<<OneofTag1:8/big-unsigned, LeftBin2/binary>> = LeftBin1,
	case OneofTag1 of
		0 ->
			V2 = undefined,
			LeftBin3 = LeftBin2;
		1 ->
			<<TemV2:32/big-signed, LeftBin3/binary>> = LeftBin2,
			V2 = {gold, TemV2};
		2 ->
			<<Len1:16/big-unsigned, TemV2:Len1/binary, LeftBin3/binary>> = LeftBin2,
			V2 = {coupon, TemV2};
		3 ->
			{TemV2, LeftBin3} = decodeRec(2, LeftBin2),
			V2 = {phone, TemV2};
		_ ->
			V2 = undefined,
			LeftBin3 = LeftBin2,
			throw({invalid_oneof_tag, OneofTag1})
	end,
	<<OneofTag2:8/big-unsigned, LeftBin4/binary>> = LeftBin3,
	case OneofTag2 of
		0 ->
			V3 = undefined,
			LeftBin5 = LeftBin4;
		1 ->
			<<TemV3:32/big-signed, LeftBin5/binary>> = LeftBin4,
			V3 = {gold, TemV3};
		2 ->
			<<Len2:16/big-unsigned, TemV3:Len2/binary, LeftBin5/binary>> = LeftBin4,
			V3 = {coupon, TemV3};
		3 ->
			{TemV3, LeftBin5} = decodeRec(2, LeftBin4),
			V3 = {phone, TemV3};
		_ ->
			V3 = undefined,
			LeftBin5 = LeftBin4,
			throw({invalid_oneof_tag, OneofTag2})
	end,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len3:16/big-unsigned, TemStrV4:Len3/binary, LeftBin6/binary>> = LeftBin5,
	case Len3 < ?BinaryShareSize of
		true ->
			V4 = TemStrV4;
		_ ->
			case RefSize / Len3 > ?BinaryCopyRatio of
				true ->
					V4 = binary:copy(TemStrV4);
				_ ->
					V4 = TemStrV4
			end
	end,
	{testHer, {toneof, V1, V2, V3, V4}};
decodeBin(1001, LeftBin0) ->
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	case Len1 < ?BinaryShareSize of
		true ->
			V1 = TemStrV1;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V1 = binary:copy(TemStrV1);
				_ ->
					V1 = TemStrV1
			end
	end,
	<<V2:32/big-signed, LeftBin2/binary>> = LeftBin1,
	<<Len2:16/big-unsigned, TemStrV3:Len2/binary, LeftBin3/binary>> = LeftBin2,
	case Len2 < ?BinaryShareSize of
		true ->
			V3 = TemStrV3;
		_ ->
			case RefSize / Len2 > ?BinaryCopyRatio of
				true ->
					V3 = binary:copy(TemStrV3);
				_ ->
					V3 = TemStrV3
			end
	end,
	<<Len3:16/big-unsigned, LeftBin4/binary>> = LeftBin3,
	{V4, LeftBin5} = deRecordList(Len3, 2, LeftBin4, []),
	{errorHer, {person1, V1, V2, V3, V4}};
decodeBin(2001, LeftBin0) ->
	<<V1:32/big-signed, LeftBin1/binary>> = LeftBin0,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV2:Len1/binary, LeftBin2/binary>> = LeftBin1,
	case Len1 < ?BinaryShareSize of
		true ->
			V2 = TemStrV2;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V2 = binary:copy(TemStrV2);
				_ ->
					V2 = TemStrV2
			end
	end,
	{comboHer, {comboLeaf, V1, V2}};
decodeBin(2002, LeftBin0) ->
	<<IsUndef1:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	case IsUndef1 of
		0 ->
			V1 = undefined,
			LeftBin2 = LeftBin1 ;
		_ ->
			{V1, LeftBin2} = decodeRec(2001, LeftBin1)
	end,
	<<Len1:16/big-unsigned, LeftBin3/binary>> = LeftBin2,
	{V2, LeftBin4} = deRecordList(Len1, 2001, LeftBin3, []),
	<<Len2:16/big-unsigned, LeftBin5/binary>> = LeftBin4,
	{V3, LeftBin6} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len3:16/big-unsigned, MK3:Len3/binary, MapBin1/binary>> = MapBin, <<MV3:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK3, MV3, MapAcc)) end)(Len2, LeftBin5, #{}),
	{comboHer, {comboNode, V1, V2, V3}};
decodeBin(2003, LeftBin0) ->
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	case Len1 < ?BinaryShareSize of
		true ->
			V1 = TemStrV1;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V1 = binary:copy(TemStrV1);
				_ ->
					V1 = TemStrV1
			end
	end,
	<<IsUndef1:8/big-unsigned, LeftBin2/binary>> = LeftBin1,
	case IsUndef1 of
		0 ->
			V2 = undefined,
			LeftBin3 = LeftBin2 ;
		_ ->
			{V2, LeftBin3} = decodeRec(2002, LeftBin2)
	end,
	<<Len2:16/big-unsigned, LeftBin4/binary>> = LeftBin3,
	{V3, LeftBin5} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK3:32/big-signed, MapBin1/binary>> = MapBin, <<Len3:16/big-unsigned, MV3:Len3/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK3, MV3, MapAcc)) end)(Len2, LeftBin4, #{}),
	{comboHer, {comboWrap, V1, V2, V3}};
decodeBin(2004, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len2:16/big-unsigned, MK1:Len2/binary, MapBin1/binary>> = MapBin, <<MV1:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK1, MV1, MapAcc)) end)(Len1, LeftBin1, #{}),
	<<Len3:16/big-unsigned, LeftBin3/binary>> = LeftBin2,
	{V2, LeftBin4} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK2:32/big-signed, MapBin1/binary>> = MapBin, <<Len4:16/big-unsigned, MV2:Len4/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK2, MV2, MapAcc)) end)(Len3, LeftBin3, #{}),
	<<Len5:16/big-unsigned, LeftBin5/binary>> = LeftBin4,
	{V3, LeftBin6} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len6:16/big-unsigned, MK3:Len6/binary, MapBin1/binary>> = MapBin, <<Len7:16/big-unsigned, MV3:Len7/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK3, MV3, MapAcc)) end)(Len5, LeftBin5, #{}),
	<<Len8:16/big-unsigned, LeftBin7/binary>> = LeftBin6,
	{V4, LeftBin8} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK4:32/big-signed, MapBin1/binary>> = MapBin, <<MV4:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK4, MV4, MapAcc)) end)(Len8, LeftBin7, #{}),
	<<Len9:16/big-unsigned, LeftBin9/binary>> = LeftBin8,
	{V5, LeftBin10} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK5:64/big-signed, MapBin1/binary>> = MapBin, <<MV5:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK5, MV5, MapAcc)) end)(Len9, LeftBin9, #{}),
	<<Len10:16/big-unsigned, LeftBin11/binary>> = LeftBin10,
	{V6, LeftBin12} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK6:32/big-unsigned, MapBin1/binary>> = MapBin, <<Len11:16/big-unsigned, MV6:Len11/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK6, MV6, MapAcc)) end)(Len10, LeftBin11, #{}),
	<<Len12:16/big-unsigned, LeftBin13/binary>> = LeftBin12,
	{V7, LeftBin14} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len13:16/big-unsigned, MK7:Len13/binary, MapBin1/binary>> = MapBin, <<Len14:16/big-unsigned, MV7:Len14/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK7, MV7, MapAcc)) end)(Len12, LeftBin13, #{}),
	<<Len15:16/big-unsigned, LeftBin15/binary>> = LeftBin14,
	{V8, LeftBin16} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK8:32/big-signed, MapBin1/binary>> = MapBin, <<Len16:16/big-unsigned, MV8:Len16/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK8, MV8, MapAcc)) end)(Len15, LeftBin15, #{}),
	<<Len17:16/big-unsigned, LeftBin17/binary>> = LeftBin16,
	{V9, LeftBin18} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len18:16/big-unsigned, MK9:Len18/binary, MapBin1/binary>> = MapBin, {MV9, MapBin2} = decodeRec(2, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK9, MV9, MapAcc)) end)(Len17, LeftBin17, #{}),
	<<Len19:16/big-unsigned, LeftBin19/binary>> = LeftBin18,
	{V10, LeftBin20} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK10:32/big-signed, MapBin1/binary>> = MapBin, {MV10, MapBin2} = decodeRec(2001, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK10, MV10, MapAcc)) end)(Len19, LeftBin19, #{}),
	{comboHer, {tMapMatrix, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10}};
decodeBin(2005, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:8, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV =:= 1 || <<TemV:8/big-unsigned>> <= ListBin1],
	<<Len2:16/big-unsigned, LeftBin3/binary>> = LeftBin2,
	<<ListBin2:Len2/big-binary-unit:8, LeftBin4/binary>> = LeftBin3,
	V2 = [TemV || <<TemV:8/big-signed>> <= ListBin2],
	<<Len3:16/big-unsigned, LeftBin5/binary>> = LeftBin4,
	<<ListBin3:Len3/big-binary-unit:8, LeftBin6/binary>> = LeftBin5,
	V3 = [TemV || <<TemV:8/big-unsigned>> <= ListBin3],
	<<Len4:16/big-unsigned, LeftBin7/binary>> = LeftBin6,
	<<ListBin4:Len4/big-binary-unit:16, LeftBin8/binary>> = LeftBin7,
	V4 = [TemV || <<TemV:16/big-signed>> <= ListBin4],
	<<Len5:16/big-unsigned, LeftBin9/binary>> = LeftBin8,
	<<ListBin5:Len5/big-binary-unit:16, LeftBin10/binary>> = LeftBin9,
	V5 = [TemV || <<TemV:16/big-unsigned>> <= ListBin5],
	<<Len6:16/big-unsigned, LeftBin11/binary>> = LeftBin10,
	<<ListBin6:Len6/big-binary-unit:32, LeftBin12/binary>> = LeftBin11,
	V6 = [TemV || <<TemV:32/big-signed>> <= ListBin6],
	<<Len7:16/big-unsigned, LeftBin13/binary>> = LeftBin12,
	<<ListBin7:Len7/big-binary-unit:32, LeftBin14/binary>> = LeftBin13,
	V7 = [TemV || <<TemV:32/big-unsigned>> <= ListBin7],
	<<Len8:16/big-unsigned, LeftBin15/binary>> = LeftBin14,
	<<ListBin8:Len8/big-binary-unit:64, LeftBin16/binary>> = LeftBin15,
	V8 = [TemV || <<TemV:64/big-signed>> <= ListBin8],
	<<Len9:16/big-unsigned, LeftBin17/binary>> = LeftBin16,
	<<ListBin9:Len9/big-binary-unit:64, LeftBin18/binary>> = LeftBin17,
	V9 = [TemV || <<TemV:64/big-unsigned>> <= ListBin9],
	<<Len10:16/big-unsigned, LeftBin19/binary>> = LeftBin18,
	{V10, LeftBin20} = deIntegerList(Len10, LeftBin19, []),
	<<Len11:16/big-unsigned, LeftBin21/binary>> = LeftBin20,
	{V11, LeftBin22} = deNumberList(Len11, LeftBin21, []),
	<<Len12:16/big-unsigned, LeftBin23/binary>> = LeftBin22,
	<<ListBin12:Len12/big-binary-unit:32, LeftBin24/binary>> = LeftBin23,
	V12 = [TemV || <<TemV:32/big-float>> <= ListBin12],
	<<Len13:16/big-unsigned, LeftBin25/binary>> = LeftBin24,
	<<ListBin13:Len13/big-binary-unit:64, LeftBin26/binary>> = LeftBin25,
	V13 = [TemV || <<TemV:64/big-float>> <= ListBin13],
	<<Len14:16/big-unsigned, LeftBin27/binary>> = LeftBin26,
	RefSize = binary:referenced_byte_size(LeftBin0),
	{V14, LeftBin28} = deStringList(Len14, LeftBin27, RefSize, []),
	<<Len15:16/big-unsigned, LeftBin29/binary>> = LeftBin28,
	{V15, LeftBin30} = deStringList(Len15, LeftBin29, RefSize, []),
	<<Len16:16/big-unsigned, LeftBin31/binary>> = LeftBin30,
	{V16, LeftBin32} = deRecordList(Len16, 5, LeftBin31, []),
	<<Len17:16/big-unsigned, LeftBin33/binary>> = LeftBin32,
	{V17, LeftBin34} = deRecordList(Len17, 2, LeftBin33, []),
	<<Len18:16/big-unsigned, LeftBin35/binary>> = LeftBin34,
	{V18, LeftBin36} = deRecordList(Len18, 2001, LeftBin35, []),
	<<Len19:16/big-unsigned, LeftBin37/binary>> = LeftBin36,
	{V19, LeftBin38} = deRecordList(Len19, 2002, LeftBin37, []),
	<<Len20:16/big-unsigned, LeftBin39/binary>> = LeftBin38,
	<<ListBin20:Len20/big-binary-unit:32, LeftBin40/binary>> = LeftBin39,
	V20 = [TemV || <<TemV:32/big-signed>> <= ListBin20],
	<<Len21:16/big-unsigned, LeftBin41/binary>> = LeftBin40,
	<<ListBin21:Len21/big-binary-unit:32, LeftBin42/binary>> = LeftBin41,
	V21 = [TemV || <<TemV:32/big-signed>> <= ListBin21],
	<<Len22:16/big-unsigned, LeftBin43/binary>> = LeftBin42,
	<<ListBin22:Len22/big-binary-unit:32, LeftBin44/binary>> = LeftBin43,
	V22 = [TemV || <<TemV:32/big-signed>> <= ListBin22],
	<<Len23:16/big-unsigned, LeftBin45/binary>> = LeftBin44,
	{V23, LeftBin46} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len24:16/big-unsigned, MK23:Len24/binary, MapBin1/binary>> = MapBin, <<MV23:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK23, MV23, MapAcc)) end)(Len23, LeftBin45, #{}),
	<<Len25:16/big-unsigned, LeftBin47/binary>> = LeftBin46,
	{V24, LeftBin48} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len26:16/big-unsigned, MK24:Len26/binary, MapBin1/binary>> = MapBin, <<MV24:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK24, MV24, MapAcc)) end)(Len25, LeftBin47, #{}),
	<<Len27:16/big-unsigned, LeftBin49/binary>> = LeftBin48,
	{V25, LeftBin50} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len28:16/big-unsigned, MK25:Len28/binary, MapBin1/binary>> = MapBin, <<MV25:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK25, MV25, MapAcc)) end)(Len27, LeftBin49, #{}),
	{comboHer, {tListMatrix, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24, V25}};
decodeBin(2006, LeftBin0) ->
	<<OneofTag1:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	case OneofTag1 of
		0 ->
			V1 = undefined,
			LeftBin2 = LeftBin1;
		1 ->
			<<BoolTemV1:8/big-unsigned, LeftBin2/binary>> = LeftBin1, TemV1 = BoolTemV1 =:= 1,
			V1 = {flag, TemV1};
		2 ->
			<<TemV1:32/big-signed, LeftBin2/binary>> = LeftBin1,
			V1 = {num, TemV1};
		3 ->
			<<Len1:16/big-unsigned, TemV1:Len1/binary, LeftBin2/binary>> = LeftBin1,
			V1 = {text, TemV1};
		4 ->
			<<Len2:16/big-unsigned, TemV1:Len2/binary, LeftBin2/binary>> = LeftBin1,
			V1 = {raw, TemV1};
		5 ->
			{TemV1, LeftBin2} = decodeRec(2, LeftBin1),
			V1 = {phone, TemV1};
		6 ->
			{TemV1, LeftBin2} = decodeRec(2001, LeftBin1),
			V1 = {leaf, TemV1};
		7 ->
			{TemV1, LeftBin2} = decodeRec(2002, LeftBin1),
			V1 = {node, TemV1};
		8 ->
			<<Len3:16/big-unsigned, ListBin1/binary>> = LeftBin1, {TemV1, LeftBin2} = (fun DeL(0, __LB, __Acc) -> {lists:reverse(__Acc), __LB}; DeL(__N, LB0TemV1, __Acc) -> <<LETemV1:32/big-signed, LB1TemV1/binary>> = LB0TemV1, DeL(__N - 1, LB1TemV1, [LETemV1 | __Acc]) end)(Len3, ListBin1, []),
			V1 = {ids, TemV1};
		9 ->
			<<Len4:16/big-unsigned, ListBin2/binary>> = LeftBin1, {TemV1, LeftBin2} = (fun DeL(0, __LB, __Acc) -> {lists:reverse(__Acc), __LB}; DeL(__N, LB0TemV1, __Acc) -> <<Len5:16/big-unsigned, LETemV1:Len5/binary, LB1TemV1/binary>> = LB0TemV1, DeL(__N - 1, LB1TemV1, [LETemV1 | __Acc]) end)(Len4, ListBin2, []),
			V1 = {tags, TemV1};
		_ ->
			V1 = undefined,
			LeftBin2 = LeftBin1,
			throw({invalid_oneof_tag, OneofTag1})
	end,
	{comboHer, {tOneofMatrix, V1}};
decodeBin(2007, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len2:16/big-unsigned, MK1:Len2/binary, MapBin1/binary>> = MapBin, {MV1, MapBin2} = decodeRec(2001, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK1, MV1, MapAcc)) end)(Len1, LeftBin1, #{}),
	<<IsUndef1:8/big-unsigned, LeftBin3/binary>> = LeftBin2,
	case IsUndef1 of
		0 ->
			V2 = undefined,
			LeftBin4 = LeftBin3 ;
		_ ->
			{V2, LeftBin4} = decodeRec(2003, LeftBin3)
	end,
	<<OneofTag1:8/big-unsigned, LeftBin5/binary>> = LeftBin4,
	case OneofTag1 of
		0 ->
			V3 = undefined,
			LeftBin6 = LeftBin5;
		1 ->
			{TemV3, LeftBin6} = decodeRec(2002, LeftBin5),
			V3 = {as_node, TemV3};
		2 ->
			{TemV3, LeftBin6} = decodeRec(2001, LeftBin5),
			V3 = {as_leaf, TemV3};
		_ ->
			V3 = undefined,
			LeftBin6 = LeftBin5,
			throw({invalid_oneof_tag, OneofTag1})
	end,
	<<Len3:16/big-unsigned, LeftBin7/binary>> = LeftBin6,
	{V4, LeftBin8} = deRecordList(Len3, 2003, LeftBin7, []),
	<<Len4:16/big-unsigned, LeftBin9/binary>> = LeftBin8,
	{V5, LeftBin10} = deRecordList(Len4, 2003, LeftBin9, []),
	<<Len5:16/big-unsigned, LeftBin11/binary>> = LeftBin10,
	{V6, LeftBin12} = deRecordList(Len5, 2003, LeftBin11, []),
	{comboHer, {tNestDeep, V1, V2, V3, V4, V5, V6}};
decodeBin(2008, LeftBin0) ->
	<<V1:32/big-signed, LeftBin1/binary>> = LeftBin0,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV2:Len1/binary, LeftBin2/binary>> = LeftBin1,
	case Len1 < ?BinaryShareSize of
		true ->
			V2 = TemStrV2;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V2 = binary:copy(TemStrV2);
				_ ->
					V2 = TemStrV2
			end
	end,
	<<Len2:16/big-unsigned, LeftBin3/binary>> = LeftBin2,
	{V3, LeftBin4} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len3:16/big-unsigned, MK3:Len3/binary, MapBin1/binary>> = MapBin, <<MV3:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK3, MV3, MapAcc)) end)(Len2, LeftBin3, #{}),
	<<Len4:16/big-unsigned, LeftBin5/binary>> = LeftBin4,
	<<ListBin1:Len4/big-binary-unit:32, LeftBin6/binary>> = LeftBin5,
	V4 = [TemV || <<TemV:32/big-signed>> <= ListBin1],
	<<OneofTag1:8/big-unsigned, LeftBin7/binary>> = LeftBin6,
	case OneofTag1 of
		0 ->
			V5 = undefined,
			LeftBin8 = LeftBin7;
		1 ->
			<<TemV5:32/big-signed, LeftBin8/binary>> = LeftBin7,
			V5 = {a, TemV5};
		2 ->
			<<Len5:16/big-unsigned, TemV5:Len5/binary, LeftBin8/binary>> = LeftBin7,
			V5 = {b, TemV5};
		_ ->
			V5 = undefined,
			LeftBin8 = LeftBin7,
			throw({invalid_oneof_tag, OneofTag1})
	end,
	<<V6:32/big-signed, LeftBin9/binary>> = LeftBin8,
	<<Len6:16/big-unsigned, TemStrV7:Len6/binary, LeftBin10/binary>> = LeftBin9,
	case Len6 < ?BinaryShareSize of
		true ->
			V7 = TemStrV7;
		_ ->
			case RefSize / Len6 > ?BinaryCopyRatio of
				true ->
					V7 = binary:copy(TemStrV7);
				_ ->
					V7 = TemStrV7
			end
	end,
	<<Len7:16/big-unsigned, LeftBin11/binary>> = LeftBin10,
	{V8, LeftBin12} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len8:16/big-unsigned, MK8:Len8/binary, MapBin1/binary>> = MapBin, <<MV8:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK8, MV8, MapAcc)) end)(Len7, LeftBin11, #{}),
	<<Len9:16/big-unsigned, LeftBin13/binary>> = LeftBin12,
	<<ListBin2:Len9/big-binary-unit:32, LeftBin14/binary>> = LeftBin13,
	V9 = [TemV || <<TemV:32/big-signed>> <= ListBin2],
	<<OneofTag2:8/big-unsigned, LeftBin15/binary>> = LeftBin14,
	case OneofTag2 of
		0 ->
			V10 = undefined,
			LeftBin16 = LeftBin15;
		1 ->
			<<TemV10:32/big-signed, LeftBin16/binary>> = LeftBin15,
			V10 = {a, TemV10};
		2 ->
			<<Len10:16/big-unsigned, TemV10:Len10/binary, LeftBin16/binary>> = LeftBin15,
			V10 = {b, TemV10};
		_ ->
			V10 = undefined,
			LeftBin16 = LeftBin15,
			throw({invalid_oneof_tag, OneofTag2})
	end,
	<<V11:32/big-signed, LeftBin17/binary>> = LeftBin16,
	<<Len11:16/big-unsigned, TemStrV12:Len11/binary, LeftBin18/binary>> = LeftBin17,
	case Len11 < ?BinaryShareSize of
		true ->
			V12 = TemStrV12;
		_ ->
			case RefSize / Len11 > ?BinaryCopyRatio of
				true ->
					V12 = binary:copy(TemStrV12);
				_ ->
					V12 = TemStrV12
			end
	end,
	<<Len12:16/big-unsigned, LeftBin19/binary>> = LeftBin18,
	{V13, LeftBin20} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len13:16/big-unsigned, MK13:Len13/binary, MapBin1/binary>> = MapBin, <<MV13:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK13, MV13, MapAcc)) end)(Len12, LeftBin19, #{}),
	<<Len14:16/big-unsigned, LeftBin21/binary>> = LeftBin20,
	<<ListBin3:Len14/big-binary-unit:32, LeftBin22/binary>> = LeftBin21,
	V14 = [TemV || <<TemV:32/big-signed>> <= ListBin3],
	<<OneofTag3:8/big-unsigned, LeftBin23/binary>> = LeftBin22,
	case OneofTag3 of
		0 ->
			V15 = undefined,
			LeftBin24 = LeftBin23;
		1 ->
			<<TemV15:32/big-signed, LeftBin24/binary>> = LeftBin23,
			V15 = {a, TemV15};
		2 ->
			<<Len15:16/big-unsigned, TemV15:Len15/binary, LeftBin24/binary>> = LeftBin23,
			V15 = {b, TemV15};
		_ ->
			V15 = undefined,
			LeftBin24 = LeftBin23,
			throw({invalid_oneof_tag, OneofTag3})
	end,
	<<Len16:16/big-unsigned, TemStrV16:Len16/binary, LeftBin25/binary>> = LeftBin24,
	case Len16 < ?BinaryShareSize of
		true ->
			V16 = TemStrV16;
		_ ->
			case RefSize / Len16 > ?BinaryCopyRatio of
				true ->
					V16 = binary:copy(TemStrV16);
				_ ->
					V16 = TemStrV16
			end
	end,
	<<Len17:16/big-unsigned, TemStrV17:Len17/binary, LeftBin26/binary>> = LeftBin25,
	case Len17 < ?BinaryShareSize of
		true ->
			V17 = TemStrV17;
		_ ->
			case RefSize / Len17 > ?BinaryCopyRatio of
				true ->
					V17 = binary:copy(TemStrV17);
				_ ->
					V17 = TemStrV17
			end
	end,
	<<Len18:16/big-unsigned, TemStrV18:Len18/binary, LeftBin27/binary>> = LeftBin26,
	case Len18 < ?BinaryShareSize of
		true ->
			V18 = TemStrV18;
		_ ->
			case RefSize / Len18 > ?BinaryCopyRatio of
				true ->
					V18 = binary:copy(TemStrV18);
				_ ->
					V18 = TemStrV18
			end
	end,
	{comboHer, {tInterleave, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18}};
decodeBin(2009, LeftBin0) ->
	<<OneofTag1:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	case OneofTag1 of
		0 ->
			V1 = undefined,
			LeftBin2 = LeftBin1;
		1 ->
			<<TemV1:32/big-signed, LeftBin2/binary>> = LeftBin1,
			V1 = {x, TemV1};
		2 ->
			<<Len1:16/big-unsigned, TemV1:Len1/binary, LeftBin2/binary>> = LeftBin1,
			V1 = {y, TemV1};
		_ ->
			V1 = undefined,
			LeftBin2 = LeftBin1,
			throw({invalid_oneof_tag, OneofTag1})
	end,
	<<V2:32/big-signed, LeftBin3/binary>> = LeftBin2,
	<<OneofTag2:8/big-unsigned, LeftBin4/binary>> = LeftBin3,
	case OneofTag2 of
		0 ->
			V3 = undefined,
			LeftBin5 = LeftBin4;
		1 ->
			<<TemV3:32/big-signed, LeftBin5/binary>> = LeftBin4,
			V3 = {x, TemV3};
		2 ->
			<<Len2:16/big-unsigned, TemV3:Len2/binary, LeftBin5/binary>> = LeftBin4,
			V3 = {y, TemV3};
		3 ->
			<<Len3:16/big-unsigned, TemV3:Len3/binary, LeftBin5/binary>> = LeftBin4,
			V3 = {z, TemV3};
		_ ->
			V3 = undefined,
			LeftBin5 = LeftBin4,
			throw({invalid_oneof_tag, OneofTag2})
	end,
	<<V4:32/big-signed, LeftBin6/binary>> = LeftBin5,
	<<OneofTag3:8/big-unsigned, LeftBin7/binary>> = LeftBin6,
	case OneofTag3 of
		0 ->
			V5 = undefined,
			LeftBin8 = LeftBin7;
		1 ->
			<<TemV5:32/big-signed, LeftBin8/binary>> = LeftBin7,
			V5 = {x, TemV5};
		2 ->
			{TemV5, LeftBin8} = decodeRec(2, LeftBin7),
			V5 = {p, TemV5};
		_ ->
			V5 = undefined,
			LeftBin8 = LeftBin7,
			throw({invalid_oneof_tag, OneofTag3})
	end,
	<<V6:32/big-signed, LeftBin9/binary>> = LeftBin8,
	<<OneofTag4:8/big-unsigned, LeftBin10/binary>> = LeftBin9,
	case OneofTag4 of
		0 ->
			V7 = undefined,
			LeftBin11 = LeftBin10;
		1 ->
			{TemV7, LeftBin11} = decodeRec(2001, LeftBin10),
			V7 = {l, TemV7};
		2 ->
			<<Len4:16/big-unsigned, ListBin1/binary>> = LeftBin10, {TemV7, LeftBin11} = (fun DeL(0, __LB, __Acc) -> {lists:reverse(__Acc), __LB}; DeL(__N, LB0TemV7, __Acc) -> <<LETemV7:32/big-signed, LB1TemV7/binary>> = LB0TemV7, DeL(__N - 1, LB1TemV7, [LETemV7 | __Acc]) end)(Len4, ListBin1, []),
			V7 = {ids, TemV7};
		_ ->
			V7 = undefined,
			LeftBin11 = LeftBin10,
			throw({invalid_oneof_tag, OneofTag4})
	end,
	<<V8:32/big-signed, LeftBin12/binary>> = LeftBin11,
	<<OneofTag5:8/big-unsigned, LeftBin13/binary>> = LeftBin12,
	case OneofTag5 of
		0 ->
			V9 = undefined,
			LeftBin14 = LeftBin13;
		1 ->
			<<Len5:16/big-unsigned, TemV9:Len5/binary, LeftBin14/binary>> = LeftBin13,
			V9 = {s, TemV9};
		2 ->
			<<TemV9:32/big-signed, LeftBin14/binary>> = LeftBin13,
			V9 = {x, TemV9};
		_ ->
			V9 = undefined,
			LeftBin14 = LeftBin13,
			throw({invalid_oneof_tag, OneofTag5})
	end,
	{comboHer, {tMultiOneof, V1, V2, V3, V4, V5, V6, V7, V8, V9}};
decodeBin(2010, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len2:16/big-unsigned, MK1:Len2/binary, MapBin1/binary>> = MapBin, <<MV1:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK1, MV1, MapAcc)) end)(Len1, LeftBin1, #{}),
	<<Len3:16/big-unsigned, LeftBin3/binary>> = LeftBin2,
	<<ListBin1:Len3/big-binary-unit:32, LeftBin4/binary>> = LeftBin3,
	V2 = [TemV || <<TemV:32/big-signed>> <= ListBin1],
	<<Len4:16/big-unsigned, LeftBin5/binary>> = LeftBin4,
	RefSize = binary:referenced_byte_size(LeftBin0),
	{V3, LeftBin6} = deStringList(Len4, LeftBin5, RefSize, []),
	<<Len5:16/big-unsigned, TemStrV4:Len5/binary, LeftBin7/binary>> = LeftBin6,
	case Len5 < ?BinaryShareSize of
		true ->
			V4 = TemStrV4;
		_ ->
			case RefSize / Len5 > ?BinaryCopyRatio of
				true ->
					V4 = binary:copy(TemStrV4);
				_ ->
					V4 = TemStrV4
			end
	end,
	<<Len6:16/big-unsigned, TemStrV5:Len6/binary, LeftBin8/binary>> = LeftBin7,
	case Len6 < ?BinaryShareSize of
		true ->
			V5 = TemStrV5;
		_ ->
			case RefSize / Len6 > ?BinaryCopyRatio of
				true ->
					V5 = binary:copy(TemStrV5);
				_ ->
					V5 = TemStrV5
			end
	end,
	<<OneofTag1:8/big-unsigned, LeftBin9/binary>> = LeftBin8,
	case OneofTag1 of
		0 ->
			V6 = undefined,
			LeftBin10 = LeftBin9;
		1 ->
			<<TemV6:32/big-signed, LeftBin10/binary>> = LeftBin9,
			V6 = {x, TemV6};
		2 ->
			<<Len7:16/big-unsigned, TemV6:Len7/binary, LeftBin10/binary>> = LeftBin9,
			V6 = {y, TemV6};
		_ ->
			V6 = undefined,
			LeftBin10 = LeftBin9,
			throw({invalid_oneof_tag, OneofTag1})
	end,
	<<IsUndef1:8/big-unsigned, LeftBin11/binary>> = LeftBin10,
	case IsUndef1 of
		0 ->
			V7 = undefined,
			LeftBin12 = LeftBin11 ;
		_ ->
			{V7, LeftBin12} = decodeRec(2, LeftBin11)
	end,
	<<IsUndef2:8/big-unsigned, LeftBin13/binary>> = LeftBin12,
	case IsUndef2 of
		0 ->
			V8 = undefined,
			LeftBin14 = LeftBin13 ;
		_ ->
			{V8, LeftBin14} = decodeRec(2001, LeftBin13)
	end,
	{comboHer, {tEmptyEdge, V1, V2, V3, V4, V5, V6, V7, V8}};
decodeBin(2011, LeftBin0) ->
	<<V1:32/big-signed, V2:32/big-signed, V3:32/big-signed, LeftBin1/binary>> = LeftBin0,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV4:Len1/binary, LeftBin2/binary>> = LeftBin1,
	case Len1 < ?BinaryShareSize of
		true ->
			V4 = TemStrV4;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V4 = binary:copy(TemStrV4);
				_ ->
					V4 = TemStrV4
			end
	end,
	<<Len2:16/big-unsigned, TemStrV5:Len2/binary, LeftBin3/binary>> = LeftBin2,
	case Len2 < ?BinaryShareSize of
		true ->
			V5 = TemStrV5;
		_ ->
			case RefSize / Len2 > ?BinaryCopyRatio of
				true ->
					V5 = binary:copy(TemStrV5);
				_ ->
					V5 = TemStrV5
			end
	end,
	<<Len3:16/big-unsigned, TemStrV6:Len3/binary, LeftBin4/binary>> = LeftBin3,
	case Len3 < ?BinaryShareSize of
		true ->
			V6 = TemStrV6;
		_ ->
			case RefSize / Len3 > ?BinaryCopyRatio of
				true ->
					V6 = binary:copy(TemStrV6);
				_ ->
					V6 = TemStrV6
			end
	end,
	<<Len4:16/big-unsigned, LeftBin5/binary>> = LeftBin4,
	{V7, LeftBin6} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len5:16/big-unsigned, MK7:Len5/binary, MapBin1/binary>> = MapBin, <<MV7:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK7, MV7, MapAcc)) end)(Len4, LeftBin5, #{}),
	<<Len6:16/big-unsigned, LeftBin7/binary>> = LeftBin6,
	{V8, LeftBin8} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len7:16/big-unsigned, MK8:Len7/binary, MapBin1/binary>> = MapBin, <<MV8:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK8, MV8, MapAcc)) end)(Len6, LeftBin7, #{}),
	<<Len8:16/big-unsigned, LeftBin9/binary>> = LeftBin8,
	{V9, LeftBin10} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len9:16/big-unsigned, MK9:Len9/binary, MapBin1/binary>> = MapBin, <<MV9:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK9, MV9, MapAcc)) end)(Len8, LeftBin9, #{}),
	{comboHer, {tScalarRun, V1, V2, V3, V4, V5, V6, V7, V8, V9}};
decodeBin(2012, LeftBin0) ->
	<<Bool1:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	V1 = Bool1 =:= 1,
	<<Bool2:8/big-unsigned, LeftBin2/binary>> = LeftBin1,
	V2 = Bool2 =:= 1,
	<<Bool3:8/big-unsigned, LeftBin3/binary>> = LeftBin2,
	V3 = Bool3 =:= 1,
	<<V4:32/big-signed, V5:32/big-signed, V6:32/big-signed, LeftBin4/binary>> = LeftBin3,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV7:Len1/binary, LeftBin5/binary>> = LeftBin4,
	case Len1 < ?BinaryShareSize of
		true ->
			V7 = TemStrV7;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V7 = binary:copy(TemStrV7);
				_ ->
					V7 = TemStrV7
			end
	end,
	<<V8:32/big-signed, V9:8/big-signed, V10:8/big-signed, V11:8/big-unsigned, V12:8/big-signed, V13:8/big-unsigned, V14:8/big-unsigned, V15:16/big-signed, V16:16/big-signed, V17:16/big-signed, V18:16/big-unsigned, LeftBin6/binary>> = LeftBin5,
	<<Len2:16/big-unsigned, TemStrV19:Len2/binary, LeftBin7/binary>> = LeftBin6,
	case Len2 < ?BinaryShareSize of
		true ->
			V19 = TemStrV19;
		_ ->
			case RefSize / Len2 > ?BinaryCopyRatio of
				true ->
					V19 = binary:copy(TemStrV19);
				_ ->
					V19 = TemStrV19
			end
	end,
	<<V20:16/big-unsigned, V21:16/big-unsigned, V22:32/big-signed, V23:32/big-signed, V24:32/big-signed, V25:32/big-unsigned, V26:32/big-unsigned, V27:32/big-unsigned, V28:64/big-signed, V29:64/big-signed, LeftBin8/binary>> = LeftBin7,
	<<Len3:16/big-unsigned, TemStrV30:Len3/binary, LeftBin9/binary>> = LeftBin8,
	case Len3 < ?BinaryShareSize of
		true ->
			V30 = TemStrV30;
		_ ->
			case RefSize / Len3 > ?BinaryCopyRatio of
				true ->
					V30 = binary:copy(TemStrV30);
				_ ->
					V30 = TemStrV30
			end
	end,
	<<V31:64/big-signed, V32:64/big-unsigned, V33:64/big-unsigned, V34:64/big-unsigned, IntBits1:8, V35:IntBits1/big-signed, IntBits2:8, V36:IntBits2/big-signed, IntBits3:8, V37:IntBits3/big-signed, LeftBin10/binary>> = LeftBin9,
	<<NumBits1:8, LeftBin11/binary>> = LeftBin10,
	case NumBits1 of
		33-> 
			<<V38:32/big-float, LeftBin12/binary>> = LeftBin11;
		65 ->
			<<V38:64/big-float, LeftBin12/binary>> = LeftBin11;
		_ ->
			<<V38:NumBits1/big-signed, LeftBin12/binary>> = LeftBin11
	end,
	<<Len4:16/big-unsigned, TemStrV39:Len4/binary, LeftBin13/binary>> = LeftBin12,
	case Len4 < ?BinaryShareSize of
		true ->
			V39 = TemStrV39;
		_ ->
			case RefSize / Len4 > ?BinaryCopyRatio of
				true ->
					V39 = binary:copy(TemStrV39);
				_ ->
					V39 = TemStrV39
			end
	end,
	<<NumBits2:8, LeftBin14/binary>> = LeftBin13,
	case NumBits2 of
		33-> 
			<<V40:32/big-float, LeftBin15/binary>> = LeftBin14;
		65 ->
			<<V40:64/big-float, LeftBin15/binary>> = LeftBin14;
		_ ->
			<<V40:NumBits2/big-signed, LeftBin15/binary>> = LeftBin14
	end,
	<<NumBits3:8, LeftBin16/binary>> = LeftBin15,
	case NumBits3 of
		33-> 
			<<V41:32/big-float, LeftBin17/binary>> = LeftBin16;
		65 ->
			<<V41:64/big-float, LeftBin17/binary>> = LeftBin16;
		_ ->
			<<V41:NumBits3/big-signed, LeftBin17/binary>> = LeftBin16
	end,
	<<V42:32/big-float, V43:32/big-float, V44:32/big-float, V45:64/big-float, V46:64/big-float, V47:64/big-float, LeftBin18/binary>> = LeftBin17,
	<<Len5:16/big-unsigned, TemStrV48:Len5/binary, LeftBin19/binary>> = LeftBin18,
	case Len5 < ?BinaryShareSize of
		true ->
			V48 = TemStrV48;
		_ ->
			case RefSize / Len5 > ?BinaryCopyRatio of
				true ->
					V48 = binary:copy(TemStrV48);
				_ ->
					V48 = TemStrV48
			end
	end,
	<<Len6:16/big-unsigned, TemStrV49:Len6/binary, LeftBin20/binary>> = LeftBin19,
	case Len6 < ?BinaryShareSize of
		true ->
			V49 = TemStrV49;
		_ ->
			case RefSize / Len6 > ?BinaryCopyRatio of
				true ->
					V49 = binary:copy(TemStrV49);
				_ ->
					V49 = TemStrV49
			end
	end,
	<<Len7:16/big-unsigned, TemStrV50:Len7/binary, LeftBin21/binary>> = LeftBin20,
	case Len7 < ?BinaryShareSize of
		true ->
			V50 = TemStrV50;
		_ ->
			case RefSize / Len7 > ?BinaryCopyRatio of
				true ->
					V50 = binary:copy(TemStrV50);
				_ ->
					V50 = TemStrV50
			end
	end,
	<<V51:32/big-signed, LeftBin22/binary>> = LeftBin21,
	<<Len8:16/big-unsigned, TemStrV52:Len8/binary, LeftBin23/binary>> = LeftBin22,
	case Len8 < ?BinaryShareSize of
		true ->
			V52 = TemStrV52;
		_ ->
			case RefSize / Len8 > ?BinaryCopyRatio of
				true ->
					V52 = binary:copy(TemStrV52);
				_ ->
					V52 = TemStrV52
			end
	end,
	<<Len9:16/big-unsigned, TemStrV53:Len9/binary, LeftBin24/binary>> = LeftBin23,
	case Len9 < ?BinaryShareSize of
		true ->
			V53 = TemStrV53;
		_ ->
			case RefSize / Len9 > ?BinaryCopyRatio of
				true ->
					V53 = binary:copy(TemStrV53);
				_ ->
					V53 = TemStrV53
			end
	end,
	<<Len10:16/big-unsigned, TemStrV54:Len10/binary, LeftBin25/binary>> = LeftBin24,
	case Len10 < ?BinaryShareSize of
		true ->
			V54 = TemStrV54;
		_ ->
			case RefSize / Len10 > ?BinaryCopyRatio of
				true ->
					V54 = binary:copy(TemStrV54);
				_ ->
					V54 = TemStrV54
			end
	end,
	<<Len11:16/big-unsigned, TemStrV55:Len11/binary, LeftBin26/binary>> = LeftBin25,
	case Len11 < ?BinaryShareSize of
		true ->
			V55 = TemStrV55;
		_ ->
			case RefSize / Len11 > ?BinaryCopyRatio of
				true ->
					V55 = binary:copy(TemStrV55);
				_ ->
					V55 = TemStrV55
			end
	end,
	<<IsUndef1:8/big-unsigned, LeftBin27/binary>> = LeftBin26,
	case IsUndef1 of
		0 ->
			V56 = undefined,
			LeftBin28 = LeftBin27 ;
		_ ->
			{V56, LeftBin28} = decodeRec(5, LeftBin27)
	end,
	<<IsUndef2:8/big-unsigned, LeftBin29/binary>> = LeftBin28,
	case IsUndef2 of
		0 ->
			V57 = undefined,
			LeftBin30 = LeftBin29 ;
		_ ->
			{V57, LeftBin30} = decodeRec(5, LeftBin29)
	end,
	<<IsUndef3:8/big-unsigned, LeftBin31/binary>> = LeftBin30,
	case IsUndef3 of
		0 ->
			V58 = undefined,
			LeftBin32 = LeftBin31 ;
		_ ->
			{V58, LeftBin32} = decodeRec(5, LeftBin31)
	end,
	<<Len12:16/big-unsigned, LeftBin33/binary>> = LeftBin32,
	<<ListBin1:Len12/big-binary-unit:32, LeftBin34/binary>> = LeftBin33,
	V59 = [TemV || <<TemV:32/big-signed>> <= ListBin1],
	<<Len13:16/big-unsigned, LeftBin35/binary>> = LeftBin34,
	<<ListBin2:Len13/big-binary-unit:32, LeftBin36/binary>> = LeftBin35,
	V60 = [TemV || <<TemV:32/big-signed>> <= ListBin2],
	<<Len14:16/big-unsigned, LeftBin37/binary>> = LeftBin36,
	<<ListBin3:Len14/big-binary-unit:32, LeftBin38/binary>> = LeftBin37,
	V61 = [TemV || <<TemV:32/big-signed>> <= ListBin3],
	<<Len15:16/big-unsigned, LeftBin39/binary>> = LeftBin38,
	{V62, LeftBin40} = deStringList(Len15, LeftBin39, RefSize, []),
	<<V63:32/big-signed, LeftBin41/binary>> = LeftBin40,
	<<Len16:16/big-unsigned, LeftBin42/binary>> = LeftBin41,
	{V64, LeftBin43} = deStringList(Len16, LeftBin42, RefSize, []),
	<<Len17:16/big-unsigned, LeftBin44/binary>> = LeftBin43,
	{V65, LeftBin45} = deStringList(Len17, LeftBin44, RefSize, []),
	<<Len18:16/big-unsigned, LeftBin46/binary>> = LeftBin45,
	{V66, LeftBin47} = deStringList(Len18, LeftBin46, RefSize, []),
	<<Len19:16/big-unsigned, LeftBin48/binary>> = LeftBin47,
	{V67, LeftBin49} = deStringList(Len19, LeftBin48, RefSize, []),
	<<Len20:16/big-unsigned, LeftBin50/binary>> = LeftBin49,
	{V68, LeftBin51} = deStringList(Len20, LeftBin50, RefSize, []),
	<<Len21:16/big-unsigned, LeftBin52/binary>> = LeftBin51,
	<<ListBin10:Len21/big-binary-unit:8, LeftBin53/binary>> = LeftBin52,
	V69 = [TemV =:= 1 || <<TemV:8/big-unsigned>> <= ListBin10],
	<<Len22:16/big-unsigned, LeftBin54/binary>> = LeftBin53,
	<<ListBin11:Len22/big-binary-unit:8, LeftBin55/binary>> = LeftBin54,
	V70 = [TemV =:= 1 || <<TemV:8/big-unsigned>> <= ListBin11],
	<<Len23:16/big-unsigned, LeftBin56/binary>> = LeftBin55,
	<<ListBin12:Len23/big-binary-unit:8, LeftBin57/binary>> = LeftBin56,
	V71 = [TemV =:= 1 || <<TemV:8/big-unsigned>> <= ListBin12],
	<<Len24:16/big-unsigned, LeftBin58/binary>> = LeftBin57,
	{V72, LeftBin59} = deIntegerList(Len24, LeftBin58, []),
	<<Len25:16/big-unsigned, LeftBin60/binary>> = LeftBin59,
	{V73, LeftBin61} = deIntegerList(Len25, LeftBin60, []),
	<<Len26:16/big-unsigned, LeftBin62/binary>> = LeftBin61,
	{V74, LeftBin63} = deIntegerList(Len26, LeftBin62, []),
	<<Len27:16/big-unsigned, LeftBin64/binary>> = LeftBin63,
	{V75, LeftBin65} = deNumberList(Len27, LeftBin64, []),
	<<Len28:16/big-unsigned, LeftBin66/binary>> = LeftBin65,
	{V76, LeftBin67} = deNumberList(Len28, LeftBin66, []),
	<<Len29:16/big-unsigned, LeftBin68/binary>> = LeftBin67,
	{V77, LeftBin69} = deNumberList(Len29, LeftBin68, []),
	<<Len30:16/big-unsigned, LeftBin70/binary>> = LeftBin69,
	{V78, LeftBin71} = deRecordList(Len30, 2, LeftBin70, []),
	<<Len31:16/big-unsigned, LeftBin72/binary>> = LeftBin71,
	{V79, LeftBin73} = deRecordList(Len31, 2, LeftBin72, []),
	<<Len32:16/big-unsigned, TemStrV80:Len32/binary, LeftBin74/binary>> = LeftBin73,
	case Len32 < ?BinaryShareSize of
		true ->
			V80 = TemStrV80;
		_ ->
			case RefSize / Len32 > ?BinaryCopyRatio of
				true ->
					V80 = binary:copy(TemStrV80);
				_ ->
					V80 = TemStrV80
			end
	end,
	<<Len33:16/big-unsigned, LeftBin75/binary>> = LeftBin74,
	{V81, LeftBin76} = deRecordList(Len33, 2, LeftBin75, []),
	<<Len34:16/big-unsigned, LeftBin77/binary>> = LeftBin76,
	{V82, LeftBin78} = deRecordList(Len34, 2001, LeftBin77, []),
	<<Len35:16/big-unsigned, LeftBin79/binary>> = LeftBin78,
	{V83, LeftBin80} = deRecordList(Len35, 2001, LeftBin79, []),
	<<Len36:16/big-unsigned, LeftBin81/binary>> = LeftBin80,
	{V84, LeftBin82} = deRecordList(Len36, 2001, LeftBin81, []),
	<<Len37:16/big-unsigned, LeftBin83/binary>> = LeftBin82,
	{V85, LeftBin84} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len38:16/big-unsigned, MK85:Len38/binary, MapBin1/binary>> = MapBin, <<MV85:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK85, MV85, MapAcc)) end)(Len37, LeftBin83, #{}),
	<<Len39:16/big-unsigned, LeftBin85/binary>> = LeftBin84,
	{V86, LeftBin86} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len40:16/big-unsigned, MK86:Len40/binary, MapBin1/binary>> = MapBin, <<MV86:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK86, MV86, MapAcc)) end)(Len39, LeftBin85, #{}),
	<<Len41:16/big-unsigned, LeftBin87/binary>> = LeftBin86,
	{V87, LeftBin88} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len42:16/big-unsigned, MK87:Len42/binary, MapBin1/binary>> = MapBin, <<MV87:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK87, MV87, MapAcc)) end)(Len41, LeftBin87, #{}),
	<<Len43:16/big-unsigned, LeftBin89/binary>> = LeftBin88,
	{V88, LeftBin90} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK88:32/big-signed, MapBin1/binary>> = MapBin, <<Len44:16/big-unsigned, MV88:Len44/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK88, MV88, MapAcc)) end)(Len43, LeftBin89, #{}),
	<<V89:32/big-signed, LeftBin91/binary>> = LeftBin90,
	<<Len45:16/big-unsigned, LeftBin92/binary>> = LeftBin91,
	{V90, LeftBin93} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK90:32/big-signed, MapBin1/binary>> = MapBin, <<Len46:16/big-unsigned, MV90:Len46/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK90, MV90, MapAcc)) end)(Len45, LeftBin92, #{}),
	<<Len47:16/big-unsigned, LeftBin94/binary>> = LeftBin93,
	{V91, LeftBin95} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK91:32/big-signed, MapBin1/binary>> = MapBin, <<Len48:16/big-unsigned, MV91:Len48/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK91, MV91, MapAcc)) end)(Len47, LeftBin94, #{}),
	<<Len49:16/big-unsigned, LeftBin96/binary>> = LeftBin95,
	{V92, LeftBin97} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len50:16/big-unsigned, MK92:Len50/binary, MapBin1/binary>> = MapBin, <<Len51:16/big-unsigned, MV92:Len51/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK92, MV92, MapAcc)) end)(Len49, LeftBin96, #{}),
	<<Len52:16/big-unsigned, LeftBin98/binary>> = LeftBin97,
	{V93, LeftBin99} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len53:16/big-unsigned, MK93:Len53/binary, MapBin1/binary>> = MapBin, <<Len54:16/big-unsigned, MV93:Len54/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK93, MV93, MapAcc)) end)(Len52, LeftBin98, #{}),
	<<Len55:16/big-unsigned, LeftBin100/binary>> = LeftBin99,
	{V94, LeftBin101} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len56:16/big-unsigned, MK94:Len56/binary, MapBin1/binary>> = MapBin, <<Len57:16/big-unsigned, MV94:Len57/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK94, MV94, MapAcc)) end)(Len55, LeftBin100, #{}),
	<<Len58:16/big-unsigned, LeftBin102/binary>> = LeftBin101,
	{V95, LeftBin103} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK95:32/big-signed, MapBin1/binary>> = MapBin, <<MV95:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK95, MV95, MapAcc)) end)(Len58, LeftBin102, #{}),
	<<Len59:16/big-unsigned, LeftBin104/binary>> = LeftBin103,
	{V96, LeftBin105} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK96:32/big-signed, MapBin1/binary>> = MapBin, <<MV96:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK96, MV96, MapAcc)) end)(Len59, LeftBin104, #{}),
	<<Len60:16/big-unsigned, LeftBin106/binary>> = LeftBin105,
	{V97, LeftBin107} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK97:32/big-signed, MapBin1/binary>> = MapBin, <<MV97:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK97, MV97, MapAcc)) end)(Len60, LeftBin106, #{}),
	<<Len61:16/big-unsigned, LeftBin108/binary>> = LeftBin107,
	{V98, LeftBin109} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len62:16/big-unsigned, MK98:Len62/binary, MapBin1/binary>> = MapBin, <<Len63:16/big-unsigned, MV98:Len63/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK98, MV98, MapAcc)) end)(Len61, LeftBin108, #{}),
	<<Len64:16/big-unsigned, LeftBin110/binary>> = LeftBin109,
	{V99, LeftBin111} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len65:16/big-unsigned, MK99:Len65/binary, MapBin1/binary>> = MapBin, <<Len66:16/big-unsigned, MV99:Len66/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK99, MV99, MapAcc)) end)(Len64, LeftBin110, #{}),
	<<Len67:16/big-unsigned, LeftBin112/binary>> = LeftBin111,
	{V100, LeftBin113} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len68:16/big-unsigned, MK100:Len68/binary, MapBin1/binary>> = MapBin, <<Len69:16/big-unsigned, MV100:Len69/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK100, MV100, MapAcc)) end)(Len67, LeftBin112, #{}),
	<<Len70:16/big-unsigned, LeftBin114/binary>> = LeftBin113,
	{V101, LeftBin115} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK101:32/big-signed, MapBin1/binary>> = MapBin, <<Len71:16/big-unsigned, MV101:Len71/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK101, MV101, MapAcc)) end)(Len70, LeftBin114, #{}),
	<<Len72:16/big-unsigned, LeftBin116/binary>> = LeftBin115,
	{V102, LeftBin117} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK102:32/big-signed, MapBin1/binary>> = MapBin, <<Len73:16/big-unsigned, MV102:Len73/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK102, MV102, MapAcc)) end)(Len72, LeftBin116, #{}),
	<<Len74:16/big-unsigned, LeftBin118/binary>> = LeftBin117,
	{V103, LeftBin119} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK103:32/big-signed, MapBin1/binary>> = MapBin, <<Len75:16/big-unsigned, MV103:Len75/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK103, MV103, MapAcc)) end)(Len74, LeftBin118, #{}),
	<<Len76:16/big-unsigned, LeftBin120/binary>> = LeftBin119,
	{V104, LeftBin121} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len77:16/big-unsigned, MK104:Len77/binary, MapBin1/binary>> = MapBin, {MV104, MapBin2} = decodeRec(2, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK104, MV104, MapAcc)) end)(Len76, LeftBin120, #{}),
	<<Len78:16/big-unsigned, LeftBin122/binary>> = LeftBin121,
	{V105, LeftBin123} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len79:16/big-unsigned, MK105:Len79/binary, MapBin1/binary>> = MapBin, {MV105, MapBin2} = decodeRec(2, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK105, MV105, MapAcc)) end)(Len78, LeftBin122, #{}),
	<<Len80:16/big-unsigned, LeftBin124/binary>> = LeftBin123,
	{V106, LeftBin125} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len81:16/big-unsigned, MK106:Len81/binary, MapBin1/binary>> = MapBin, {MV106, MapBin2} = decodeRec(2, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK106, MV106, MapAcc)) end)(Len80, LeftBin124, #{}),
	<<Len82:16/big-unsigned, LeftBin126/binary>> = LeftBin125,
	{V107, LeftBin127} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK107:32/big-signed, MapBin1/binary>> = MapBin, {MV107, MapBin2} = decodeRec(2001, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK107, MV107, MapAcc)) end)(Len82, LeftBin126, #{}),
	<<Len83:16/big-unsigned, LeftBin128/binary>> = LeftBin127,
	{V108, LeftBin129} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK108:32/big-signed, MapBin1/binary>> = MapBin, {MV108, MapBin2} = decodeRec(2001, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK108, MV108, MapAcc)) end)(Len83, LeftBin128, #{}),
	<<Len84:16/big-unsigned, LeftBin130/binary>> = LeftBin129,
	{V109, LeftBin131} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK109:32/big-signed, MapBin1/binary>> = MapBin, {MV109, MapBin2} = decodeRec(2001, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK109, MV109, MapAcc)) end)(Len84, LeftBin130, #{}),
	<<OneofTag4:8/big-unsigned, LeftBin132/binary>> = LeftBin131,
	case OneofTag4 of
		0 ->
			V110 = undefined,
			LeftBin133 = LeftBin132;
		1 ->
			<<TemV110:32/big-signed, LeftBin133/binary>> = LeftBin132,
			V110 = {gold, TemV110};
		2 ->
			<<Len85:16/big-unsigned, TemV110:Len85/binary, LeftBin133/binary>> = LeftBin132,
			V110 = {coupon, TemV110};
		3 ->
			<<Len86:16/big-unsigned, TemV110:Len86/binary, LeftBin133/binary>> = LeftBin132,
			V110 = {voucher, TemV110};
		_ ->
			V110 = undefined,
			LeftBin133 = LeftBin132,
			throw({invalid_oneof_tag, OneofTag4})
	end,
	<<V111:32/big-signed, LeftBin134/binary>> = LeftBin133,
	<<OneofTag5:8/big-unsigned, LeftBin135/binary>> = LeftBin134,
	case OneofTag5 of
		0 ->
			V112 = undefined,
			LeftBin136 = LeftBin135;
		1 ->
			<<TemV112:32/big-signed, LeftBin136/binary>> = LeftBin135,
			V112 = {gold, TemV112};
		2 ->
			<<Len87:16/big-unsigned, TemV112:Len87/binary, LeftBin136/binary>> = LeftBin135,
			V112 = {coupon, TemV112};
		3 ->
			{TemV112, LeftBin136} = decodeRec(2, LeftBin135),
			V112 = {phone, TemV112};
		_ ->
			V112 = undefined,
			LeftBin136 = LeftBin135,
			throw({invalid_oneof_tag, OneofTag5})
	end,
	<<Len88:16/big-unsigned, TemStrV113:Len88/binary, LeftBin137/binary>> = LeftBin136,
	case Len88 < ?BinaryShareSize of
		true ->
			V113 = TemStrV113;
		_ ->
			case RefSize / Len88 > ?BinaryCopyRatio of
				true ->
					V113 = binary:copy(TemStrV113);
				_ ->
					V113 = TemStrV113
			end
	end,
	<<OneofTag6:8/big-unsigned, LeftBin138/binary>> = LeftBin137,
	case OneofTag6 of
		0 ->
			V114 = undefined,
			LeftBin139 = LeftBin138;
		1 ->
			<<TemV114:32/big-signed, LeftBin139/binary>> = LeftBin138,
			V114 = {gold, TemV114};
		2 ->
			{TemV114, LeftBin139} = decodeRec(2001, LeftBin138),
			V114 = {leaf, TemV114};
		3 ->
			<<Len89:16/big-unsigned, ListBin25/binary>> = LeftBin138, {TemV114, LeftBin139} = (fun DeL(0, __LB, __Acc) -> {lists:reverse(__Acc), __LB}; DeL(__N, LB0TemV114, __Acc) -> <<LETemV114:32/big-signed, LB1TemV114/binary>> = LB0TemV114, DeL(__N - 1, LB1TemV114, [LETemV114 | __Acc]) end)(Len89, ListBin25, []),
			V114 = {ids, TemV114};
		_ ->
			V114 = undefined,
			LeftBin139 = LeftBin138,
			throw({invalid_oneof_tag, OneofTag6})
	end,
	<<IsUndef4:8/big-unsigned, LeftBin140/binary>> = LeftBin139,
	case IsUndef4 of
		0 ->
			V115 = undefined,
			LeftBin141 = LeftBin140 ;
		_ ->
			{V115, LeftBin141} = decodeRec(2, LeftBin140)
	end,
	<<IsUndef5:8/big-unsigned, LeftBin142/binary>> = LeftBin141,
	case IsUndef5 of
		0 ->
			V116 = undefined,
			LeftBin143 = LeftBin142 ;
		_ ->
			{V116, LeftBin143} = decodeRec(2, LeftBin142)
	end,
	<<Len90:16/big-unsigned, TemStrV117:Len90/binary, LeftBin144/binary>> = LeftBin143,
	case Len90 < ?BinaryShareSize of
		true ->
			V117 = TemStrV117;
		_ ->
			case RefSize / Len90 > ?BinaryCopyRatio of
				true ->
					V117 = binary:copy(TemStrV117);
				_ ->
					V117 = TemStrV117
			end
	end,
	<<IsUndef6:8/big-unsigned, LeftBin145/binary>> = LeftBin144,
	case IsUndef6 of
		0 ->
			V118 = undefined,
			LeftBin146 = LeftBin145 ;
		_ ->
			{V118, LeftBin146} = decodeRec(2, LeftBin145)
	end,
	<<IsUndef7:8/big-unsigned, LeftBin147/binary>> = LeftBin146,
	case IsUndef7 of
		0 ->
			V119 = undefined,
			LeftBin148 = LeftBin147 ;
		_ ->
			{V119, LeftBin148} = decodeRec(2001, LeftBin147)
	end,
	<<IsUndef8:8/big-unsigned, LeftBin149/binary>> = LeftBin148,
	case IsUndef8 of
		0 ->
			V120 = undefined,
			LeftBin150 = LeftBin149 ;
		_ ->
			{V120, LeftBin150} = decodeRec(2001, LeftBin149)
	end,
	<<IsUndef9:8/big-unsigned, LeftBin151/binary>> = LeftBin150,
	case IsUndef9 of
		0 ->
			V121 = undefined,
			LeftBin152 = LeftBin151 ;
		_ ->
			{V121, LeftBin152} = decodeRec(2001, LeftBin151)
	end,
	<<IsUndef10:8/big-unsigned, LeftBin153/binary>> = LeftBin152,
	case IsUndef10 of
		0 ->
			V122 = undefined,
			LeftBin154 = LeftBin153 ;
		_ ->
			{V122, LeftBin154} = decodeRec(2002, LeftBin153)
	end,
	<<IsUndef11:8/big-unsigned, LeftBin155/binary>> = LeftBin154,
	case IsUndef11 of
		0 ->
			V123 = undefined,
			LeftBin156 = LeftBin155 ;
		_ ->
			{V123, LeftBin156} = decodeRec(2002, LeftBin155)
	end,
	<<IsUndef12:8/big-unsigned, LeftBin157/binary>> = LeftBin156,
	case IsUndef12 of
		0 ->
			V124 = undefined,
			LeftBin158 = LeftBin157 ;
		_ ->
			{V124, LeftBin158} = decodeRec(2002, LeftBin157)
	end,
	<<IsUndef13:8/big-unsigned, LeftBin159/binary>> = LeftBin158,
	case IsUndef13 of
		0 ->
			V125 = undefined,
			LeftBin160 = LeftBin159 ;
		_ ->
			{V125, LeftBin160} = decodeRec(2003, LeftBin159)
	end,
	<<IsUndef14:8/big-unsigned, LeftBin161/binary>> = LeftBin160,
	case IsUndef14 of
		0 ->
			V126 = undefined,
			LeftBin162 = LeftBin161 ;
		_ ->
			{V126, LeftBin162} = decodeRec(2003, LeftBin161)
	end,
	<<IsUndef15:8/big-unsigned, LeftBin163/binary>> = LeftBin162,
	case IsUndef15 of
		0 ->
			V127 = undefined,
			LeftBin164 = LeftBin163 ;
		_ ->
			{V127, LeftBin164} = decodeRec(2003, LeftBin163)
	end,
	{comboHer, {tRepeatAll, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54, V55, V56, V57, V58, V59, V60, V61, V62, V63, V64, V65, V66, V67, V68, V69, V70, V71, V72, V73, V74, V75, V76, V77, V78, V79, V80, V81, V82, V83, V84, V85, V86, V87, V88, V89, V90, V91, V92, V93, V94, V95, V96, V97, V98, V99, V100, V101, V102, V103, V104, V105, V106, V107, V108, V109, V110, V111, V112, V113, V114, V115, V116, V117, V118, V119, V120, V121, V122, V123, V124, V125, V126, V127}};
decodeBin(3001, LeftBin0) ->
	<<V1:32/big-signed, LeftBin1/binary>> = LeftBin0,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV2:Len1/binary, LeftBin2/binary>> = LeftBin1,
	case Len1 < ?BinaryShareSize of
		true ->
			V2 = TemStrV2;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V2 = binary:copy(TemStrV2);
				_ ->
					V2 = TemStrV2
			end
	end,
	<<Len2:16/big-unsigned, TemStrV3:Len2/binary, LeftBin3/binary>> = LeftBin2,
	case Len2 < ?BinaryShareSize of
		true ->
			V3 = TemStrV3;
		_ ->
			case RefSize / Len2 > ?BinaryCopyRatio of
				true ->
					V3 = binary:copy(TemStrV3);
				_ ->
					V3 = TemStrV3
			end
	end,
	{benchHer, {benchLeaf, V1, V2, V3}};
decodeBin(3002, LeftBin0) ->
	<<V1:32/big-signed, V2:32/big-signed, V3:32/big-signed, V4:32/big-signed, V5:32/big-signed, V6:32/big-signed, V7:32/big-signed, V8:32/big-signed, V9:64/big-signed, V10:64/big-signed, V11:64/big-signed, V12:64/big-signed, LeftBin1/binary>> = LeftBin0,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV13:Len1/binary, LeftBin2/binary>> = LeftBin1,
	case Len1 < ?BinaryShareSize of
		true ->
			V13 = TemStrV13;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V13 = binary:copy(TemStrV13);
				_ ->
					V13 = TemStrV13
			end
	end,
	<<Len2:16/big-unsigned, TemStrV14:Len2/binary, LeftBin3/binary>> = LeftBin2,
	case Len2 < ?BinaryShareSize of
		true ->
			V14 = TemStrV14;
		_ ->
			case RefSize / Len2 > ?BinaryCopyRatio of
				true ->
					V14 = binary:copy(TemStrV14);
				_ ->
					V14 = TemStrV14
			end
	end,
	<<Len3:16/big-unsigned, TemStrV15:Len3/binary, LeftBin4/binary>> = LeftBin3,
	case Len3 < ?BinaryShareSize of
		true ->
			V15 = TemStrV15;
		_ ->
			case RefSize / Len3 > ?BinaryCopyRatio of
				true ->
					V15 = binary:copy(TemStrV15);
				_ ->
					V15 = TemStrV15
			end
	end,
	<<Len4:16/big-unsigned, TemStrV16:Len4/binary, LeftBin5/binary>> = LeftBin4,
	case Len4 < ?BinaryShareSize of
		true ->
			V16 = TemStrV16;
		_ ->
			case RefSize / Len4 > ?BinaryCopyRatio of
				true ->
					V16 = binary:copy(TemStrV16);
				_ ->
					V16 = TemStrV16
			end
	end,
	<<Len5:16/big-unsigned, TemStrV17:Len5/binary, LeftBin6/binary>> = LeftBin5,
	case Len5 < ?BinaryShareSize of
		true ->
			V17 = TemStrV17;
		_ ->
			case RefSize / Len5 > ?BinaryCopyRatio of
				true ->
					V17 = binary:copy(TemStrV17);
				_ ->
					V17 = TemStrV17
			end
	end,
	<<Len6:16/big-unsigned, TemStrV18:Len6/binary, LeftBin7/binary>> = LeftBin6,
	case Len6 < ?BinaryShareSize of
		true ->
			V18 = TemStrV18;
		_ ->
			case RefSize / Len6 > ?BinaryCopyRatio of
				true ->
					V18 = binary:copy(TemStrV18);
				_ ->
					V18 = TemStrV18
			end
	end,
	<<Len7:16/big-unsigned, TemStrV19:Len7/binary, LeftBin8/binary>> = LeftBin7,
	case Len7 < ?BinaryShareSize of
		true ->
			V19 = TemStrV19;
		_ ->
			case RefSize / Len7 > ?BinaryCopyRatio of
				true ->
					V19 = binary:copy(TemStrV19);
				_ ->
					V19 = TemStrV19
			end
	end,
	<<Len8:16/big-unsigned, TemStrV20:Len8/binary, LeftBin9/binary>> = LeftBin8,
	case Len8 < ?BinaryShareSize of
		true ->
			V20 = TemStrV20;
		_ ->
			case RefSize / Len8 > ?BinaryCopyRatio of
				true ->
					V20 = binary:copy(TemStrV20);
				_ ->
					V20 = TemStrV20
			end
	end,
	<<Bool1:8/big-unsigned, LeftBin10/binary>> = LeftBin9,
	V21 = Bool1 =:= 1,
	<<Bool2:8/big-unsigned, LeftBin11/binary>> = LeftBin10,
	V22 = Bool2 =:= 1,
	<<V23:32/big-float, V24:64/big-float, LeftBin12/binary>> = LeftBin11,
	{benchHer, {tBenchScalar, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24}};
decodeBin(3003, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:32, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:32/big-signed>> <= ListBin1],
	<<Len2:16/big-unsigned, LeftBin3/binary>> = LeftBin2,
	RefSize = binary:referenced_byte_size(LeftBin0),
	{V2, LeftBin4} = deStringList(Len2, LeftBin3, RefSize, []),
	<<Len3:16/big-unsigned, LeftBin5/binary>> = LeftBin4,
	{V3, LeftBin6} = deStringList(Len3, LeftBin5, RefSize, []),
	<<Len4:16/big-unsigned, LeftBin7/binary>> = LeftBin6,
	{V4, LeftBin8} = deRecordList(Len4, 3001, LeftBin7, []),
	<<Len5:16/big-unsigned, LeftBin9/binary>> = LeftBin8,
	{V5, LeftBin10} = deRecordList(Len5, 2001, LeftBin9, []),
	{benchHer, {tBenchList, V1, V2, V3, V4, V5}};
decodeBin(3004, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len2:16/big-unsigned, MK1:Len2/binary, MapBin1/binary>> = MapBin, <<MV1:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK1, MV1, MapAcc)) end)(Len1, LeftBin1, #{}),
	<<Len3:16/big-unsigned, LeftBin3/binary>> = LeftBin2,
	{V2, LeftBin4} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK2:32/big-signed, MapBin1/binary>> = MapBin, <<Len4:16/big-unsigned, MV2:Len4/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK2, MV2, MapAcc)) end)(Len3, LeftBin3, #{}),
	<<Len5:16/big-unsigned, LeftBin5/binary>> = LeftBin4,
	{V3, LeftBin6} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len6:16/big-unsigned, MK3:Len6/binary, MapBin1/binary>> = MapBin, {MV3, MapBin2} = decodeRec(3001, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK3, MV3, MapAcc)) end)(Len5, LeftBin5, #{}),
	<<Len7:16/big-unsigned, LeftBin7/binary>> = LeftBin6,
	{V4, LeftBin8} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len8:16/big-unsigned, MK4:Len8/binary, MapBin1/binary>> = MapBin, {MV4, MapBin2} = decodeRec(2, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK4, MV4, MapAcc)) end)(Len7, LeftBin7, #{}),
	<<Len9:16/big-unsigned, LeftBin9/binary>> = LeftBin8,
	{V5, LeftBin10} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len10:16/big-unsigned, MK5:Len10/binary, MapBin1/binary>> = MapBin, <<Len11:16/big-unsigned, MV5:Len11/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK5, MV5, MapAcc)) end)(Len9, LeftBin9, #{}),
	{benchHer, {tBenchMap, V1, V2, V3, V4, V5}};
decodeBin(3005, LeftBin0) ->
	<<V1:32/big-signed, LeftBin1/binary>> = LeftBin0,
	<<OneofTag1:8/big-unsigned, LeftBin2/binary>> = LeftBin1,
	case OneofTag1 of
		0 ->
			V2 = undefined,
			LeftBin3 = LeftBin2;
		1 ->
			<<TemV2:32/big-signed, LeftBin3/binary>> = LeftBin2,
			V2 = {gold, TemV2};
		2 ->
			<<Len1:16/big-unsigned, TemV2:Len1/binary, LeftBin3/binary>> = LeftBin2,
			V2 = {coupon, TemV2};
		3 ->
			{TemV2, LeftBin3} = decodeRec(3001, LeftBin2),
			V2 = {leaf, TemV2};
		4 ->
			{TemV2, LeftBin3} = decodeRec(2, LeftBin2),
			V2 = {phone, TemV2};
		_ ->
			V2 = undefined,
			LeftBin3 = LeftBin2,
			throw({invalid_oneof_tag, OneofTag1})
	end,
	<<OneofTag2:8/big-unsigned, LeftBin4/binary>> = LeftBin3,
	case OneofTag2 of
		0 ->
			V3 = undefined,
			LeftBin5 = LeftBin4;
		1 ->
			<<TemV3:32/big-signed, LeftBin5/binary>> = LeftBin4,
			V3 = {code, TemV3};
		2 ->
			<<Len2:16/big-unsigned, TemV3:Len2/binary, LeftBin5/binary>> = LeftBin4,
			V3 = {note, TemV3};
		_ ->
			V3 = undefined,
			LeftBin5 = LeftBin4,
			throw({invalid_oneof_tag, OneofTag2})
	end,
	<<OneofTag3:8/big-unsigned, LeftBin6/binary>> = LeftBin5,
	case OneofTag3 of
		0 ->
			V4 = undefined,
			LeftBin7 = LeftBin6;
		1 ->
			<<Len3:16/big-unsigned, TemV4:Len3/binary, LeftBin7/binary>> = LeftBin6,
			V4 = {raw, TemV4};
		2 ->
			<<Len4:16/big-unsigned, TemV4:Len4/binary, LeftBin7/binary>> = LeftBin6,
			V4 = {text, TemV4};
		3 ->
			{TemV4, LeftBin7} = decodeRec(3001, LeftBin6),
			V4 = {blob, TemV4};
		_ ->
			V4 = undefined,
			LeftBin7 = LeftBin6,
			throw({invalid_oneof_tag, OneofTag3})
	end,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len5:16/big-unsigned, TemStrV5:Len5/binary, LeftBin8/binary>> = LeftBin7,
	case Len5 < ?BinaryShareSize of
		true ->
			V5 = TemStrV5;
		_ ->
			case RefSize / Len5 > ?BinaryCopyRatio of
				true ->
					V5 = binary:copy(TemStrV5);
				_ ->
					V5 = TemStrV5
			end
	end,
	{benchHer, {tBenchOneof, V1, V2, V3, V4, V5}};
decodeBin(3006, LeftBin0) ->
	<<IsUndef1:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	case IsUndef1 of
		0 ->
			V1 = undefined,
			LeftBin2 = LeftBin1 ;
		_ ->
			{V1, LeftBin2} = decodeRec(3001, LeftBin1)
	end,
	<<IsUndef2:8/big-unsigned, LeftBin3/binary>> = LeftBin2,
	case IsUndef2 of
		0 ->
			V2 = undefined,
			LeftBin4 = LeftBin3 ;
		_ ->
			{V2, LeftBin4} = decodeRec(2, LeftBin3)
	end,
	<<IsUndef3:8/big-unsigned, LeftBin5/binary>> = LeftBin4,
	case IsUndef3 of
		0 ->
			V3 = undefined,
			LeftBin6 = LeftBin5 ;
		_ ->
			{V3, LeftBin6} = decodeRec(2001, LeftBin5)
	end,
	<<Len1:16/big-unsigned, LeftBin7/binary>> = LeftBin6,
	{V4, LeftBin8} = deRecordList(Len1, 3001, LeftBin7, []),
	<<Len2:16/big-unsigned, LeftBin9/binary>> = LeftBin8,
	{V5, LeftBin10} = deRecordList(Len2, 2, LeftBin9, []),
	{benchHer, {tBenchNested, V1, V2, V3, V4, V5}};
decodeBin(3007, LeftBin0) ->
	<<V1:32/big-signed, LeftBin1/binary>> = LeftBin0,
	RefSize = binary:referenced_byte_size(LeftBin0),
	<<Len1:16/big-unsigned, TemStrV2:Len1/binary, LeftBin2/binary>> = LeftBin1,
	case Len1 < ?BinaryShareSize of
		true ->
			V2 = TemStrV2;
		_ ->
			case RefSize / Len1 > ?BinaryCopyRatio of
				true ->
					V2 = binary:copy(TemStrV2);
				_ ->
					V2 = TemStrV2
			end
	end,
	<<IsUndef1:8/big-unsigned, LeftBin3/binary>> = LeftBin2,
	case IsUndef1 of
		0 ->
			V3 = undefined,
			LeftBin4 = LeftBin3 ;
		_ ->
			{V3, LeftBin4} = decodeRec(3001, LeftBin3)
	end,
	<<Len2:16/big-unsigned, LeftBin5/binary>> = LeftBin4,
	{V4, LeftBin6} = deRecordList(Len2, 3001, LeftBin5, []),
	<<Len3:16/big-unsigned, LeftBin7/binary>> = LeftBin6,
	<<ListBin2:Len3/big-binary-unit:32, LeftBin8/binary>> = LeftBin7,
	V5 = [TemV || <<TemV:32/big-signed>> <= ListBin2],
	<<Len4:16/big-unsigned, LeftBin9/binary>> = LeftBin8,
	{V6, LeftBin10} = deStringList(Len4, LeftBin9, RefSize, []),
	<<Len5:16/big-unsigned, LeftBin11/binary>> = LeftBin10,
	{V7, LeftBin12} = deStringList(Len5, LeftBin11, RefSize, []),
	<<Len6:16/big-unsigned, LeftBin13/binary>> = LeftBin12,
	{V8, LeftBin14} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len7:16/big-unsigned, MK8:Len7/binary, MapBin1/binary>> = MapBin, <<MV8:32/big-signed, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK8, MV8, MapAcc)) end)(Len6, LeftBin13, #{}),
	<<Len8:16/big-unsigned, LeftBin15/binary>> = LeftBin14,
	{V9, LeftBin16} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<MK9:32/big-signed, MapBin1/binary>> = MapBin, <<Len9:16/big-unsigned, MV9:Len9/binary, MapBin2/binary>> = MapBin1, DeMap(N - 1, MapBin2, maps:put(MK9, MV9, MapAcc)) end)(Len8, LeftBin15, #{}),
	<<Len10:16/big-unsigned, LeftBin17/binary>> = LeftBin16,
	{V10, LeftBin18} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len11:16/big-unsigned, MK10:Len11/binary, MapBin1/binary>> = MapBin, {MV10, MapBin2} = decodeRec(3001, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK10, MV10, MapAcc)) end)(Len10, LeftBin17, #{}),
	<<Len12:16/big-unsigned, LeftBin19/binary>> = LeftBin18,
	{V11, LeftBin20} = (fun DeMap(0, MapBin, MapAcc) -> {MapAcc, MapBin}; DeMap(N, MapBin, MapAcc) -> <<Len13:16/big-unsigned, MK11:Len13/binary, MapBin1/binary>> = MapBin, {MV11, MapBin2} = decodeRec(2, MapBin1), DeMap(N - 1, MapBin2, maps:put(MK11, MV11, MapAcc)) end)(Len12, LeftBin19, #{}),
	<<OneofTag1:8/big-unsigned, LeftBin21/binary>> = LeftBin20,
	case OneofTag1 of
		0 ->
			V12 = undefined,
			LeftBin22 = LeftBin21;
		1 ->
			<<TemV12:32/big-signed, LeftBin22/binary>> = LeftBin21,
			V12 = {gold, TemV12};
		2 ->
			<<Len14:16/big-unsigned, TemV12:Len14/binary, LeftBin22/binary>> = LeftBin21,
			V12 = {coupon, TemV12};
		3 ->
			{TemV12, LeftBin22} = decodeRec(3001, LeftBin21),
			V12 = {leaf, TemV12};
		4 ->
			{TemV12, LeftBin22} = decodeRec(2, LeftBin21),
			V12 = {phone, TemV12};
		_ ->
			V12 = undefined,
			LeftBin22 = LeftBin21,
			throw({invalid_oneof_tag, OneofTag1})
	end,
	<<OneofTag2:8/big-unsigned, LeftBin23/binary>> = LeftBin22,
	case OneofTag2 of
		0 ->
			V13 = undefined,
			LeftBin24 = LeftBin23;
		1 ->
			<<TemV13:32/big-signed, LeftBin24/binary>> = LeftBin23,
			V13 = {code, TemV13};
		2 ->
			<<Len15:16/big-unsigned, TemV13:Len15/binary, LeftBin24/binary>> = LeftBin23,
			V13 = {note, TemV13};
		_ ->
			V13 = undefined,
			LeftBin24 = LeftBin23,
			throw({invalid_oneof_tag, OneofTag2})
	end,
	<<Len16:16/big-unsigned, TemStrV14:Len16/binary, LeftBin25/binary>> = LeftBin24,
	case Len16 < ?BinaryShareSize of
		true ->
			V14 = TemStrV14;
		_ ->
			case RefSize / Len16 > ?BinaryCopyRatio of
				true ->
					V14 = binary:copy(TemStrV14);
				_ ->
					V14 = TemStrV14
			end
	end,
	<<Len17:16/big-unsigned, LeftBin26/binary>> = LeftBin25,
	{V15, LeftBin27} = deRecordList(Len17, 2001, LeftBin26, []),
	{benchHer, {tBenchHeavy, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15}};
decodeBin(_, _) ->
	{undefinedHer, {}}.

getMsgName(1)-> test;
getMsgName(2)-> phoneNumber;
getMsgName(3)-> person;
getMsgName(4)-> addressBook;
getMsgName(5)-> union;
getMsgName(6)-> tbool;
getMsgName(7)-> tint8;
getMsgName(8)-> tuint8;
getMsgName(9)-> tint16;
getMsgName(10)-> tuint16;
getMsgName(11)-> tint32;
getMsgName(12)-> tuint32;
getMsgName(13)-> tint64;
getMsgName(14)-> tuint64;
getMsgName(15)-> tinteger;
getMsgName(16)-> tnumber;
getMsgName(17)-> tfloat;
getMsgName(18)-> tdouble;
getMsgName(19)-> tstring;
getMsgName(20)-> tlistbool;
getMsgName(21)-> tlistint8;
getMsgName(22)-> tlistuint8;
getMsgName(23)-> tlistint16;
getMsgName(24)-> tlistuint16;
getMsgName(25)-> tlistint32;
getMsgName(26)-> tlistuint32;
getMsgName(27)-> tlistint64;
getMsgName(28)-> tlistuint64;
getMsgName(29)-> tlistinteger;
getMsgName(30)-> tlistnumber;
getMsgName(31)-> tlistfloat;
getMsgName(32)-> tlistdouble;
getMsgName(33)-> tliststring;
getMsgName(34)-> tlistunion;
getMsgName(35)-> allType;
getMsgName(36)-> testnull;
getMsgName(37)-> tbinary;
getMsgName(38)-> tmap;
getMsgName(39)-> toneof;
getMsgName(1001)-> person1;
getMsgName(2001)-> comboLeaf;
getMsgName(2002)-> comboNode;
getMsgName(2003)-> comboWrap;
getMsgName(2004)-> tMapMatrix;
getMsgName(2005)-> tListMatrix;
getMsgName(2006)-> tOneofMatrix;
getMsgName(2007)-> tNestDeep;
getMsgName(2008)-> tInterleave;
getMsgName(2009)-> tMultiOneof;
getMsgName(2010)-> tEmptyEdge;
getMsgName(2011)-> tScalarRun;
getMsgName(2012)-> tRepeatAll;
getMsgName(3001)-> benchLeaf;
getMsgName(3002)-> tBenchScalar;
getMsgName(3003)-> tBenchList;
getMsgName(3004)-> tBenchMap;
getMsgName(3005)-> tBenchOneof;
getMsgName(3006)-> tBenchNested;
getMsgName(3007)-> tBenchHeavy;
getMsgName(_) -> undefined.

