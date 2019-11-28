-module(protoMsg).


-compile([nowarn_unused_vars]).

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

encodeRec({test, V1}) ->
	[?string(V1)];
encodeRec({phoneNumber, V1, V2}) ->
	[?record(V1), ?int32(V2)];
encodeRec({person, V1, V2, V3, V4}) ->
	[?string(V1), ?int32(V2), ?string(V3), ?list_record(V4)];
encodeRec({union, V1, V2}) ->
	[?string(V1), ?int32(V2)];
encodeRec(_) ->
	[].

encode({test, V1}) ->
	[<<1:16/big-unsigned>>, ?string(V1)];
encode({phoneNumber, V1, V2}) ->
	[<<2:16/big-unsigned>>, ?record(V1), ?int32(V2)];
encode({person, V1, V2, V3, V4}) ->
	[<<3:16/big-unsigned>>, ?string(V1), ?int32(V2), ?string(V3), ?list_record(V4)];
encode({addressBook, V1, V2}) ->
	[<<4:16/big-unsigned>>, ?list_record(V1), ?list_record(V2)];
encode({union, V1, V2}) ->
	[<<5:16/big-unsigned>>, ?string(V1), ?int32(V2)];
encode({tbool, V1}) ->
	[<<6:16/big-unsigned>>, ?bool(V1)];
encode({tint8, V1, V2}) ->
	[<<7:16/big-unsigned>>, ?int8(V1), ?int8(V2)];
encode({tuint8, V1, V2}) ->
	[<<8:16/big-unsigned>>, ?uint8(V1), ?uint8(V2)];
encode({tint16, V1, V2}) ->
	[<<9:16/big-unsigned>>, ?int16(V1), ?int16(V2)];
encode({tuint16, V1, V2}) ->
	[<<10:16/big-unsigned>>, ?uint16(V1), ?uint16(V2)];
encode({tint32, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10}) ->
	[<<11:16/big-unsigned>>, ?int32(V1), ?int32(V2), ?int32(V3), ?int32(V4), ?int32(V5), ?int32(V6), ?int32(V7), ?int32(V8), ?int32(V9), ?int32(V10)];
encode({tuint32, V1, V2}) ->
	[<<12:16/big-unsigned>>, ?uint32(V1), ?uint32(V2)];
encode({tint64, V1, V2}) ->
	[<<13:16/big-unsigned>>, ?int64(V1), ?int64(V2)];
encode({tuint64, V1, V2}) ->
	[<<14:16/big-unsigned>>, ?uint64(V1), ?uint64(V2)];
encode({tinteger, V1, V2, V3, V4, V5, V6, V7, V8}) ->
	[<<15:16/big-unsigned>>, ?integer(V1), ?integer(V2), ?integer(V3), ?integer(V4), ?integer(V5), ?integer(V6), ?integer(V7), ?integer(V8)];
encode({tnumber, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10}) ->
	[<<16:16/big-unsigned>>, ?number(V1), ?number(V2), ?number(V3), ?number(V4), ?number(V5), ?number(V6), ?number(V7), ?number(V8), ?number(V9), ?number(V10)];
encode({tfloat, V1, V2}) ->
	[<<17:16/big-unsigned>>, ?float(V1), ?float(V2)];
encode({tdouble, V1, V2}) ->
	[<<18:16/big-unsigned>>, ?double(V1), ?double(V2)];
encode({tstring, V1, V2}) ->
	[<<19:16/big-unsigned>>, ?string(V1), ?string(V2)];
encode({tlistbool, V1}) ->
	[<<20:16/big-unsigned>>, ?list_bool(V1)];
encode({tlistint8, V1}) ->
	[<<21:16/big-unsigned>>, ?list_int8(V1)];
encode({tlistuint8, V1}) ->
	[<<22:16/big-unsigned>>, ?list_uint8(V1)];
encode({tlistint16, V1}) ->
	[<<23:16/big-unsigned>>, ?list_int16(V1)];
encode({tlistuint16, V1}) ->
	[<<24:16/big-unsigned>>, ?list_uint16(V1)];
encode({tlistint32, V1}) ->
	[<<25:16/big-unsigned>>, ?list_int32(V1)];
encode({tlistuint32, V1}) ->
	[<<26:16/big-unsigned>>, ?list_uint32(V1)];
encode({tlistint64, V1}) ->
	[<<27:16/big-unsigned>>, ?list_int64(V1)];
encode({tlistuint64, V1}) ->
	[<<28:16/big-unsigned>>, ?list_uint64(V1)];
encode({tlistinteger, V1}) ->
	[<<29:16/big-unsigned>>, ?list_integer(V1)];
encode({tlistnumber, V1}) ->
	[<<30:16/big-unsigned>>, ?list_number(V1)];
encode({tlistfloat, V1}) ->
	[<<31:16/big-unsigned>>, ?list_float(V1)];
encode({tlistdouble, V1}) ->
	[<<32:16/big-unsigned>>, ?list_double(V1)];
encode({tliststring, V1}) ->
	[<<33:16/big-unsigned>>, ?list_string(V1)];
encode({tlistunion, V1}) ->
	[<<34:16/big-unsigned>>, ?list_record(V1)];
encode({allType, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54, V55}) ->
	[<<35:16/big-unsigned>>, ?bool(V1), ?int8(V2), ?uint8(V3), ?int16(V4), ?uint16(V5), ?int32(V6), ?uint32(V7), ?int64(V8), ?uint64(V9), ?integer(V10), ?integer(V11), ?integer(V12), ?integer(V13), ?integer(V14), ?integer(V15), ?integer(V16), ?integer(V17), ?number(V18), ?number(V19), ?number(V20), ?number(V21), ?number(V22), ?number(V23), ?number(V24), ?number(V25), ?number(V26), ?number(V27), ?float(V28), ?double(V29), ?string(V30), ?string(V31), ?record(V32), ?list_bool(V33), ?list_int8(V34), ?list_uint8(V35), ?list_int16(V36), ?list_uint16(V37), ?list_int32(V38), ?list_uint32(V39), ?list_int64(V40), ?list_uint64(V41), ?list_integer(V42), ?list_integer(V43), ?list_integer(V44), ?list_integer(V45), ?list_number(V46), ?list_number(V47), ?list_number(V48), ?list_number(V49), ?list_number(V50), ?list_number(V51), ?list_float(V52), ?list_double(V53), ?list_string(V54), ?list_record(V55)];
encode({person1, V1, V2, V3, V4}) ->
	[<<1001:16/big-unsigned>>, ?string(V1), ?int32(V2), ?string(V3), ?list_record(V4)];
encode(_) ->
	[].

decodeRec(1, LeftBin0) ->
	<<Len1:16/big-unsigned, V1:Len1/binary, LeftBin1/binary>> = LeftBin0,
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
	<<Len1:16/big-unsigned, V1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	<<V2:32/big-signed, LeftBin2/binary>> = LeftBin1,
	<<Len2:16/big-unsigned, V3:Len2/binary, LeftBin3/binary>> = LeftBin2,
	<<Len3:16/big-unsigned, LeftBin4/binary>> = LeftBin3,
	{V4, LeftBin5} = deRecordList(Len3, 2, LeftBin4, []),
	MsgRec = {person, V1, V2, V3, V4},
	{MsgRec, LeftBin5};
decodeRec(5, LeftBin0) ->
	<<Len1:16/big-unsigned, V1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	<<V2:32/big-signed, LeftBin2/binary>> = LeftBin1,
	MsgRec = {union, V1, V2},
	{MsgRec, LeftBin2};
decodeRec(_, _) ->
	{{}, <<>>}.

decodeBin(1, LeftBin0) ->
	<<Len1:16/big-unsigned, V1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	{test, V1};
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
	{phoneNumber, V1, V2};
decodeBin(3, LeftBin0) ->
	<<Len1:16/big-unsigned, V1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	<<V2:32/big-signed, LeftBin2/binary>> = LeftBin1,
	<<Len2:16/big-unsigned, V3:Len2/binary, LeftBin3/binary>> = LeftBin2,
	<<Len3:16/big-unsigned, LeftBin4/binary>> = LeftBin3,
	{V4, LeftBin5} = deRecordList(Len3, 2, LeftBin4, []),
	{person, V1, V2, V3, V4};
decodeBin(4, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = deRecordList(Len1, 3, LeftBin1, []),
	<<Len2:16/big-unsigned, LeftBin3/binary>> = LeftBin2,
	{V2, LeftBin4} = deRecordList(Len2, 3, LeftBin3, []),
	{addressBook, V1, V2};
decodeBin(5, LeftBin0) ->
	<<Len1:16/big-unsigned, V1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	<<V2:32/big-signed, LeftBin2/binary>> = LeftBin1,
	{union, V1, V2};
decodeBin(6, LeftBin0) ->
	<<Bool1:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	V1 = Bool1 =:= 1,
	{tbool, V1};
decodeBin(7, LeftBin0) ->
	<<V1:8/big-signed, V2:8/big-signed, LeftBin1/binary>> = LeftBin0,
	{tint8, V1, V2};
decodeBin(8, LeftBin0) ->
	<<V1:8/big-unsigned, V2:8/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{tuint8, V1, V2};
decodeBin(9, LeftBin0) ->
	<<V1:16/big-signed, V2:16/big-signed, LeftBin1/binary>> = LeftBin0,
	{tint16, V1, V2};
decodeBin(10, LeftBin0) ->
	<<V1:16/big-unsigned, V2:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{tuint16, V1, V2};
decodeBin(11, LeftBin0) ->
	<<V1:32/big-signed, V2:32/big-signed, V3:32/big-signed, V4:32/big-signed, V5:32/big-signed, V6:32/big-signed, V7:32/big-signed, V8:32/big-signed, V9:32/big-signed, V10:32/big-signed, LeftBin1/binary>> = LeftBin0,
	{tint32, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10};
decodeBin(12, LeftBin0) ->
	<<V1:32/big-unsigned, V2:32/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{tuint32, V1, V2};
decodeBin(13, LeftBin0) ->
	<<V1:64/big-signed, V2:64/big-signed, LeftBin1/binary>> = LeftBin0,
	{tint64, V1, V2};
decodeBin(14, LeftBin0) ->
	<<V1:64/big-unsigned, V2:64/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{tuint64, V1, V2};
decodeBin(15, LeftBin0) ->
	<<IntBits1:8, V1:IntBits1/big-signed, IntBits2:8, V2:IntBits2/big-signed, IntBits3:8, V3:IntBits3/big-signed, IntBits4:8, V4:IntBits4/big-signed, IntBits5:8, V5:IntBits5/big-signed, IntBits6:8, V6:IntBits6/big-signed, IntBits7:8, V7:IntBits7/big-signed, IntBits8:8, V8:IntBits8/big-signed, LeftBin1/binary>> = LeftBin0,
	{tinteger, V1, V2, V3, V4, V5, V6, V7, V8};
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
	{tnumber, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10};
decodeBin(17, LeftBin0) ->
	<<V1:32/big-float, V2:32/big-float, LeftBin1/binary>> = LeftBin0,
	{tfloat, V1, V2};
decodeBin(18, LeftBin0) ->
	<<V1:64/big-float, V2:64/big-float, LeftBin1/binary>> = LeftBin0,
	{tdouble, V1, V2};
decodeBin(19, LeftBin0) ->
	<<Len1:16/big-unsigned, V1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	<<Len2:16/big-unsigned, V2:Len2/binary, LeftBin2/binary>> = LeftBin1,
	{tstring, V1, V2};
decodeBin(20, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:8, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV =:= 1 || <<TemV:8/big-unsigned>> <= ListBin1],
	{tlistbool, V1};
decodeBin(21, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:8, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:8/big-signed>> <= ListBin1],
	{tlistint8, V1};
decodeBin(22, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:8, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:8/big-unsigned>> <= ListBin1],
	{tlistuint8, V1};
decodeBin(23, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:16, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:16/big-signed>> <= ListBin1],
	{tlistint16, V1};
decodeBin(24, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:16, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:16/big-unsigned>> <= ListBin1],
	{tlistuint16, V1};
decodeBin(25, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:32, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:32/big-signed>> <= ListBin1],
	{tlistint32, V1};
decodeBin(26, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:32, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:32/big-unsigned>> <= ListBin1],
	{tlistuint32, V1};
decodeBin(27, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:64, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:64/big-signed>> <= ListBin1],
	{tlistint64, V1};
decodeBin(28, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:64, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:64/big-unsigned>> <= ListBin1],
	{tlistuint64, V1};
decodeBin(29, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = deIntegerList(Len1, LeftBin1, []),
	{tlistinteger, V1};
decodeBin(30, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = deNumberList(Len1, LeftBin1, []),
	{tlistnumber, V1};
decodeBin(31, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:32, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:32/big-float>> <= ListBin1],
	{tlistfloat, V1};
decodeBin(32, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	<<ListBin1:Len1/big-binary-unit:64, LeftBin2/binary>> = LeftBin1,
	V1 = [TemV || <<TemV:64/big-float>> <= ListBin1],
	{tlistdouble, V1};
decodeBin(33, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = deStringList(Len1, LeftBin1, []),
	{tliststring, V1};
decodeBin(34, LeftBin0) ->
	<<Len1:16/big-unsigned, LeftBin1/binary>> = LeftBin0,
	{V1, LeftBin2} = deRecordList(Len1, 5, LeftBin1, []),
	{tlistunion, V1};
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
	<<Len1:16/big-unsigned, V30:Len1/binary, LeftBin24/binary>> = LeftBin23,
	<<Len2:16/big-unsigned, V31:Len2/binary, LeftBin25/binary>> = LeftBin24,
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
	{V54, LeftBin71} = deStringList(Len24, LeftBin70, []),
	<<Len25:16/big-unsigned, LeftBin72/binary>> = LeftBin71,
	{V55, LeftBin73} = deRecordList(Len25, 5, LeftBin72, []),
	{allType, V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54, V55};
decodeBin(1001, LeftBin0) ->
	<<Len1:16/big-unsigned, V1:Len1/binary, LeftBin1/binary>> = LeftBin0,
	<<V2:32/big-signed, LeftBin2/binary>> = LeftBin1,
	<<Len2:16/big-unsigned, V3:Len2/binary, LeftBin3/binary>> = LeftBin2,
	<<Len3:16/big-unsigned, LeftBin4/binary>> = LeftBin3,
	{V4, LeftBin5} = deRecordList(Len3, 2, LeftBin4, []),
	{person1, V1, V2, V3, V4};
decodeBin(_, _) ->
	{{}, <<>>}.

