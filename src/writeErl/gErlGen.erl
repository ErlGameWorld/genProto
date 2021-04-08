-module(gErlGen).
-include("genDef.hrl").

-export([
   genErl/4
]).

protoHrlHeader() ->
<<"-opaque int8() :: -128..127.
-opaque int16() :: -32768..32767.
-opaque int32() :: -2147483648..2147483647.
-opaque int64() :: -9223372036854775808..9223372036854775807.
-opaque uint8() :: 0..255.
-opaque uint16() :: 0..65536.
-opaque uint32() :: 0..4294967295.
-opaque uint64() :: 0..18446744073709551615.
-opaque double() :: float().\n\n">>.

protoErlHeader() ->
<<"-module(protoMsg).\n\n
-compile([nowarn_unused_vars]).

-export([encodeIol/1, encodeBin/1,  encodeIol/2, subEncode/1, subEncode/2, decode/1,  decodeBin/2]).

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
-define(record(V), (case V of undefined -> [<<0:8>>]; V -> [<<1:8>>, subEncode(V)] end)).
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
-define(list_record(List), [<<(length(List)):16/big>>, [subEncode(V) || V <- List]]).

-define(BinaryShareSize, 65).
-define(BinaryCopyRatio, 1.2).

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
   subEncode(erlang:element(1, RecMsg), RecMsg).\n\n">>.

genMsgHrl([], _Index, _Len, HrlBinStr) ->
   HrlBinStr;
genMsgHrl([FieldInfo | FieldList], Index, Len, HrlBinStr) ->
   TemStr =
      case Index of
         Len ->
            <<"">>;
         _ ->
            <<", ">>
      end,
   RecStr = <<TemStr/binary, (gErlField:builtRecStr(FieldInfo))/binary, (case Index == 1 of true -> <<"">>; _ -> <<"\t">> end)/binary>>,
   genMsgHrl(FieldList, Index - 1, Len, <<HrlBinStr/binary, RecStr/binary>>).

spellHead([], _Index, HeadBinStr) ->
   HeadBinStr;
spellHead([_FieldInfo | FieldList], Index, HeadBinStr) ->
   spellHead(FieldList, Index + 1, <<HeadBinStr/binary, ", V", (integer_to_binary(Index))/binary>>).

spellBody([], _Index, BodyBinStr) ->
   BodyBinStr;
spellBody([{FieldType, _FieldName}], Index, BodyBinStr) ->
   TemV = <<"V", (integer_to_binary(Index))/binary>>,
   <<BodyBinStr/binary, (gErlField:builtPackStr(FieldType))/binary, TemV/binary, ")">>;
spellBody([{FieldType, _FieldName} | FieldList], Index, BodyBinStr) ->
   TemV = <<"V", (integer_to_binary(Index))/binary>>,
   NewBodyBinStr = <<BodyBinStr/binary, (gErlField:builtPackStr(FieldType))/binary, TemV/binary, "), ">>,
   spellBody(FieldList, Index + 1, NewBodyBinStr).

spellErrCodeHrl([], ErrBinStr) ->
   ErrBinStr;
spellErrCodeHrl([{ErrName, ErrCodeId, ComDesc} | SortedErrList], ErrBinStr) ->
   ErrStr = <<"-define(", ErrName/binary, ", ", (integer_to_binary(ErrCodeId))/binary, ").\t\t%% ", ComDesc/binary, "\n">>,
   spellErrCodeHrl(SortedErrList, <<ErrBinStr/binary, ErrStr/binary>>).

genEncodeRec({MsgName, MsgId, FieldList}, IsForBin) ->
   FieldLen = length(FieldList),
   TemStr = spellHead(FieldList, 1, <<"">>),
   HeadStr =
      case IsForBin of
         true ->
            <<"encodeIol(", MsgName/binary, ", {_", TemStr/binary, "}) ->\n\t">>;
         _ ->
            <<"subEncode(", MsgName/binary, ", {_", TemStr/binary, "}) ->\n\t">>
      end,

   BodyStr = spellBody(FieldList, 1, <<"">>),
   case IsForBin of
      true ->
         case FieldLen > 0 of
            true ->
               <<HeadStr/binary, "[<<", (integer_to_binary(MsgId))/binary, ":16/big-unsigned>>, ", BodyStr/binary, "];\n">>;
            _ ->
               <<HeadStr/binary, "[<<", (integer_to_binary(MsgId))/binary, ":16/big-unsigned>>];\n">>
         end;
      _ ->
         case FieldLen > 0 of
            true ->
               <<HeadStr/binary, "[", BodyStr/binary, "];\n">>;
            _ ->
               <<HeadStr/binary, "[];\n">>
         end
   end.

resetPd() ->
   erlang:put(pd_v, 0),
   erlang:put(pd_len, 0),
   erlang:put(pd_bool, 0),
   erlang:put(pd_leftBin, 0),
   erlang:put(pd_intBits, 0),
   erlang:put(pd_numBits, 0),
   erlang:put(pd_listBin, 0),
   erlang:put(pd_isUndef, 0),
   erlang:put(pd_isCalcRefSize, 0).

getIndexStr(Type) ->
   Index = erlang:get(Type),
   integer_to_binary(Index).

useIndexStr(Type) ->
   Index = erlang:get(Type) + 1,
   erlang:put(Type, Index),
   integer_to_binary(Index).

isCalcRefSize() ->
   erlang:get(pd_isCalcRefSize) > 0.

initSubRec() ->
   erlang:put(pd_subRec, []).

getSubRec() ->
   erlang:get(pd_subRec).

initSubSubRec() ->
   erlang:put(pd_subSubRec, []).

getSubSubRec() ->
   erlang:get(pd_subSubRec).

addSubRec({MsgName, _MsgId, _FieldList} = Info, IsForBin) when IsForBin ->
   OldList = erlang:get(pd_subRec),
   case lists:keyfind(MsgName, 1, OldList) of
      false ->
         erlang:put(pd_subRec, [Info | OldList]);
      _ ->
         ignore
   end;
addSubRec({MsgName, _MsgId, _FieldList} = Info, _IsForBin) ->
   OldAllList = erlang:get(pd_subRec),
   case lists:keyfind(MsgName, 1, OldAllList) of
      false ->
         erlang:put(pd_subRec, [Info | OldAllList]),
         OldSubList = erlang:get(pd_subSubRec),
         case lists:keyfind(MsgName, 1, OldSubList) of
            false ->
               erlang:put(pd_subSubRec, [Info | OldSubList]);
            _ ->
               ignore
         end;
      _ ->
         ignore
   end.

genDecodeBin({MsgName, MsgId, FieldList}, SortedSProtoList, IsForBin) ->
   FieldLen = length(FieldList),
   case IsForBin of
      true ->
         HeadStr = <<"decodeBin(", (integer_to_binary(MsgId))/binary, ", LeftBin", (getIndexStr(pd_leftBin))/binary, ") ->\n">>;
      _ ->
         HeadStr = <<"decodeRec(", (integer_to_binary(MsgId))/binary, ", LeftBin", (getIndexStr(pd_leftBin))/binary, ") ->\n">>
   end,

   FunBody =
      fun({FieldType, _FieldName}, {IsSimple, StrAcc}) ->
         case FieldType of
            <<"bool">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        GetLeftBinStr1 = getIndexStr(pd_leftBin),
                        UseLeftBinStr1 = useIndexStr(pd_leftBin),
                        <<"LeftBin", UseLeftBinStr1/binary, "/binary>> = LeftBin", GetLeftBinStr1/binary, ",\n">>;
                     _ ->
                        <<"">>
                  end,
               UseBoolStr = useIndexStr(pd_bool),
               GetLeftBinStr2 = getIndexStr(pd_leftBin),
               UseLeftBinStr2 = useIndexStr(pd_leftBin),
               BoolStr = <<"\t<<Bool", UseBoolStr/binary, ":8/big-unsigned, LeftBin", UseLeftBinStr2/binary, "/binary>> = LeftBin", GetLeftBinStr2/binary, ",\n">>,
               UseVStr = useIndexStr(pd_v),
               VStr = <<"\tV", UseVStr/binary, " = Bool", UseBoolStr/binary, " =:= 1,\n">>,
               {false, <<StrAcc/binary, TemStr/binary, BoolStr/binary, VStr/binary>>};
            <<"int8">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        <<"V", UseVStr/binary, ":8/big-signed, ">>;
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        <<"\t<<V", UseVStr/binary, ":8/big-signed, ">>
                  end,
               {true, <<StrAcc/binary, TemStr/binary>>};
            <<"uint8">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        <<"V", UseVStr/binary, ":8/big-unsigned, ">>;
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        <<"\t<<V", UseVStr/binary, ":8/big-unsigned, ">>
                  end,
               {true, <<StrAcc/binary, TemStr/binary>>};
            <<"int16">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        <<"V", UseVStr/binary, ":16/big-signed, ">>;
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        <<"\t<<V", UseVStr/binary, ":16/big-signed, ">>
                  end,
               {true, <<StrAcc/binary, TemStr/binary>>};
            <<"uint16">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        <<"V", UseVStr/binary, ":16/big-unsigned, ">>;
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        <<"\t<<V", UseVStr/binary, ":16/big-unsigned, ">>
                  end,
               {true, <<StrAcc/binary, TemStr/binary>>};
            <<"int32">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        <<"V", UseVStr/binary, ":32/big-signed, ">>;
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        <<"\t<<V", UseVStr/binary, ":32/big-signed, ">>
                  end,
               {true, <<StrAcc/binary, TemStr/binary>>};
            <<"uint32">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        <<"V", UseVStr/binary, ":32/big-unsigned, ">>;
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        <<"\t<<V", UseVStr/binary, ":32/big-unsigned, ">>
                  end,
               {true, <<StrAcc/binary, TemStr/binary>>};
            <<"int64">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        <<"V", UseVStr/binary, ":64/big-signed, ">>;
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        <<"\t<<V", UseVStr/binary, ":64/big-signed, ">>
                  end,
               {true, <<StrAcc/binary, TemStr/binary>>};
            <<"uint64">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        <<"V", UseVStr/binary, ":64/big-unsigned, ">>;
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        <<"\t<<V", UseVStr/binary, ":64/big-unsigned, ">>
                  end,
               {true, <<StrAcc/binary, TemStr/binary>>};
            <<"float">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        <<"V", UseVStr/binary, ":32/big-float, ">>;
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        <<"\t<<V", UseVStr/binary, ":32/big-float, ">>
                  end,
               {true, <<StrAcc/binary, TemStr/binary>>};
            <<"double">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        <<"V", UseVStr/binary, ":64/big-float, ">>;
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        <<"\t<<V", UseVStr/binary, ":64/big-float, ">>
                  end,
               {true, <<StrAcc/binary, TemStr/binary>>};
            <<"string">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        GetLeftBinStr1 = getIndexStr(pd_leftBin),
                        UseLeftBinStr1 = useIndexStr(pd_leftBin),
                        <<"LeftBin", UseLeftBinStr1/binary, "/binary>> = LeftBin", GetLeftBinStr1/binary, ",\n">>;
                     _ ->
                        <<"">>
                  end,
               UseLenStr = useIndexStr(pd_len),
               GetLeftBinStr2 = getIndexStr(pd_leftBin),
               UseLeftBinStr2 = useIndexStr(pd_leftBin),
               UseVStr = useIndexStr(pd_v),
               RefSizeStr =
                  case isCalcRefSize() of
                     false ->
                        useIndexStr(pd_isCalcRefSize),
                        <<"\tRefSize = binary:referenced_byte_size(LeftBin0),\n">>;
                     _ ->
                        <<"">>
                  end,
               StrStr = <<"\t<<Len", UseLenStr/binary, ":16/big-unsigned, TemStrV", UseVStr/binary, ":Len", UseLenStr/binary, "/binary, LeftBin", UseLeftBinStr2/binary, "/binary>> = LeftBin", GetLeftBinStr2/binary, ",\n">>,
               VStr = <<"\tcase Len", UseLenStr/binary, " < ?BinaryShareSize of\n\t\t",
                  "true ->\n\t\t\tV", UseVStr/binary, " = TemStrV", UseVStr/binary, ";\n\t\t",
                  "_ ->\n\t\t\tcase RefSize / Len", UseLenStr/binary, " > ?BinaryCopyRatio of\n\t\t\t\t",
                  "true ->\n\t\t\t\t\tV", UseVStr/binary, " = binary:copy(TemStrV", UseVStr/binary, ");\n\t\t\t\t",
                  "_ ->\n\t\t\t\t\tV", UseVStr/binary, " = TemStrV", UseVStr/binary, "\n\t\t\tend\n\tend,\n">>,
               {false, <<StrAcc/binary, TemStr/binary, RefSizeStr/binary, StrStr/binary, VStr/binary>>};
            <<"integer">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        UseIntBitsStr = useIndexStr(pd_intBits),
                        <<"IntBits", UseIntBitsStr/binary, ":8, V", UseVStr/binary, ":IntBits", UseIntBitsStr/binary, "/big-signed, ">>;
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        UseIntBitsStr = useIndexStr(pd_intBits),
                        <<"\t<<IntBits", UseIntBitsStr/binary, ":8, V", UseVStr/binary, ":IntBits", UseIntBitsStr/binary, "/big-signed, ">>
                  end,
               {true, <<StrAcc/binary, TemStr/binary>>};
            <<"number">> ->
               TemStr =
                  case IsSimple of
                     true ->
                        GetLeftBinStr1 = getIndexStr(pd_leftBin),
                        UseLeftBinStr1 = useIndexStr(pd_leftBin),
                        <<"LeftBin", UseLeftBinStr1/binary, "/binary>> = LeftBin", GetLeftBinStr1/binary, ",\n">>;
                     _ ->
                        <<"">>
                  end,
               UseNumBitsStr = useIndexStr(pd_numBits),
               GetLeftBinStr2 = getIndexStr(pd_leftBin),
               UseLeftBinStr2 = useIndexStr(pd_leftBin),
               NumStr = <<"\t<<NumBits", UseNumBitsStr/binary, ":8, LeftBin", UseLeftBinStr2/binary, "/binary>> = LeftBin", GetLeftBinStr2/binary, ",\n">>,
               UseVStr = useIndexStr(pd_v),
               UseLeftBinStr3 = useIndexStr(pd_leftBin),
               VStr =
                  <<"\tcase NumBits", UseNumBitsStr/binary, " of\n\t\t33-> \n\t\t\t<<V", UseVStr/binary, ":32/big-float, LeftBin", UseLeftBinStr3/binary,
                  "/binary>> = LeftBin", UseLeftBinStr2/binary, ";\n\t\t65 ->\n\t\t\t<<V", UseVStr/binary, ":64/big-float, LeftBin", UseLeftBinStr3/binary,
                  "/binary>> = LeftBin", UseLeftBinStr2/binary, ";\n\t\t_ ->\n\t\t\t<<V", UseVStr/binary, ":NumBits", UseNumBitsStr/binary, "/big-signed, LeftBin", UseLeftBinStr3/binary,
                  "/binary>> = LeftBin", UseLeftBinStr2/binary, "\n\tend,\n">>,
               {false, <<StrAcc/binary, TemStr/binary, NumStr/binary, VStr/binary>>};
            <<"list[", LeftStr/binary>> ->
               [SubTypeStr | _] = re:split(LeftStr, <<"\\]">>, [{return, binary}]),
               TemStr =
                  case IsSimple of
                     true ->
                        GetLeftBinStr1 = getIndexStr(pd_leftBin),
                        UseLeftBinStr1 = useIndexStr(pd_leftBin),
                        <<"LeftBin", UseLeftBinStr1/binary, "/binary>> = LeftBin", GetLeftBinStr1/binary, ",\n">>;
                     _ ->
                        <<"">>
                  end,

               UseLenStr = useIndexStr(pd_len),
               GetLeftBinStr2 = getIndexStr(pd_leftBin),
               UseLeftBinStr2 = useIndexStr(pd_leftBin),
               UseVStr = useIndexStr(pd_v),
               UseListBinStr = useIndexStr(pd_listBin),
               GetLeftBinStr3 = getIndexStr(pd_leftBin),
               UseLeftBinStr3 = useIndexStr(pd_leftBin),
               LenStr = <<"\t<<Len", UseLenStr/binary, ":16/big-unsigned, LeftBin", UseLeftBinStr2/binary, "/binary>> = LeftBin", GetLeftBinStr2/binary, ",\n">>,
               DeListStr =
                  case SubTypeStr of
                     <<"bool">> ->
                        ListBinStr = <<"\t<<ListBin", UseListBinStr/binary, ":Len", UseLenStr/binary, "/big-binary-unit:8, LeftBin", UseLeftBinStr3/binary, "/binary>> = LeftBin", GetLeftBinStr3/binary, ",\n">>,
                        VStr = <<"\tV", UseVStr/binary, " = [TemV =:= 1 || <<TemV:8/big-unsigned>> <= ListBin", UseListBinStr/binary, "],\n">>,
                        <<ListBinStr/binary, VStr/binary>>;
                     <<"int8">> ->
                        ListBinStr = <<"\t<<ListBin", UseListBinStr/binary, ":Len", UseLenStr/binary, "/big-binary-unit:8, LeftBin", UseLeftBinStr3/binary, "/binary>> = LeftBin", GetLeftBinStr3/binary, ",\n">>,
                        VStr = <<"\tV", UseVStr/binary, " = [TemV || <<TemV:8/big-signed>> <= ListBin", UseListBinStr/binary, "],\n">>,
                        <<ListBinStr/binary, VStr/binary>>;
                     <<"uint8">> ->
                        ListBinStr = <<"\t<<ListBin", UseListBinStr/binary, ":Len", UseLenStr/binary, "/big-binary-unit:8, LeftBin", UseLeftBinStr3/binary, "/binary>> = LeftBin", GetLeftBinStr3/binary, ",\n">>,
                        VStr = <<"\tV", UseVStr/binary, " = [TemV || <<TemV:8/big-unsigned>> <= ListBin", UseListBinStr/binary, "],\n">>,
                        <<ListBinStr/binary, VStr/binary>>;
                     <<"int16">> ->
                        ListBinStr = <<"\t<<ListBin", UseListBinStr/binary, ":Len", UseLenStr/binary, "/big-binary-unit:16, LeftBin", UseLeftBinStr3/binary, "/binary>> = LeftBin", GetLeftBinStr3/binary, ",\n">>,
                        VStr = <<"\tV", UseVStr/binary, " = [TemV || <<TemV:16/big-signed>> <= ListBin", UseListBinStr/binary, "],\n">>,
                        <<ListBinStr/binary, VStr/binary>>;
                     <<"uint16">> ->
                        ListBinStr = <<"\t<<ListBin", UseListBinStr/binary, ":Len", UseLenStr/binary, "/big-binary-unit:16, LeftBin", UseLeftBinStr3/binary, "/binary>> = LeftBin", GetLeftBinStr3/binary, ",\n">>,
                        VStr = <<"\tV", UseVStr/binary, " = [TemV || <<TemV:16/big-unsigned>> <= ListBin", UseListBinStr/binary, "],\n">>,
                        <<ListBinStr/binary, VStr/binary>>;
                     <<"int32">> ->
                        ListBinStr = <<"\t<<ListBin", UseListBinStr/binary, ":Len", UseLenStr/binary, "/big-binary-unit:32, LeftBin", UseLeftBinStr3/binary, "/binary>> = LeftBin", GetLeftBinStr3/binary, ",\n">>,
                        VStr = <<"\tV", UseVStr/binary, " = [TemV || <<TemV:32/big-signed>> <= ListBin", UseListBinStr/binary, "],\n">>,
                        <<ListBinStr/binary, VStr/binary>>;
                     <<"uint32">> ->
                        ListBinStr = <<"\t<<ListBin", UseListBinStr/binary, ":Len", UseLenStr/binary, "/big-binary-unit:32, LeftBin", UseLeftBinStr3/binary, "/binary>> = LeftBin", GetLeftBinStr3/binary, ",\n">>,
                        VStr = <<"\tV", UseVStr/binary, " = [TemV || <<TemV:32/big-unsigned>> <= ListBin", UseListBinStr/binary, "],\n">>,
                        <<ListBinStr/binary, VStr/binary>>;
                     <<"int64">> ->
                        ListBinStr = <<"\t<<ListBin", UseListBinStr/binary, ":Len", UseLenStr/binary, "/big-binary-unit:64, LeftBin", UseLeftBinStr3/binary, "/binary>> = LeftBin", GetLeftBinStr3/binary, ",\n">>,
                        VStr = <<"\tV", UseVStr/binary, " = [TemV || <<TemV:64/big-signed>> <= ListBin", UseListBinStr/binary, "],\n">>,
                        <<ListBinStr/binary, VStr/binary>>;
                     <<"uint64">> ->
                        ListBinStr = <<"\t<<ListBin", UseListBinStr/binary, ":Len", UseLenStr/binary, "/big-binary-unit:64, LeftBin", UseLeftBinStr3/binary, "/binary>> = LeftBin", GetLeftBinStr3/binary, ",\n">>,
                        VStr = <<"\tV", UseVStr/binary, " = [TemV || <<TemV:64/big-unsigned>> <= ListBin", UseListBinStr/binary, "],\n">>,
                        <<ListBinStr/binary, VStr/binary>>;
                     <<"integer">> ->
                        <<"\t{V", UseVStr/binary, ", LeftBin", UseLeftBinStr3/binary, "} = deIntegerList(Len", UseLenStr/binary, ", LeftBin", GetLeftBinStr3/binary, ", []),\n">>;
                     <<"number">> ->
                        <<"\t{V", UseVStr/binary, ", LeftBin", UseLeftBinStr3/binary, "} = deNumberList(Len", UseLenStr/binary, ", LeftBin", GetLeftBinStr3/binary, ", []),\n">>;
                     <<"float">> ->
                        ListBinStr = <<"\t<<ListBin", UseListBinStr/binary, ":Len", UseLenStr/binary, "/big-binary-unit:32, LeftBin", UseLeftBinStr3/binary, "/binary>> = LeftBin", GetLeftBinStr3/binary, ",\n">>,
                        VStr = <<"\tV", UseVStr/binary, " = [TemV || <<TemV:32/big-float>> <= ListBin", UseListBinStr/binary, "],\n">>,
                        <<ListBinStr/binary, VStr/binary>>;
                     <<"double">> ->
                        ListBinStr = <<"\t<<ListBin", UseListBinStr/binary, ":Len", UseLenStr/binary, "/big-binary-unit:64, LeftBin", UseLeftBinStr3/binary, "/binary>> = LeftBin", GetLeftBinStr3/binary, ",\n">>,
                        VStr = <<"\tV", UseVStr/binary, " = [TemV || <<TemV:64/big-float>> <= ListBin", UseListBinStr/binary, "],\n">>,
                        <<ListBinStr/binary, VStr/binary>>;
                     <<"string">> ->
                        case isCalcRefSize() of
                           true ->
                              <<"\t{V", UseVStr/binary, ", LeftBin", UseLeftBinStr3/binary, "} = deStringList(Len", UseLenStr/binary, ", LeftBin", GetLeftBinStr3/binary, ", RefSize, []),\n">>;
                           _ ->
                              useIndexStr(pd_isCalcRefSize),
                              RefSizeStr = <<"\tRefSize = binary:referenced_byte_size(LeftBin0),\n">>,
                              VStr = <<"\t{V", UseVStr/binary, ", LeftBin", UseLeftBinStr3/binary, "} = deStringList(Len", UseLenStr/binary, ", LeftBin", GetLeftBinStr3/binary, ", RefSize, []),\n">>,
                              <<RefSizeStr/binary, VStr/binary>>
                        end;
                     ListRecord ->
                        case lists:keyfind(ListRecord, 1, SortedSProtoList) of
                           {ListRecord, ListMsgId, _} = RecordInfo ->
                              addSubRec(RecordInfo, IsForBin),
                              <<"\t{V", UseVStr/binary, ", LeftBin", UseLeftBinStr3/binary, "} = deRecordList(Len", UseLenStr/binary, ", ", (integer_to_binary(ListMsgId))/binary, ", LeftBin", GetLeftBinStr3/binary, ", []),\n">>;
                           _ ->
                              io:format("this an Record undefined :~p~n", [ListRecord]),
                              throw(record_undefined)
                        end
                  end,
               {false, <<StrAcc/binary, TemStr/binary, LenStr/binary, DeListStr/binary>>};
            OtherRecord ->
               TemStr =
                  case IsSimple of
                     true ->
                        GetLeftBinStr1 = getIndexStr(pd_leftBin),
                        UseLeftBinStr1 = useIndexStr(pd_leftBin),
                        <<"LeftBin", UseLeftBinStr1/binary, "/binary>> = LeftBin", GetLeftBinStr1/binary, ",\n">>;
                     _ ->
                        <<"">>
                  end,
               UseIsUndefStr = useIndexStr(pd_isUndef),
               GetLeftBinStr2 = getIndexStr(pd_leftBin),
               UseLeftBinStr2 = useIndexStr(pd_leftBin),
               IsStr = <<"\t<<IsUndef", UseIsUndefStr/binary, ":8/big-unsigned, LeftBin", UseLeftBinStr2/binary, "/binary>> = LeftBin", GetLeftBinStr2/binary, ",\n">>,
               UseVStr = useIndexStr(pd_v),
               UseLeftBinStr3 = useIndexStr(pd_leftBin),
               case lists:keyfind(OtherRecord, 1, SortedSProtoList) of
                  {OtherRecord, OtherMsgId, _} = RecordInfo ->
                     addSubRec(RecordInfo, IsForBin),
                     VStr = <<"\tcase IsUndef", UseIsUndefStr/binary, " of\n\t\t0 ->\n\t\t\tV", UseVStr/binary, " = undefined,\n\t\t\tLeftBin", UseLeftBinStr3/binary, " = LeftBin", UseLeftBinStr2/binary,
                     " ;\n\t\t_ ->\n\t\t\t{V", UseVStr/binary, ", LeftBin", UseLeftBinStr3/binary, "} = ", "decodeRec(", (integer_to_binary(OtherMsgId))/binary, ", LeftBin", UseLeftBinStr2/binary, ")\n\tend,\n">>,
                     {false, <<StrAcc/binary, TemStr/binary, IsStr/binary, VStr/binary>>};
                  _ ->
                     io:format("this an Record undefined :~p~n", [OtherRecord]),
                     throw(record_undefined)
               end
         end
      end,

   {LIsSimple, BodyStr} = lists:foldl(FunBody, {false, <<"">>}, FieldList),
   LBodyStr =
      case LIsSimple of
         true ->
            LGetLeftBinStr = getIndexStr(pd_leftBin),
            LUseLeftBinStr = useIndexStr(pd_leftBin),
            <<BodyStr/binary, "LeftBin", LUseLeftBinStr/binary, "/binary>> = LeftBin", LGetLeftBinStr/binary, ",\n">>;
         _ ->
            BodyStr
      end,

   RetStr =
      case IsForBin of
         true ->
            MsgIndex = MsgId div ?MsgIdSegSize,
            Handler = erlang:get(pd_handler),
            {MsgIndex, ModName} = lists:keyfind(MsgIndex, 1, Handler),
            HandleName = <<ModName/binary, "Her">>,
            case FieldLen > 0 of
               true ->
                  FunRec =
                     fun(N, Acc) ->
                        <<Acc/binary, ", V", (integer_to_binary(N))/binary>>
                     end,
                  RecStr = lists:foldl(FunRec, <<"">>, lists:seq(1, FieldLen)),
                  <<"\t{", HandleName/binary, ", ", MsgName/binary, ", {", MsgName/binary, RecStr/binary, "}};\n">>;
               _ ->
                  <<"\t{", HandleName/binary, ", ", MsgName/binary, ", {", MsgName/binary, "}};\n">>
            end;
         _ ->
            case FieldLen > 0 of
               true ->
                  FunRec =
                     fun(N, Acc) ->
                        <<Acc/binary, ", V", (integer_to_binary(N))/binary>>
                     end,
                  RecStr = lists:foldl(FunRec, <<"">>, lists:seq(1, FieldLen)),
                  <<"\tMsgRec = {", MsgName/binary, RecStr/binary, "},\n\t{MsgRec, LeftBin", (getIndexStr(pd_leftBin))/binary, "};\n">>;
               _ ->
                  <<"\t{{", MsgName/binary, "}, ", "<<>>};\n">>
            end
      end,
   <<HeadStr/binary, LBodyStr/binary, RetStr/binary>>.

genErl(SortedSProtoList, SortedErrList, HrlDir, ErlDir) ->
   initSubRec(),
   FunSpell =
      fun({MsgName, MsgId, FieldList} = MsgInfo, {MsgHrlAcc, MsgEncodeAcc, MsgDecodeAcc, MsgIdAcc, MsgNameAcc}) ->
         %% gen hrl str
         Len = erlang:length(FieldList),
         LastFieldStr = genMsgHrl(FieldList, Len, Len, <<"">>),
         HrlStr = <<"-record(", MsgName/binary, ", {\n\t", LastFieldStr/binary, "}).\n">>,

         %% gen getMsgId  getMsgType str
         IdStr = <<"getMsgId(", MsgName/binary, ")-> ", (integer_to_binary(MsgId))/binary, ";\n">>,
         NameStr = <<"getMsgName(", (integer_to_binary(MsgId))/binary, ")-> ", MsgName/binary, ";\n">>,

         %% gen encodeRec Str
         EncodeStr = genEncodeRec(MsgInfo, true),

         %% gen decodeBin str
         resetPd(),
         DecodeStr = genDecodeBin(MsgInfo, SortedSProtoList, true),

         {<<MsgHrlAcc/binary, HrlStr/binary>>,  <<MsgEncodeAcc/binary, EncodeStr/binary>>, <<MsgDecodeAcc/binary, DecodeStr/binary>>, <<MsgIdAcc/binary, IdStr/binary>>, <<MsgNameAcc/binary, NameStr/binary>>}
      end,
   {MsgHrlStr, TMsgEncodeStr, TMsgDecodeStr, _TMsgIdStr, _TMsgNameStr} = lists:foldl(FunSpell, {<<>>, <<>>, <<>>, <<>>, <<>>}, SortedSProtoList),
   MsgEncodeStr = <<TMsgEncodeStr/binary, "encodeIol(_, _) ->\n\t[].\n\n">>,
   MsgDecodeStr = <<TMsgDecodeStr/binary, "decodeBin(_, _) ->\n\t{undefinedHer, undefined, {}}.\n\n">>,
   _MsgIdStr = <<_TMsgIdStr/binary, "getMsgId(_) -> 0.\n\n">>,
   _MsgNameStr = <<_TMsgNameStr/binary, "getMsgName(_) -> undefiend.\n\n">>,

   ErrCodeStr = spellErrCodeHrl(SortedErrList, <<>>),

   %% gen decodeRec str
   SubRecList = getSubRec(),
   initSubSubRec(),
   SortedSubRecList = lists:sort(fun({_Name1, MessageId1, _FieldList1}, {_Name2, MessageId2, _FieldList2}) -> MessageId1 < MessageId2 end, SubRecList),
   {MsgEncodeRecStr, MsgDecodeRecStr} = dealSubRec(SortedSubRecList, SortedSProtoList, <<"">>, <<"">>),

   ErlHeaderStr = protoErlHeader(),
   HrlHeaderStr = protoHrlHeader(),
   OutputErlStr = <<ErlHeaderStr/binary, MsgEncodeRecStr/binary, MsgEncodeStr/binary, MsgDecodeRecStr/binary, MsgDecodeStr/binary>>,
   OutputHrlStr = <<HrlHeaderStr/binary, ErrCodeStr/binary, MsgHrlStr/binary>>,
   HrlFilename = do_write_hrl(HrlDir, protoMsg, OutputHrlStr),
   ErlFilename = do_write_erl(ErlDir, protoMsg, OutputErlStr),

   io:format("protoConvert hrl dir : ~s ~n", [HrlDir]),
   io:format("protoConvert erl dir : ~s ~n", [ErlDir]),
   io:format("protoConvert to hrl file ~s succ.~n", [HrlFilename]),
   io:format("protoConvert to erl file ~s succ.~n", [ErlFilename]),
   ok.

dealSubRec([], SortedSProtoList, SubEncodeAcc, SubDecodeAcc) ->
   case getSubSubRec() of
      [] ->
         {<<SubEncodeAcc/binary, "subEncode(_, _) ->\n\t[].\n\n">>, <<SubDecodeAcc/binary, "decodeRec(_, _) ->\n\t{{}, <<>>}.\n\n">>};
      NewAddList ->
         initSubSubRec(),
         SortedSubRecList = lists:sort(fun({_Name1, MessageId1, _FieldList1}, {_Name2, MessageId2, _FieldList2}) -> MessageId1 < MessageId2 end, NewAddList),
         dealSubRec(SortedSubRecList, SortedSProtoList, SubEncodeAcc, SubDecodeAcc)
   end;
dealSubRec([MsgInfo | SubRecList], SortedSProtoList, SubEncodeAcc, SubDecodeAcc) ->
   EncodeStr = genEncodeRec(MsgInfo, false),
   resetPd(),
   DecodeStr = genDecodeBin(MsgInfo, SortedSProtoList, false),
   dealSubRec(SubRecList, SortedSProtoList, <<SubEncodeAcc/binary, EncodeStr/binary>>, <<SubDecodeAcc/binary, DecodeStr/binary>>).

do_write_hrl(OutDir, Mod, BinStr) ->
   Filename = filename:join([OutDir, atom_to_list(Mod) ++ ".hrl"]),
   ok = file:write_file(Filename, BinStr),
   Filename.

do_write_erl(OutDir, Mod, BinStr ) ->
   Filename = filename:join([OutDir, atom_to_list(Mod) ++ ".erl"]),
   case file:write_file(Filename, BinStr) of
      ok ->
         ok;
      _Ret ->
         io:format("write to erl file error:~p ~n", [_Ret])
   end,
   Filename.

