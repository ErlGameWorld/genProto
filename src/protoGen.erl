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
-opaque double() :: float().\n\n".

protoErlHeader() ->
"-module(protoMsg).\n\n
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
   deRecordList(N - 1, MsgId, LeftBin, [Tuple | RetList]).\n\n".

genMsgHrl(FieldInfo, {Index, Len, AccList}) ->
   TemStr =
      case Index of
         1 ->
            "";
         _ ->
            ", "
      end,
   RecStr = TemStr ++ protoField:builtRecStr(FieldInfo) ++ (case Index == Len of true -> ""; _ -> "\t" end),
   {Index - 1, Len, [RecStr | AccList]}.

genErrCodeHrl({ErrName, ErrCodeId, ComDesc}, AccList) ->
   Str = "-define(" ++ ErrName ++ ", " ++ integer_to_list(ErrCodeId) ++ ").\t\t%% " ++ ComDesc ++ "\n",
   [Str | AccList].

genEncodeRec({MsgName, MsgId, FieldList}, IsForBin) ->
   FieldLen = length(FieldList),
   FunHead =
      fun(_, {Index, StrAcc}) ->
         {Index - 1, ", V" ++ integer_to_list(Index) ++ StrAcc}
      end,
   {_, TemStr} = lists:foldr(FunHead, {FieldLen, "}) ->\n\t"}, FieldList),
   HeadStr =
      case IsForBin of
         true ->
            "encode({" ++ MsgName ++ TemStr;
         _ ->
            "encodeRec({" ++ MsgName ++ TemStr
      end,

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
   {_, BodyStr} = lists:foldr(FunBody, {FieldLen, ""}, FieldList),
   case IsForBin of
      true ->
         case FieldLen > 0 of
            true ->
               HeadStr ++ "[<<" ++ integer_to_list(MsgId) ++ ":16/big-unsigned>>, " ++ BodyStr ++ "];\n";
            _ ->
               HeadStr ++ "[<<" ++ integer_to_list(MsgId) ++ ":16/big-unsigned>>];\n"
         end;
      _ ->
         case FieldLen > 0 of
            true ->
               HeadStr ++ "[" ++ BodyStr ++ "];\n";
            _ ->
               HeadStr ++ "[];\n"
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
   erlang:integer_to_list(Index).

useIndexStr(Type) ->
   Index = erlang:get(Type) + 1,
   erlang:put(Type, Index),
   erlang:integer_to_list(Index).

isCalcRefSize() ->
   erlang:get(pd_isCalcRefSize) > 0.

initSubRec() ->
   erlang:put(pd_subRec, []).

getSubRec() ->
   erlang:get(pd_subRec).

addSubRec({MsgName, _MsgId, _FieldList} = Info, IsForBin) when IsForBin ->
   OldList = erlang:get(pd_subRec),
   case lists:keyfind(MsgName, 1, OldList) of
      false ->
         erlang:put(pd_subRec, [Info | OldList]);
      _ ->
         ignore
   end;
addSubRec(_Info, _IsForBin) ->
   ignore.

genDecodeBin({MsgName, MsgId, FieldList}, SortedSProtoList, IsForBin) ->
   FieldLen = length(FieldList),
   case IsForBin of
      true ->
         HeadStr = "decodeBin(" ++ integer_to_list(MsgId) ++ ", LeftBin" ++ getIndexStr(pd_leftBin) ++ ") ->\n";
      _ ->
         HeadStr = "decodeRec(" ++ integer_to_list(MsgId) ++ ", LeftBin" ++ getIndexStr(pd_leftBin) ++ ") ->\n"
   end,

   FunBody =
      fun({FieldType, _FieldName}, {IsSimple, StrAcc}) ->
         case FieldType of
            "bool" ->
               TemStr =
                  case IsSimple of
                     true ->
                        GetLeftBinStr1 = getIndexStr(pd_leftBin),
                        UseLeftBinStr1 = useIndexStr(pd_leftBin),
                        "LeftBin" ++ UseLeftBinStr1 ++ "/binary>> = LeftBin" ++ GetLeftBinStr1 ++ ",\n";
                     _ ->
                        ""
                  end,
               UseBoolStr = useIndexStr(pd_bool),
               GetLeftBinStr2 = getIndexStr(pd_leftBin),
               UseLeftBinStr2 = useIndexStr(pd_leftBin),
               BoolStr = "\t<<Bool" ++ UseBoolStr ++ ":8/big-unsigned, LeftBin" ++ UseLeftBinStr2 ++ "/binary>> = LeftBin" ++ GetLeftBinStr2 ++ ",\n",
               UseVStr = useIndexStr(pd_v),
               VStr = "\tV" ++ UseVStr ++ " = Bool" ++ UseBoolStr ++ " =:= 1,\n",
               {false, StrAcc ++ TemStr ++ BoolStr ++ VStr};
            "int8" ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        "V" ++ UseVStr ++ ":8/big-signed, ";
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        "\t<<V" ++ UseVStr ++ ":8/big-signed, "
                  end,
               {true, StrAcc ++ TemStr};
            "uint8" ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        "V" ++ UseVStr ++ ":8/big-unsigned, ";
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        "\t<<V" ++ UseVStr ++ ":8/big-unsigned, "
                  end,
               {true, StrAcc ++ TemStr};
            "int16" ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        "V" ++ UseVStr ++ ":16/big-signed, ";
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        "\t<<V" ++ UseVStr ++ ":16/big-signed, "
                  end,
               {true, StrAcc ++ TemStr};
            "uint16" ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        "V" ++ UseVStr ++ ":16/big-unsigned, ";
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        "\t<<V" ++ UseVStr ++ ":16/big-unsigned, "
                  end,
               {true, StrAcc ++ TemStr};
            "int32" ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        "V" ++ UseVStr ++ ":32/big-signed, ";
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        "\t<<V" ++ UseVStr ++ ":32/big-signed, "
                  end,
               {true, StrAcc ++ TemStr};
            "uint32" ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        "V" ++ UseVStr ++ ":32/big-unsigned, ";
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        "\t<<V" ++ UseVStr ++ ":32/big-unsigned, "
                  end,
               {true, StrAcc ++ TemStr};
            "int64" ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        "V" ++ UseVStr ++ ":64/big-signed, ";
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        "\t<<V" ++ UseVStr ++ ":64/big-signed, "
                  end,
               {true, StrAcc ++ TemStr};
            "uint64" ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        "V" ++ UseVStr ++ ":64/big-unsigned, ";
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        "\t<<V" ++ UseVStr ++ ":64/big-unsigned, "
                  end,
               {true, StrAcc ++ TemStr};
            "float" ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        "V" ++ UseVStr ++ ":32/big-float, ";
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        "\t<<V" ++ UseVStr ++ ":32/big-float, "
                  end,
               {true, StrAcc ++ TemStr};
            "double" ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        "V" ++ UseVStr ++ ":64/big-float, ";
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        "\t<<V" ++ UseVStr ++ ":64/big-float, "
                  end,
               {true, StrAcc ++ TemStr};
            "string" ->
               TemStr =
                  case IsSimple of
                     true ->
                        GetLeftBinStr1 = getIndexStr(pd_leftBin),
                        UseLeftBinStr1 = useIndexStr(pd_leftBin),
                        "LeftBin" ++ UseLeftBinStr1 ++ "/binary>> = LeftBin" ++ GetLeftBinStr1 ++ ",\n";
                     _ ->
                        ""
                  end,
               UseLenStr = useIndexStr(pd_len),
               GetLeftBinStr2 = getIndexStr(pd_leftBin),
               UseLeftBinStr2 = useIndexStr(pd_leftBin),
               UseVStr = useIndexStr(pd_v),
               RefSizeStr =
                  case isCalcRefSize() of
                     false ->
                        useIndexStr(pd_isCalcRefSize),
                        "\tRefSize = binary:referenced_byte_size(LeftBin0),\n";
                     _ ->
                        ""
                  end,
               StrStr = "\t<<Len" ++ UseLenStr ++ ":16/big-unsigned, TemStrV" ++ UseVStr ++ ":Len" ++ UseLenStr ++ "/binary, LeftBin" ++ UseLeftBinStr2 ++ "/binary>> = LeftBin" ++ GetLeftBinStr2 ++ ",\n",
               VStr = "\tcase Len" ++ UseLenStr ++ " < ?BinaryShareSize of\n\t\t" ++
                  "true ->\n\t\t\tV" ++ UseVStr ++ " = TemStrV" ++ UseVStr ++ ";\n\t\t" ++
                  "_ ->\n\t\t\tcase RefSize / Len" ++ UseLenStr ++ " > ?BinaryCopyRatio of\n\t\t\t\t" ++
                  "true ->\n\t\t\t\t\tV" ++ UseVStr ++ " = binary:copy(TemStrV" ++ UseVStr ++ ");\n\t\t\t\t" ++
                  "_ ->\n\t\t\t\t\tV" ++ UseVStr ++ " = TemStrV" ++ UseVStr ++ "\n\t\t\tend\n\tend,\n",
               {false, StrAcc ++ TemStr ++ RefSizeStr ++ StrStr ++ VStr};
            "integer" ->
               TemStr =
                  case IsSimple of
                     true ->
                        UseVStr = useIndexStr(pd_v),
                        UseIntBitsStr = useIndexStr(pd_intBits),
                        "IntBits" ++ UseIntBitsStr ++ ":8, " ++ "V" ++ UseVStr ++ ":IntBits" ++ UseIntBitsStr ++ "/big-signed, ";
                     _ ->
                        UseVStr = useIndexStr(pd_v),
                        UseIntBitsStr = useIndexStr(pd_intBits),
                        "\t<<IntBits" ++ UseIntBitsStr ++ ":8, " ++ "V" ++ UseVStr ++ ":IntBits" ++ UseIntBitsStr ++ "/big-signed, "
                  end,
               {true, StrAcc ++ TemStr};
            "number" ->
               TemStr =
                  case IsSimple of
                     true ->
                        GetLeftBinStr1 = getIndexStr(pd_leftBin),
                        UseLeftBinStr1 = useIndexStr(pd_leftBin),
                        "LeftBin" ++ UseLeftBinStr1 ++ "/binary>> = LeftBin" ++ GetLeftBinStr1 ++ ",\n";
                     _ ->
                        ""
                  end,
               UseNumBitsStr = useIndexStr(pd_numBits),
               GetLeftBinStr2 = getIndexStr(pd_leftBin),
               UseLeftBinStr2 = useIndexStr(pd_leftBin),
               NumStr = "\t<<NumBits" ++ UseNumBitsStr ++ ":8, LeftBin" ++ UseLeftBinStr2 ++ "/binary>> = LeftBin" ++ GetLeftBinStr2 ++ ",\n",
               UseVStr = useIndexStr(pd_v),
               UseLeftBinStr3 = useIndexStr(pd_leftBin),
               VStr =
                  "\tcase NumBits" ++ UseNumBitsStr ++ " of\n\t\t33-> \n\t\t\t<<V" ++ UseVStr ++ ":32/big-float, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ UseLeftBinStr2 ++
                  ";\n\t\t65 ->\n\t\t\t<<V" ++ UseVStr ++ ":64/big-float, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ UseLeftBinStr2 ++
                  ";\n\t\t_ ->\n\t\t\t<<V" ++ UseVStr ++ ":NumBits" ++ UseNumBitsStr ++ "/big-signed, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ UseLeftBinStr2 ++ "\n\tend,\n",
               {false, StrAcc ++ TemStr ++ NumStr ++ VStr};
            "list[" ++ LeftStr ->
               [SubTypeStr | _] = re:split(LeftStr, "\\]", [{return, list}]),
               TemStr =
                  case IsSimple of
                     true ->
                        GetLeftBinStr1 = getIndexStr(pd_leftBin),
                        UseLeftBinStr1 = useIndexStr(pd_leftBin),
                        "LeftBin" ++ UseLeftBinStr1 ++ "/binary>> = LeftBin" ++ GetLeftBinStr1 ++ ",\n";
                     _ ->
                        ""
                  end,

               UseLenStr = useIndexStr(pd_len),
               GetLeftBinStr2 = getIndexStr(pd_leftBin),
               UseLeftBinStr2 = useIndexStr(pd_leftBin),
               UseVStr = useIndexStr(pd_v),
               UseListBinStr = useIndexStr(pd_listBin),
               GetLeftBinStr3 = getIndexStr(pd_leftBin),
               UseLeftBinStr3 = useIndexStr(pd_leftBin),
               LenStr = "\t<<Len" ++ UseLenStr ++ ":16/big-unsigned, LeftBin" ++ UseLeftBinStr2 ++ "/binary>> = LeftBin" ++ GetLeftBinStr2 ++ ",\n",
               DeListStr =
                  case SubTypeStr of
                     "bool" ->
                        ListBinStr = "\t<<ListBin" ++ UseListBinStr ++ ":Len" ++ UseLenStr ++ "/big-binary-unit:8, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ GetLeftBinStr3 ++ ",\n",
                        VStr = "\tV" ++ UseVStr ++ " = [TemV =:= 1 || <<TemV:8/big-unsigned>> <= ListBin" ++ UseListBinStr ++ "],\n",
                        ListBinStr ++ VStr;
                     "int8" ->
                        ListBinStr = "\t<<ListBin" ++ UseListBinStr ++ ":Len" ++ UseLenStr ++ "/big-binary-unit:8, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ GetLeftBinStr3 ++ ",\n",
                        VStr = "\tV" ++ UseVStr ++ " = [TemV || <<TemV:8/big-signed>> <= ListBin" ++ UseListBinStr ++ "],\n",
                        ListBinStr ++ VStr;
                     "uint8" ->
                        ListBinStr = "\t<<ListBin" ++ UseListBinStr ++ ":Len" ++ UseLenStr ++ "/big-binary-unit:8, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ GetLeftBinStr3 ++ ",\n",
                        VStr = "\tV" ++ UseVStr ++ " = [TemV || <<TemV:8/big-unsigned>> <= ListBin" ++ UseListBinStr ++ "],\n",
                        ListBinStr ++ VStr;
                     "int16" ->
                        ListBinStr = "\t<<ListBin" ++ UseListBinStr ++ ":Len" ++ UseLenStr ++ "/big-binary-unit:16, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ GetLeftBinStr3 ++ ",\n",
                        VStr = "\tV" ++ UseVStr ++ " = [TemV || <<TemV:16/big-signed>> <= ListBin" ++ UseListBinStr ++ "],\n",
                        ListBinStr ++ VStr;
                     "uint16" ->
                        ListBinStr = "\t<<ListBin" ++ UseListBinStr ++ ":Len" ++ UseLenStr ++ "/big-binary-unit:16, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ GetLeftBinStr3 ++ ",\n",
                        VStr = "\tV" ++ UseVStr ++ " = [TemV || <<TemV:16/big-unsigned>> <= ListBin" ++ UseListBinStr ++ "],\n",
                        ListBinStr ++ VStr;
                     "int32" ->
                        ListBinStr = "\t<<ListBin" ++ UseListBinStr ++ ":Len" ++ UseLenStr ++ "/big-binary-unit:32, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ GetLeftBinStr3 ++ ",\n",
                        VStr = "\tV" ++ UseVStr ++ " = [TemV || <<TemV:32/big-signed>> <= ListBin" ++ UseListBinStr ++ "],\n",
                        ListBinStr ++ VStr;
                     "uint32" ->
                        ListBinStr = "\t<<ListBin" ++ UseListBinStr ++ ":Len" ++ UseLenStr ++ "/big-binary-unit:32, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ GetLeftBinStr3 ++ ",\n",
                        VStr = "\tV" ++ UseVStr ++ " = [TemV || <<TemV:32/big-unsigned>> <= ListBin" ++ UseListBinStr ++ "],\n",
                        ListBinStr ++ VStr;
                     "int64" ->
                        ListBinStr = "\t<<ListBin" ++ UseListBinStr ++ ":Len" ++ UseLenStr ++ "/big-binary-unit:64, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ GetLeftBinStr3 ++ ",\n",
                        VStr = "\tV" ++ UseVStr ++ " = [TemV || <<TemV:64/big-signed>> <= ListBin" ++ UseListBinStr ++ "],\n",
                        ListBinStr ++ VStr;
                     "uint64" ->
                        ListBinStr = "\t<<ListBin" ++ UseListBinStr ++ ":Len" ++ UseLenStr ++ "/big-binary-unit:64, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ GetLeftBinStr3 ++ ",\n",
                        VStr = "\tV" ++ UseVStr ++ " = [TemV || <<TemV:64/big-unsigned>> <= ListBin" ++ UseListBinStr ++ "],\n",
                        ListBinStr ++ VStr;
                     "integer" ->
                        "\t{V" ++ UseVStr ++ ", LeftBin" ++ UseLeftBinStr3 ++ "} = deIntegerList(Len" ++ UseLenStr ++ ", LeftBin" ++ GetLeftBinStr3 ++ ", []),\n";
                     "number" ->
                        "\t{V" ++ UseVStr ++ ", LeftBin" ++ UseLeftBinStr3 ++ "} = deNumberList(Len" ++ UseLenStr ++ ", LeftBin" ++ GetLeftBinStr3 ++ ", []),\n";
                     "float" ->
                        ListBinStr = "\t<<ListBin" ++ UseListBinStr ++ ":Len" ++ UseLenStr ++ "/big-binary-unit:32, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ GetLeftBinStr3 ++ ",\n",
                        VStr = "\tV" ++ UseVStr ++ " = [TemV || <<TemV:32/big-float>> <= ListBin" ++ UseListBinStr ++ "],\n",
                        ListBinStr ++ VStr;
                     "double" ->
                        ListBinStr = "\t<<ListBin" ++ UseListBinStr ++ ":Len" ++ UseLenStr ++ "/big-binary-unit:64, LeftBin" ++ UseLeftBinStr3 ++ "/binary>> = LeftBin" ++ GetLeftBinStr3 ++ ",\n",
                        VStr = "\tV" ++ UseVStr ++ " = [TemV || <<TemV:64/big-float>> <= ListBin" ++ UseListBinStr ++ "],\n",
                        ListBinStr ++ VStr;
                     "string" ->
                        case isCalcRefSize() of
                           true ->
                              "\t{V" ++ UseVStr ++ ", LeftBin" ++ UseLeftBinStr3 ++ "} = deStringList(Len" ++ UseLenStr ++ ", LeftBin" ++ GetLeftBinStr3 ++ ", RefSize, []),\n";
                           _ ->
                              useIndexStr(pd_isCalcRefSize),
                              RefSizeStr = "\tRefSize = binary:referenced_byte_size(LeftBin0),\n",
                              VStr = "\t{V" ++ UseVStr ++ ", LeftBin" ++ UseLeftBinStr3 ++ "} = deStringList(Len" ++ UseLenStr ++ ", LeftBin" ++ GetLeftBinStr3 ++ ", RefSize, []),\n",
                              RefSizeStr ++ VStr
                        end;
                     ListRecord ->
                        case lists:keyfind(ListRecord, 1, SortedSProtoList) of
                           {ListRecord, ListMsgId, _} = RecordInfo ->
                              addSubRec(RecordInfo, IsForBin),
                              "\t{V" ++ UseVStr ++ ", LeftBin" ++ UseLeftBinStr3 ++ "} = deRecordList(Len" ++ UseLenStr ++ ", " ++ integer_to_list(ListMsgId) ++ ", LeftBin" ++ GetLeftBinStr3 ++ ", []),\n";
                           _ ->
                              io:format("this an Record undefined :~p~n", [ListRecord]),
                              throw(record_undefined)
                        end
                  end,
               {false, StrAcc ++ TemStr ++ LenStr ++ DeListStr};
            OtherRecord ->
               TemStr =
                  case IsSimple of
                     true ->
                        GetLeftBinStr1 = getIndexStr(pd_leftBin),
                        UseLeftBinStr1 = useIndexStr(pd_leftBin),
                        "LeftBin" ++ UseLeftBinStr1 ++ "/binary>> = LeftBin" ++ GetLeftBinStr1 ++ ",\n";
                     _ ->
                        ""
                  end,
               UseIsUndefStr = useIndexStr(pd_isUndef),
               GetLeftBinStr2 = getIndexStr(pd_leftBin),
               UseLeftBinStr2 = useIndexStr(pd_leftBin),
               IsStr = "\t<<IsUndef" ++ UseIsUndefStr ++ ":8/big-unsigned, LeftBin" ++ UseLeftBinStr2 ++ "/binary>> = LeftBin" ++ GetLeftBinStr2 ++ ",\n",
               UseVStr = useIndexStr(pd_v),
               UseLeftBinStr3 = useIndexStr(pd_leftBin),
               case lists:keyfind(OtherRecord, 1, SortedSProtoList) of
                  {OtherRecord, OtherMsgId, _} = RecordInfo ->
                     addSubRec(RecordInfo, IsForBin),
                     VStr = "\tcase IsUndef" ++ UseIsUndefStr ++ " of\n\t\t0 ->\n\t\t\tV" ++ UseVStr ++ " = undefined,\n\t\t\tLeftBin" ++ UseLeftBinStr3 ++ " = LeftBin" ++ UseLeftBinStr2 ++
                        " ;\n\t\t_ ->\n\t\t\t{V" ++ UseVStr ++ ", LeftBin" ++ UseLeftBinStr3 ++ "} = " ++ "decodeRec(" ++ integer_to_list(OtherMsgId) ++ ", LeftBin" ++ UseLeftBinStr2 ++ ")\n\tend,\n",
                     {false, StrAcc ++ TemStr ++ IsStr ++ VStr};
                  _ ->
                     io:format("this an Record undefined :~p~n", [OtherRecord]),
                     throw(record_undefined)
               end
         end
      end,

   {LIsSimple, BodyStr} = lists:foldl(FunBody, {false, ""}, FieldList),
   LBodyStr =
      case LIsSimple of
         true ->
            LGetLeftBinStr = getIndexStr(pd_leftBin),
            LUseLeftBinStr = useIndexStr(pd_leftBin),
            BodyStr ++ "LeftBin" ++ LUseLeftBinStr ++ "/binary>> = LeftBin" ++ LGetLeftBinStr ++ ",\n";
         _ ->
            BodyStr
      end,

   RetStr =
      case IsForBin of
         true ->
            case FieldLen > 0 of
               true ->
                  FunRec =
                     fun(N, Acc) ->
                        ", V" ++ integer_to_list(N) ++ Acc

                     end,
                  RecStr = lists:foldr(FunRec, "", lists:seq(1, FieldLen)),
                  "\t{" ++ MsgName ++ RecStr ++ "};\n";
               _ ->
                  "\t{" + MsgName ++ "};\n"
            end;
         _ ->
            case FieldLen > 0 of
               true ->
                  FunRec =
                     fun(N, Acc) ->
                        ", V" ++ integer_to_list(N) ++ Acc

                     end,
                  RecStr = lists:foldr(FunRec, "", lists:seq(1, FieldLen)),
                  "\tMsgRec = {" ++ MsgName ++ RecStr ++ "},\n"
                  "\t{MsgRec, LeftBin" ++ getIndexStr(pd_leftBin) ++ "};\n";
               _ ->
                  "\t{{" + MsgName ++ "}, " ++ "<<>>};\n"
            end
      end,
   HeadStr ++ LBodyStr ++ RetStr.

convertFile(File) ->
   protoParse:parseFile(File).

convert([ProtoDir, HrlDir, ErlDir]) ->
   convertDir(atom_to_list(ProtoDir), atom_to_list(HrlDir), atom_to_list(ErlDir)).

convertDir() ->
   convertDir("./", "./", "./").
convertDir(ProtoDir) ->
   convertDir(ProtoDir, "./", "./").
convertDir(ProtoDir, HrlDir, ErlDir) ->
   erlang:put(pd_errlist, []),
   FunRead =
      fun(File, ProAcc) ->
         case filename:extension(File) == ".mpdf" of
            true ->
               io:format("Convert proto msg file: ~s ~n", [File]),
               BaseName = filename:basename(File, ".mpdf"),
               [ModIndex | _ModName] = re:split(BaseName, "_"),
               Index = binary_to_integer(ModIndex),
               erlang:put(pd_messageid, Index * 1000 + 1),
               erlang:put(pd_errcodeid, Index * 1000 + 1),
               SProto = protoParse:parseFile(File),
               ErrCode = erlang:get(pd_errlist),
               erlang:erase(),
               erlang:put(pd_errlist, ErrCode),
               [SProto | ProAcc];
            _ ->
               ProAcc
         end
      end,
   %% 下面文件帅选并不能准确的帅选出文件名为.mpdf结尾的文件 在FunRead函数中纠正处理一下
   SProtoListOfList = filelib:fold_files(ProtoDir, "\\.mpdf$", true, FunRead, []),
   SProtoList = lists:append(SProtoListOfList),
   ErrCodeList = erlang:get(pd_errlist),
   initSubRec(),

   SortedSProtoList = lists:sort(fun({_Name1, MessageId1, _FieldList1}, {_Name2, MessageId2, _FieldList2}) ->
      MessageId1 > MessageId2 end, SProtoList),

   FunSpell =
      fun({MsgName, _MsgId, FieldList} = MsgInfo, {MsgHrlAcc, _MsgIdAcc, MsgEncodeAcc, MsgDecodeAcc}) ->
         %% gen hrl str
         Len = erlang:length(FieldList),
         {_, Len, LastFieldStr} = lists:foldr(fun genMsgHrl/2, {Len, Len, ""}, FieldList),
         HrlStr = "-record(" ++ MsgName ++ ", {\n\t" ++ LastFieldStr ++ "}).\n",
         %% gen getMsgId  getMsgType str
         %% TypeStr = "getMsgType(" ++ integer_to_list(MsgId) ++ ")-> " ++ MsgName ++ ";\n",
         %% IdStr = "getMsgId(" ++ MsgName ++ ")-> " ++ integer_to_list(MsgId) ++ ";\n",

         %% gen encodeRec Str
         EncodeStr = genEncodeRec(MsgInfo, true),

         %% gen decodeBin str
         resetPd(),
         DecodeStr = genDecodeBin(MsgInfo, SortedSProtoList, true),

         {[HrlStr | MsgHrlAcc], ["Unuse"], [EncodeStr | MsgEncodeAcc], [DecodeStr | MsgDecodeAcc]}
      end,
   {MsgHrlStr, _MsgIdStr, MsgEncodeStr, MsgDecodeStr} = lists:foldl(FunSpell, {[], ["getMsgId(_) -> 0.\n\n"], ["encode(_) ->\n\t[].\n\n"], ["decodeBin(_, _) ->\n\t{{}, <<>>}.\n\n"]}, SortedSProtoList),

   SortedErrList = lists:sort(fun({_ErrName1, ErrCodeId1, _Desc1}, {_ErrName2, ErrCodeId2, _Desc2}) ->
      ErrCodeId1 > ErrCodeId2 end, ErrCodeList),
   ErrCodeStr = lists:foldl(fun genErrCodeHrl/2, [], SortedErrList) ++ "\n\n",

   %% gen decodeRec str
   SubRecList = getSubRec(),
   SortedSubRecList = lists:sort(fun({_Name1, MessageId1, _FieldList1}, {_Name2, MessageId2, _FieldList2}) ->
      MessageId1 > MessageId2 end, SubRecList),

   FunSubRec =
      fun(MsgInfo, {SubEncodeAcc, SubDecodeAcc}) ->
         EncodeStr = genEncodeRec(MsgInfo, false),
         resetPd(),
         DecodeStr = genDecodeBin(MsgInfo, SortedSProtoList, false),
         {[EncodeStr | SubEncodeAcc], [DecodeStr | SubDecodeAcc]}
      end,
   {MsgEncodeRecStr, MsgDecodeRecStr} = lists:foldl(FunSubRec, {["encodeRec(_) ->\n\t[].\n\n"], ["decodeRec(_, _) ->\n\t{{}, <<>>}.\n\n"]}, SortedSubRecList),

   ErlHeaderStr = protoErlHeader(),
   OutputStr = ErlHeaderStr ++ MsgEncodeRecStr ++ MsgEncodeStr ++ MsgDecodeRecStr ++ MsgDecodeStr,
   HrlFilename = do_write_hrl(HrlDir, protoMsg, protoHrlHeader() ++ ErrCodeStr ++ MsgHrlStr),
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

