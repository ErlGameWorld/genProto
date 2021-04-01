-module(genProto).

-include("genDef.hrl").

-export([
   convertFile/1
   , convert/1
   , convertDir/0
   , convertDir/1
   , convertDir/3
]).

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
   erlang:put(pd_handler, []),
   FunRead =
      fun(File, ProAcc) ->
         case filename:extension(File) == ".mpdf" of
            true ->
               io:format("Convert proto msg file: ~s ~n", [File]),
               BaseName = filename:basename(File, ".mpdf"),
               [ModIndex, ModName] = re:split(BaseName, "_"),
               Index = binary_to_integer(ModIndex),
               erlang:put(pd_messageid, Index * ?MsgIdSegSize + 1),
               erlang:put(pd_handler, [{Index, ModName} | erlang:get(pd_handler)]),
               erlang:put(pd_errcodeid, Index * ?MsgIdSegSize + 1),
               SProto = protoParse:parseFile(File),
               ErrCode = erlang:get(pd_errlist),
               Handler = erlang:get(pd_handler),
               erlang:erase(),
               erlang:put(pd_errlist, ErrCode),
               erlang:put(pd_handler, Handler),
               [SProto | ProAcc];
            _ ->
               ProAcc
         end
      end,

   SProtoListOfList = filelib:fold_files(ProtoDir, "\\.mpdf$", true, FunRead, []),
   SProtoList = lists:append(SProtoListOfList),
   ErrCodeList = erlang:get(pd_errlist),

   SortedSProtoList = lists:sort(fun({_Name1, MessageId1, _FieldList1}, {_Name2, MessageId2, _FieldList2}) ->
      MessageId1 > MessageId2 end, SProtoList),

   SortedErrList = lists:sort(fun({_ErrName1, ErrCodeId1, _Desc1}, {_ErrName2, ErrCodeId2, _Desc2}) ->
      ErrCodeId1 > ErrCodeId2 end, ErrCodeList),
   gErlGen:genErl(SortedSProtoList, SortedErrList, HrlDir, ErlDir).
%% 如果有其他语言请在这里添加入口函数