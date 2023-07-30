-module(genProto).

-include("genDef.hrl").

-define(DefProtoDir, "./").
-define(DefErlDir, "./").
-define(DefHrlDir, "./").

-export([main/1]).

-export([
	convertFile/1
	, convert/1
	, convertDir/0
	, convertDir/1
]).

main(Args) ->
	case Args of
		[] ->
			convertDir(["./", "all", "./"]);
		_ ->
			convertDir(Args)
	end.


convertFile(File) ->
	erlang:erase(),
	erlang:put(pd_errlist, []),
	erlang:put(pd_handler, []),
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
			{SProto, Handler, ErrCode};
		_ ->
			io:format("not proto msg file: ~s ~n", [File])
	end.

convert(Args) ->
	convertDir(Args).

convertDir() ->
	convertDir(["./", "all", "./"]).
convertDir([ProtoDir | LeftArgs]) ->
	erlang:erase(),
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
		MessageId1 < MessageId2 end, SProtoList),
	SortedErrList = lists:sort(fun({_ErrName1, ErrCodeId1, _Desc1}, {_ErrName2, ErrCodeId2, _Desc2}) ->
		ErrCodeId1 < ErrCodeId2 end, ErrCodeList),
	genProto(LeftArgs, SortedSProtoList, SortedErrList).

%% 如果有其他语言请在这里添加入口函数
genProto([], _SortedSProtoList, _SortedErrList) ->
	ok;
genProto(["all", AllDir], SortedSProtoList, SortedErrList) ->
	gErlGen:genErl(SortedSProtoList, SortedErrList, AllDir, AllDir),
	gCsGen:genCs(SortedSProtoList, SortedErrList, AllDir, AllDir),
	gLuaGen:genLua(SortedSProtoList, SortedErrList, AllDir, AllDir);
genProto(["erl", HrlDir, ErlDir | LeftArgs], SortedSProtoList, SortedErrList) ->
	gErlGen:genErl(SortedSProtoList, SortedErrList, HrlDir, ErlDir),
	genProto(LeftArgs, SortedSProtoList, SortedErrList);
genProto(["cs", CsDir | LeftArgs], SortedSProtoList, SortedErrList) ->
	gCsGen:genCs(SortedSProtoList, SortedErrList, CsDir, CsDir),
	genProto(LeftArgs, SortedSProtoList, SortedErrList);
genProto(["lua", LuaDir | LeftArgs], SortedSProtoList, SortedErrList) ->
	gLuaGen:genLua(SortedSProtoList, SortedErrList, LuaDir, LuaDir),
	genProto(LeftArgs, SortedSProtoList, SortedErrList).

