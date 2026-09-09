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
	, printHelp/0
]).

main(Args) ->
	case Args of
		[] ->
			printHelp();
		["-h"] ->
			printHelp();
		_ ->
			convertDir(Args)
	end.

printHelp() ->
	io:format(
		"Usage:~n"
		"  genProto [ProtoDir] [Lang OutDir ...]~n"
		"  genProto help | -h | --help~n"
		"~n"
		"Arguments:~n"
		"  ProtoDir   Directory containing .mpdf files (default: ./)~n"
		"~n"
		"Language options (can be chained):~n"
		"  all  OutDir              Generate erl + cs + lua + ts into OutDir~n"
		"  erl  HrlDir ErlDir       Generate Erlang .hrl and .erl~n"
		"  cs   CsDir               Generate C#~n"
		"  lua  LuaDir              Generate Lua~n"
		"  ts   TsDir               Generate TypeScript~n"
		"  dump_proto               Pretty-print all SortedSProtoList~n"
		"  dump_ecode               Pretty-print all SortedErrList~n"
		"~n"
		"Examples:~n"
		"  genProto~n"
		"  genProto proto all ./out~n"
		"  genProto proto erl ./erl ./erl ts ./ts cs ./cs lua ./lua~n"
		"  genProto proto dump_proto~n"
		"  genProto proto dump_proto erl ./erl ./erl~n"
		"  genProto help~n"
	).


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

	SortedSProtoList = lists:sort(fun({_Name1, MessageId1, _FieldList1}, {_Name2, MessageId2, _FieldList2}) -> MessageId1 < MessageId2 end, SProtoList),
	SortedErrList = lists:sort(fun({_ErrName1, ErrCodeId1, _Desc1}, {_ErrName2, ErrCodeId2, _Desc2}) -> ErrCodeId1 < ErrCodeId2 end, ErrCodeList),
	genProto(LeftArgs, SortedSProtoList, SortedErrList).

%% 如果有其他语言请在这里添加入口函数
genProto([], _SortedSProtoList, _SortedErrList) ->
	ok;
genProto(["help" | _], _SortedSProtoList, _SortedErrList) ->
	printHelp();
genProto(["-h" | _], _SortedSProtoList, _SortedErrList) ->
	printHelp();
genProto(["--help" | _], _SortedSProtoList, _SortedErrList) ->
	printHelp();
genProto(["dump_proto" | LeftArgs], SortedSProtoList, SortedErrList) ->
	io:format("~nSortedSProtoList =~n~p~n~n", [SortedSProtoList]),
	genProto(LeftArgs, SortedSProtoList, SortedErrList);
genProto(["dump_ecode" | LeftArgs], SortedSProtoList, SortedErrList) ->
	io:format("~nSortedErrList =~n~p~n~n", [SortedErrList]),
	genProto(LeftArgs, SortedSProtoList, SortedErrList);
genProto(["all", AllDir], SortedSProtoList, SortedErrList) ->
	gErlGen:genErl(SortedSProtoList, SortedErrList, AllDir, AllDir),
	gCsGen:genCs(SortedSProtoList, SortedErrList, AllDir, AllDir),
	gLuaGen:genLua(SortedSProtoList, SortedErrList, AllDir, AllDir),
	gTsGen:genTs(SortedSProtoList, SortedErrList, AllDir, AllDir);
genProto(["erl", HrlDir, ErlDir | LeftArgs], SortedSProtoList, SortedErrList) ->
	gErlGen:genErl(SortedSProtoList, SortedErrList, HrlDir, ErlDir),
	genProto(LeftArgs, SortedSProtoList, SortedErrList);
genProto(["cs", CsDir | LeftArgs], SortedSProtoList, SortedErrList) ->
	gCsGen:genCs(SortedSProtoList, SortedErrList, CsDir, CsDir),
	genProto(LeftArgs, SortedSProtoList, SortedErrList);
genProto(["lua", LuaDir | LeftArgs], SortedSProtoList, SortedErrList) ->
	gLuaGen:genLua(SortedSProtoList, SortedErrList, LuaDir, LuaDir),
	genProto(LeftArgs, SortedSProtoList, SortedErrList);
genProto(["ts", TsDir | LeftArgs], SortedSProtoList, SortedErrList) ->
	gTsGen:genTs(SortedSProtoList, SortedErrList, TsDir, TsDir),
	genProto(LeftArgs, SortedSProtoList, SortedErrList);
genProto([Unknown | _], _SortedSProtoList, _SortedErrList) ->
	io:format("Unknown option: ~s~n~n", [Unknown]),
	printHelp(), halt(1).

