%%% @doc .mpdf 协议定义文件解析器（PEG 风格）。
%%%
%%% 本模块负责把 proto 目录下的 .mpdf 文本解析成结构化 AST，供 genProto 各语言代码生成器使用。
%%%
%%% <b>语法概览（EBNF 风格）</b>
%%% <pre>
%%%   file        := { protocol }
%%%   protocol    := name '{' { field } '}'
%%%   field       := normal_field | oneof_field
%%%   normal_field:= typename name ';'
%%%   oneof_field := 'oneof' name '{' { normal_field } '}'
%%%   typename    := map_typename | list_typename | word
%%%   map_typename:= 'map&lt;' blank0 typename blank0 ',' blank0 typename blank0 '&gt;'
%%%   list_typename:= 'list[' blank0 typename blank0 ']'
%%% </pre>
%%%
%%% <b>解析结果</b>
%%% 成功时返回消息列表，每个元素为：
%%% `{MsgName :: binary(), MsgId :: integer(), FieldList :: [field()]}`
%%%
%%% 其中 field() 为：
%%% - 普通字段：`{TypeName :: binary(), FieldName :: binary()}`
%%%   例：`{<<"int32">>, <<"id">>}`、`{<<"list[string]">>, <<"names">>}`、`{<<"map<int32,string>">>, <<"m">>}`
%%% - oneof 字段：`{oneof, OneofName :: binary(), Branches :: [field()]}`
%%%
%%% <b>副作用（进程字典）</b>
%%% 解析过程中会向进程字典写入：
%%% - `pd_messageid` — 当前消息 ID，每解析完一个 protocol 自增（调用方 genProto 在解析前初始化）
%%% - `pd_errlist`   — 注释里 `$errcode[name:描述, ...]` 收集到的错误码列表
%%% - `pd_errcodeid` — 错误码 ID 计数器
%%%
%%% <b>实现说明</b>
%%% 下半部分的 `p/2`、`p_seq`、`p_choose` 等是 PEG 解析器运行时（带 memo 记忆化），
%%% 上半部分 `all/2`、`protocol/2` 等是具体的语法规则。
-module(protoParse).
-compile(nowarn_unused_function).

-export([
   parseParse/1
   , parseFile/1
]).

%% PEG 组合子开关：下方 peg 运行时通过 -ifdef 按需编译对应函数
-define(p_anything, true).
-define(p_charclass, true).
-define(p_choose, true).
-define(p_label, true).
-define(p_not, true).
-define(p_one_or_more, true).
-define(p_optional, true).
-define(p_scan, true).
-define(p_seq, true).
-define(p_string, true).
-define(p_zero_or_more, true).

%% ============================================================================
%% 对外入口
%% ============================================================================

%% @doc 读取 .mpdf 文件并解析，Filename 为文件路径。
-spec parseFile(file:name()) -> any().
parseFile(Filename) ->
   case file:read_file(Filename) of
      {ok, Bin} ->
         parseParse(Bin);
      Err -> Err
   end.

%% @doc 解析二进制或字符串形式的协议文本。
%% 成功返回 `[{MsgName, MsgId, Fields}, ...]`；失败返回 `{fail, Reason}` 或 `{Transformed, Rest, Index}`。
%%
%% 注意：调用前须由 genProto 初始化 pd_messageid / pd_errcodeid / pd_errlist，
%% 否则 MsgId 自增会因 undefined 崩溃。同进程多次解析建议先 erlang:erase() 清理 memo 缓存。
-spec parseParse(binary() | list()) -> any().
parseParse(List) when is_list(List) ->
   parseParse(unicode:characters_to_binary(List));
parseParse(Input) when is_binary(Input) ->
   Result =
      case 'all'(Input, {{line, 1}, {column, 1}}) of
         {AST, <<>>, _Index} ->
            AST;
         Any ->
            Any
      end,
   Result.

%% ============================================================================
%% 语法规则（自上而下）
%% ============================================================================

%% @doc 整个文件：跳过开头空白，重复解析 protocol，必须消耗完所有输入。
%% 返回：`[{MsgName, MsgId, FieldList}, ...]`
-spec 'all'(input(), index()) -> parse_result().
'all'(Input, Index) ->
   p(Input, Index, 'all',
      fun(I, D) ->
         (p_seq([fun 'blank0'/2, p_zero_or_more(p_seq([fun 'protocol'/2, fun 'blank0'/2]))]))(I, D)
      end,
      fun(Node, _Idx) ->
         %% Node = [Blank, [ [Protocol, Blank], [Protocol, Blank], ... ]]
         [_ | [T]] = Node, DataList = [H || [H | _] <- T], DataList
      end).

%% @doc 一条消息/结构体定义：`Name { ...fields... }`
%% 返回：`{MsgName, MsgId, FieldList}`
%% MsgId 从进程字典 pd_messageid 读取并自增（由 genProto 按文件分段初始化）。
-spec 'protocol'(input(), index()) -> parse_result().
'protocol'(Input, Index) ->
   p(Input, Index, 'protocol',
      fun(I, D) ->
         (p_seq([p_label('name', fun 'name'/2), fun 'blank0'/2, p_label('structural', fun 'structural'/2)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Name = iolist_to_binary(proplists:get_value(name, Node)),
         Structural = proplists:get_value(structural, Node),
         MsgId = erlang:get(pd_messageid),
         erlang:put(pd_messageid, MsgId + 1),
         {Name, MsgId, Structural}
      end).

%% @doc 消息体：`{ field field ... }`，提取中间所有 field 的 AST。
-spec 'structural'(input(), index()) -> parse_result().
'structural'(Input, Index) ->
   p(Input, Index, 'structural',
      fun(I, D) ->
         (p_seq([p_string(<<"{">>), fun 'blank0'/2, p_zero_or_more(p_seq([fun 'field'/2, fun 'blank0'/2])), p_string(<<"}">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         %% Node = [<<"{">>, Blank0, FieldList, <<"}">> ]，取第 3 项字段列表
         [_, _, List, _] = Node, [H || [H | _] <- List]
      end).

%% @doc 字段分发：优先匹配 oneof_field，否则匹配 normal_field。
%% p_choose 按顺序尝试，oneof 关键字开头走 oneof 分支，其余走普通字段。
-spec 'field'(input(), index()) -> parse_result().
'field'(Input, Index) ->
   p(Input, Index, 'field',
      fun(I, D) ->
         (p_choose([fun 'oneof_field'/2, fun 'normal_field'/2]))(I, D)
      end,
      fun(Node, _Idx) ->
         %% p_choose 直接返回选中分支的 AST，无需再转换
         Node
      end).
%% 返回：`{TypeName, FieldName}`，如 `{<<"int32">>, <<"id">>}`。
-spec 'normal_field'(input(), index()) -> parse_result().
'normal_field'(Input, Index) ->
   p(Input, Index, 'normal_field',
      fun(I, D) ->
         (p_seq([p_label('datatype', fun 'typename'/2), fun 'blanks'/2, p_label('name', fun 'name'/2), fun 'blank0'/2, p_string(<<";">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         DataType = iolist_to_binary(proplists:get_value(datatype, Node)),
         Name = iolist_to_binary(proplists:get_value(name, Node)),
         {DataType, Name}
      end).

%% @doc oneof 字段：`oneof name { field; field; ... }`
%% 返回：`{oneof, OneofName, Branches}`，Branches 为 normal_field 列表。
-spec 'oneof_field'(input(), index()) -> parse_result().
'oneof_field'(Input, Index) ->
   p(Input, Index, 'oneof_field',
      fun(I, D) ->
			(p_seq([p_string(<<"oneof">>), fun 'blanks'/2, p_label('name', fun 'name'/2), fun 'blank0'/2, p_string(<<"{">>), fun 'blank0'/2, p_zero_or_more(p_seq([fun 'normal_field'/2, fun 'blank0'/2])), p_string(<<"}">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Name = iolist_to_binary(proplists:get_value(name, Node)),
         %% Node 结构固定为 [<<"oneof">>, Blanks, {name,...}, Blank0, <<"{">>, Blank0, FieldList, <<"}">> ]
         %% 第 7 项为 FieldList；若调整 seq 组成，此处需同步修改（建议后续改用 proplists）
         FieldList = lists:nth(7, Node),
         Branches = [H || [H | _] <- FieldList],
         {oneof, Name, Branches}
      end).

%% ============================================================================
%% 词法/空白/注释
%% ============================================================================

%% @doc 文件/输入结束探测，用于行注释末尾（文件最后一行可以没有换行符）。
%% 仅当剩余输入为空 <<>> 时成功；此前误用 p_not(p_string("."))，语义不正确且命名误导。
-spec 'eof'(input(), index()) -> parse_result().
'eof'(Input, Index) ->
   p(Input, Index, 'eof',
      fun(I, D) ->
         case I of
            <<>> ->
               {[], <<>>, D};
            _ ->
               {fail, {expected, eof, D}}
         end
      end,
      fun(Node, _Idx) ->
         Node
      end).

%% @doc 换行：`\r\n` 或 `\n`。
-spec 'newline'(input(), index()) -> parse_result().
'newline'(Input, Index) ->
   p(Input, Index, 'newline',
      fun(I, D) ->
         (p_seq([p_optional(p_charclass(<<"[\r]">>)), p_charclass(<<"[\n]">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

%% @doc 行注释：`//` 或 `%%` 开头直到换行。
%% 注释中可嵌入 `$errcode[err1:描述, err2:描述]`，解析时会触发 errcode/2 收集错误码。
-spec 'line_comment'(input(), index()) -> parse_result().
'line_comment'(Input, Index) ->
   p(Input, Index, 'line_comment',
      fun(I, D) ->
         (p_seq([p_choose([p_string(<<"//">>), p_string(<<"%%">>)]), p_zero_or_more(p_seq([p_not(fun 'newline'/2), p_not(p_string(<<"$errcode[">>)), p_anything()])), p_zero_or_more(p_seq([p_string(<<"$errcode[">>), p_zero_or_more(fun 'errcode'/2), p_string(<<"]">>)])), p_zero_or_more(p_seq([p_not(fun 'newline'/2), p_anything()])), p_choose([fun 'newline'/2, fun 'eof'/2])]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

%% @doc 单个错误码项：`errName : 描述 ,`（在 $errcode[...] 块内）。
%% 副作用：去重后追加到 pd_errlist，格式 `{UpperName, ErrCodeId, Desc}`。
-spec 'errcode'(input(), index()) -> parse_result().
'errcode'(Input, Index) ->
   p(Input, Index, 'errcode',
      fun(I, D) ->
         (p_seq([fun 'blank0'/2, p_label('errname', fun 'name'/2), fun 'blank0'/2, p_string(<<":">>), fun 'blank0'/2, p_label('errcode_str', fun 'errcode_str'/2), p_choose([p_charclass(<<",">>), fun 'blank0'/2])]))(I, D)
      end,
      fun(Node, _Idx) ->
         ErrNameList = proplists:get_value('errname', Node),
         ErrCodeStrList = proplists:get_value('errcode_str', Node),
         ErrName = iolist_to_binary(ErrNameList),
         Desc = iolist_to_binary(ErrCodeStrList),
         ErrList = erlang:get(pd_errlist),
         UpErrName = toUpperStr(ErrName),
         case UpErrName =/= <<>> andalso UpErrName =/= [] andalso UpErrName =/= "" andalso lists:keyfind(UpErrName, 1, ErrList) == false of
            true ->
               ErrCodeId = erlang:get(pd_errcodeid),
               erlang:put(pd_errlist, [{UpErrName, ErrCodeId, Desc} | ErrList]),
               erlang:put(pd_errcodeid, ErrCodeId + 1);
            _ ->
               skip
         end,
         Node
      end).

%% @doc 空白字符：空格/制表符/换行/行注释 四选一。
-spec 'blank'(input(), index()) -> parse_result().
'blank'(Input, Index) ->
   p(Input, Index, 'blank',
      fun(I, D) ->
         (p_choose([p_charclass(<<"[\s\t]">>), fun 'newline'/2, fun 'line_comment'/2]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

%% @doc 零个或多个 blank（允许没有空白）。
-spec 'blank0'(input(), index()) -> parse_result().
'blank0'(Input, Index) ->
   p(Input, Index, 'blank0',
      fun(I, D) ->
         (p_zero_or_more(fun 'blank'/2))(I, D)
      end,
      fun(Node, _Idx) ->
         %% 空白/注释仅用于跳过，Transform 丢弃具体内容
         Node
      end).

%% @doc 一个或多个 blank（类型名与字段名之间至少有一个分隔符）。
-spec 'blanks'(input(), index()) -> parse_result().
'blanks'(Input, Index) ->
   p(Input, Index, 'blanks',
      fun(I, D) ->
         (p_one_or_more(fun 'blank'/2))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

%% ============================================================================
%% 标识符与类型名
%% ============================================================================

%% @doc 标识符首字符：字母或下划线（a-z、A-Z、_）。
%% list/map 类型由 list_typename/map_typename 专门解析，word 不再允许 `[` `]` 开头。
-spec 'alpha'(input(), index()) -> parse_result().
'alpha'(Input, Index) ->
   p(Input, Index, 'alpha',
      fun(I, D) ->
         (p_choose([p_charclass(<<"[a-z]">>), p_charclass(<<"[A-Z]">>), p_string(<<"_">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

%% @doc 标识符后续字符：alpha 或数字。
-spec 'alnum'(input(), index()) -> parse_result().
'alnum'(Input, Index) ->
   p(Input, Index, 'alnum',
      fun(I, D) ->
         (p_choose([fun 'alpha'/2, p_charclass(<<"[0-9]">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

%% @doc 单词：`alpha alnum*`（消息名、字段名、基础类型名）。
-spec 'word'(input(), index()) -> parse_result().
'word'(Input, Index) ->
   p(Input, Index, 'word',
      fun(I, D) ->
         (p_seq([fun 'alpha'/2, p_zero_or_more(fun 'alnum'/2)]))(I, D)
      end,
      fun(Node, _Idx) ->
         %% word 匹配结果已是 [Alpha | AlnumList]，Transform 原样透传
         Node
      end).

%% @doc $errcode 块内的描述文本：逐字读取，遇换行/逗号/右括号停止（不含分隔符本身）。
-spec 'errcode_str'(input(), index()) -> parse_result().
'errcode_str'(Input, Index) ->
   p(Input, Index, 'errcode_str',
      fun(I, D) ->
         (p_seq([p_zero_or_more(p_seq([p_not(fun 'newline'/2), p_not(p_string(<<",">>)), p_not(p_string(<<"]">>)), p_charclass(<<".">>)]))]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

%% @doc 名称解析别名：消息名、字段名、错误码名均复用 word/2 规则。
-spec 'name'(input(), index()) -> parse_result().
'name'(Input, Index) ->
   p(Input, Index, 'name',
      fun(I, D) ->
         (fun 'word'/2)(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

%% @doc 类型名分发：按 map → list → word 顺序尝试（map/list 有明确前缀，不会与 record 名冲突）。
%% 返回归一化后的 binary，如 `<<"list[int32]">>`、`<<"map<string,int32>">>`。
-spec 'typename'(input(), index()) -> parse_result().
'typename'(Input, Index) ->
   p(Input, Index, 'typename',
      fun(I, D) ->
         (p_choose([fun 'map_typename'/2, fun 'list_typename'/2, fun 'word'/2]))(I, D)
      end,
      fun(Node, _Idx) ->
         iolist_to_binary(Node)
      end).

%% @doc 列表类型：`list[ inner ]`，支持嵌套如 `list[list[int32]]`。
-spec 'list_typename'(input(), index()) -> parse_result().
'list_typename'(Input, Index) ->
   p(Input, Index, 'list_typename',
      fun(I, D) ->
         (p_seq([p_string(<<"list[">>), fun 'blank0'/2, p_label('inner', fun 'typename'/2), fun 'blank0'/2, p_string(<<"]">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Inner = iolist_to_binary(proplists:get_value(inner, Node)),
         <<"list[", Inner/binary, "]">>
      end).

%% @doc map 类型：`map<key , val>`，key/val 均可递归为 list/map/record。
-spec 'map_typename'(input(), index()) -> parse_result().
'map_typename'(Input, Index) ->
   p(Input, Index, 'map_typename',
      fun(I, D) ->
         (p_seq([p_string(<<"map<">>), fun 'blank0'/2, p_label('key', fun 'typename'/2), fun 'blank0'/2, p_string(<<",">>), fun 'blank0'/2, p_label('val', fun 'typename'/2), fun 'blank0'/2, p_string(<<">">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Key = iolist_to_binary(proplists:get_value(key, Node)),
         Val = iolist_to_binary(proplists:get_value(val, Node)),
         <<"map<", Key/binary, ",", Val/binary, ">">>
      end).

%% ============================================================================
%% PEG 解析器运行时（Parser Expression Grammar + Packrat Memo）
%% ============================================================================
%% 每个语法规则通过 p/5 包装：ParseFun 做匹配，TransformFun 把匹配树转为 AST。
%% 同一 (Index, RuleName) 的重复进入会走 memo，避免回溯爆炸。

-file("peg_includes.hrl", 1).
-type index() :: {{line, pos_integer()}, {column, pos_integer()}}.  %% 当前解析位置（行/列）
-type input() :: binary().
-type parse_failure() :: {fail, term()}.
-type parse_success() :: {term(), input(), index()}.               %% {结果, 剩余输入, 新位置}
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
-type xform_fun() :: fun((input(), index()) -> term()).

%% @doc 带记忆化的规则入口：先查 memo，未命中则 ParseFun 匹配再 TransformFun 转换结果。
-spec p(input(), index(), atom(), parse_fun(), xform_fun()) -> parse_result().
p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
   case get_memo(StartIndex, Name) of                                     % See if the current reduction is memoized
      {ok, Memo} ->                                                       % If it is, return the stored result
         Memo;
      _ ->                                                                % If not, attempt to parse
         Result =
            case ParseFun(Inp, StartIndex) of
               {fail, _} = Failure ->                               % If it fails, memoize the failure
                  Failure;
               {Match, InpRem, NewIndex} ->                         % If it passes, transform and memoize the result.
                  Transformed = TransformFun(Match, StartIndex),
                  {Transformed, InpRem, NewIndex}
            end,
         memoize(StartIndex, Name, Result),
         Result
   end.

%% @doc 将规则 Name 在 Index 处的解析结果存入进程字典，供 Packrat 记忆化复用。
-spec memoize(index(), atom(), parse_result()) -> any().
memoize(Index, Name, Result) ->
   case erlang:get(Index) of
      undefined ->
         erlang:put(Index, [{Name, Result}]);
      [] ->
         erlang:put(Index, [{Name, Result}]);
      Plist ->
         erlang:put(Index, [{Name, Result} | Plist])
   end.

%% @doc 读取 Packrat memo：若 Index 处已有规则 Name 的缓存结果则返回，否则 not_found。
-spec get_memo(index(), atom()) -> {ok, parse_result()} | {error, not_found}.
get_memo(Index, Name) ->
   case erlang:get(Index) of
      undefined ->
         {error, not_found};
      [] ->
         {error, not_found};
      Plist ->
         case proplists:lookup(Name, Plist) of
            {Name, Result} ->
               {ok, Result};
            _ ->
               {error, not_found}
         end
   end.

-ifdef(p_eof).
%% @doc 仅当输入已耗尽时成功（用于注释行末）。
-spec p_eof() -> parse_fun().
p_eof() ->
   fun(<<>>, Index) ->
      {eof, [], Index};
      (_, Index) ->
         {fail, {expected, eof, Index}} end.
-endif.

-ifdef(p_optional).
%% @doc 可选：匹配失败则返回空列表，不消耗输入。
-spec p_optional(parse_fun()) -> parse_fun().
p_optional(P) ->
   fun(Input, Index) ->
      case P(Input, Index) of
         {fail, _} ->
            {[], Input, Index};
         {_, _, _} = Success ->
            Success
      end
   end.
-endif.

-ifdef(p_not).
%% @doc 否定前瞻：P 匹配失败才成功，P 匹配成功则失败（不消耗输入）。
-spec p_not(parse_fun()) -> parse_fun().
p_not(P) ->
   fun(Input, Index) ->
      case P(Input, Index) of
         {fail, _} ->
            {[], Input, Index};
         {Result, _, _} ->
            {fail, {expected, {no_match, Result}, Index}}
      end
   end.
-endif.

-ifdef(p_assert).
%% @doc 断言：P 必须匹配，但丢弃 P 的结果。
-spec p_assert(parse_fun()) -> parse_fun().
p_assert(P) ->
   fun(Input, Index) ->
      case P(Input, Index) of
         {fail, _} = Failure ->
            Failure;
         _ ->
            {[], Input, Index}
      end
   end.
-endif.

-ifdef(p_seq).
%% @doc 顺序组合：按序匹配 P 中每个子规则，全部成功才返回结果列表。
-spec p_seq([parse_fun()]) -> parse_fun().
p_seq(P) ->
   fun(Input, Index) ->
      p_all(P, Input, Index, [])
   end.

%% @doc p_seq 的内部递归：依次执行 Parsers，成功结果逆序累积后返回。
-spec p_all([parse_fun()], input(), index(), [term()]) -> parse_result().
p_all([], Inp, Index, Accum) ->
   {lists:reverse(Accum), Inp, Index};
p_all([P | Parsers], Inp, Index, Accum) ->
   case P(Inp, Index) of
      {fail, _} = Failure ->
         Failure;
      {Result, InpRem, NewIndex} ->
         p_all(Parsers, InpRem, NewIndex, [Result | Accum])
   end.
-endif.

-ifdef(p_choose).
%% @doc 选择：依次尝试 Parsers，返回第一个成功的结果（PEG 有序选择，非 LL 回溯）。
-spec p_choose([parse_fun()]) -> parse_fun().
p_choose(Parsers) ->
   fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
   end.

%% @doc p_choose 的内部递归：依次尝试 Parsers，返回首个成功；全部失败则返回第一个失败原因。
-spec p_attempt([parse_fun()], input(), index(), none | parse_failure()) -> parse_result().
p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P | Parsers], Input, Index, FirstFailure) ->
   case P(Input, Index) of
      {fail, _} = Failure ->
         case FirstFailure of
            none ->
               p_attempt(Parsers, Input, Index, Failure);
            _ ->
               p_attempt(Parsers, Input, Index, FirstFailure)
         end;
      Result ->
         Result
   end.
-endif.

-ifdef(p_zero_or_more).
%% @doc 零次或多次：贪婪扫描 P 直到匹配失败。
-spec p_zero_or_more(parse_fun()) -> parse_fun().
p_zero_or_more(P) ->
   fun(Input, Index) ->
      p_scan(P, Input, Index, [])
   end.
-endif.

-ifdef(p_one_or_more).
%% @doc 一次或多次：至少匹配一次 P，否则失败。
-spec p_one_or_more(parse_fun()) -> parse_fun().
p_one_or_more(P) ->
   fun(Input, Index) ->
      Result = p_scan(P, Input, Index, []),
      case Result of
         {[_ | _], _, _} ->
            Result;
         _ ->
            {fail, {expected, Failure, _}} = P(Input, Index),
            {fail, {expected, {at_least_one, Failure}, Index}}
      end
   end.
-endif.

-ifdef(p_label).
%% @doc 给子规则结果打标签，Transform 阶段用 proplists:get_value 取出。
-spec p_label(atom(), parse_fun()) -> parse_fun().
p_label(Tag, P) ->
   fun(Input, Index) ->
      case P(Input, Index) of
         {fail, _} = Failure ->
            Failure;
         {Result, InpRem, NewIndex} ->
            {{Tag, Result}, InpRem, NewIndex}
      end
   end.
-endif.

-ifdef(p_scan).
%% @doc 内部扫描循环：连续匹配 P 并累积结果，直到 P 失败。
-spec p_scan(parse_fun(), input(), index(), [term()]) -> {[term()], input(), index()}.
p_scan(_, <<>>, Index, Accum) ->
   {lists:reverse(Accum), <<>>, Index};
p_scan(P, Inp, Index, Accum) ->
   case P(Inp, Index) of
      {fail, _} ->
         {lists:reverse(Accum), Inp, Index};
      {Result, InpRem, NewIndex} ->
         p_scan(P, InpRem, NewIndex, [Result | Accum])
   end.
-endif.

-ifdef(p_string).
%% @doc 字面量匹配：精确匹配二进制字符串 S。
-spec p_string(binary()) -> parse_fun().
p_string(S) ->
   Length = erlang:byte_size(S),
   fun(Input, Index) ->
      try
         <<S:Length/binary, Rest/binary>> = Input,
         {S, Rest, p_advance_index(S, Index)}
      catch
         error:{badmatch, _} ->
            {fail, {expected, {string, S}, Index}}
      end
   end.
-endif.

-ifdef(p_anything).
%% @doc 匹配任意一个 UTF-8 字符。
-spec p_anything() -> parse_fun().
p_anything() ->
   fun(<<>>, Index) ->
      {fail, {expected, any_character, Index}};
      (Input, Index) when is_binary(Input) ->
         <<C/utf8, Rest/binary>> = Input,
         {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
   end.
-endif.

-ifdef(p_charclass).
%% @doc 字符类：用正则 Class 匹配开头一段（如 `[\\s\\t]`、`[a-z]`）。
-spec p_charclass(string() | binary()) -> parse_fun().
p_charclass(Class) ->
   {ok, RE} = re:compile(Class, [unicode, dotall]),
   fun(Inp, Index) ->
      case re:run(Inp, RE, [anchored]) of
         {match, [{0, Length} | _]} ->
            {Head, Tail} = erlang:split_binary(Inp, Length),
            {Head, Tail, p_advance_index(Head, Index)};
         _ ->
            {fail, {expected, {character_class, Class}, Index}}
      end
   end.
-endif.

-ifdef(p_regexp).
%% @doc 正则匹配：用 anchored 正则匹配输入开头（本模块语法规则未直接使用，供 PEG 工具链扩展）。
-spec p_regexp(binary()) -> parse_fun().
p_regexp(Regexp) ->
   {ok, RE} = re:compile(Regexp, [unicode, dotall, anchored]),
   fun(Inp, Index) ->
      case re:run(Inp, RE) of
         {match, [{0, Length} | _]} ->
            {Head, Tail} = erlang:split_binary(Inp, Length),
            {Head, Tail, p_advance_index(Head, Index)};
         _ ->
            {fail, {expected, {regexp, Regexp}, Index}}
      end
   end.
-endif.

-ifdef(line).
%% @doc 从 index() 中提取行号；非 index 元组时返回 undefined。
-spec line(index() | term()) -> pos_integer() | undefined.
line({{line, L}, _}) -> L;
line(_) -> undefined.
-endif.

-ifdef(column).
%% @doc 从 index() 中提取列号；非 index 元组时返回 undefined。
-spec column(index() | term()) -> pos_integer() | undefined.
column({_, {column, C}}) -> C;
column(_) -> undefined.
-endif.

%% @doc 根据已匹配的文本更新行/列位置（`\n` 换行时列归 1）。
-spec p_advance_index(input() | unicode:charlist() | pos_integer(), index()) -> index().
p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput) -> % strings
   lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
   {{line, Line}, {column, Col}} = Index,
   case MatchedInput of
      $\n -> {{line, Line + 1}, {column, 1}};
      _ -> {{line, Line}, {column, Col + 1}}
   end.

%% @doc 将错误码名称转为大写（用于 -define 宏名），支持 list 和 binary。
toUpperStr(ListStr) when is_list(ListStr) ->
   [
      begin
         case C >= $a andalso C =< $z of
            true ->
               C - 32;
            _ ->
               C
         end
      end || C <- ListStr
   ];
toUpperStr(BinStr) when is_binary(BinStr) ->
   <<
      begin
         case C >= $a andalso C =< $z of
            true ->
               <<(C - 32)>>;
            _ ->
               <<C>>
         end
      end || <<C:8>> <= BinStr
   >>.
