-module(protoParse).
-compile(nowarn_unused_function).

-export([
   parseParse/1
   , parseFile/1
]).

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

-spec parseFile(file:name()) -> any().
parseFile(Filename) ->
   case file:read_file(Filename) of
      {ok, Bin} ->
         parseParse(Bin);
      Err -> Err
   end.

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

-spec 'all'(input(), index()) -> parse_result().
'all'(Input, Index) ->
   p(Input, Index, 'all',
      fun(I, D) ->
         (p_seq([fun 'blank0'/2, p_zero_or_more(p_seq([fun 'protocol'/2, fun 'blank0'/2]))]))(I, D)
      end,
      fun(Node, _Idx) ->
         [_ | [T]] = Node, DataList = [H || [H | _] <- T], DataList
      end).

-spec 'protocol'(input(), index()) -> parse_result().
'protocol'(Input, Index) ->
   p(Input, Index, 'protocol',
      fun(I, D) ->
         (p_seq([p_label('name', fun 'name'/2), fun 'blank0'/2, p_label('structural', fun 'structural'/2)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Name = binary_to_list(iolist_to_binary(proplists:get_value(name, Node))),
         Structural = proplists:get_value(structural, Node),
         MsgId = erlang:get(pd_messageid),
         erlang:put(pd_messageid, MsgId + 1),
         {Name, MsgId, Structural}
      end).

-spec 'structural'(input(), index()) -> parse_result().
'structural'(Input, Index) ->
   p(Input, Index, 'structural',
      fun(I, D) ->
         (p_seq([p_string(<<"{">>), fun 'blank0'/2, p_zero_or_more(p_seq([fun 'field'/2, fun 'blank0'/2])), p_string(<<"}">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         [_, _, List, _] = Node, [H || [H | _] <- List]
      end).

-spec 'field'(input(), index()) -> parse_result().
'field'(Input, Index) ->
   p(Input, Index, 'field',
      fun(I, D) ->
         (p_seq([p_label('datatype', fun 'typename'/2), fun 'blanks'/2, p_label('name', fun 'name'/2), fun 'blank0'/2, p_string(<<";">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         DataType = binary_to_list(iolist_to_binary(proplists:get_value(datatype, Node))),
         Name = binary_to_list(iolist_to_binary(proplists:get_value(name, Node))),
         {DataType, Name}
      end).

-spec 'eof'(input(), index()) -> parse_result().
'eof'(Input, Index) ->
   p(Input, Index, 'eof',
      fun(I, D) ->
         (p_not(p_string(<<".">>)))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-spec 'newline'(input(), index()) -> parse_result().
'newline'(Input, Index) ->
   p(Input, Index, 'newline',
      fun(I, D) ->
         (p_seq([p_optional(p_charclass(<<"[\r]">>)), p_charclass(<<"[\n]">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-spec 'line_comment'(input(), index()) -> parse_result().
'line_comment'(Input, Index) ->
   p(Input, Index, 'line_comment',
      fun(I, D) ->
         (p_seq([p_choose([p_string(<<"//">>), p_string(<<"%%">>)]), p_zero_or_more(p_seq([p_not(fun 'newline'/2), p_not(p_string(<<"$errcode[">>)), p_anything()])), p_zero_or_more(p_seq([p_string(<<"$errcode[">>), p_zero_or_more(fun 'errcode'/2), p_string(<<"]">>)])), p_zero_or_more(p_seq([p_not(fun 'newline'/2), p_anything()])), p_choose([fun 'newline'/2, fun 'eof'/2])]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-spec 'errcode'(input(), index()) -> parse_result().
'errcode'(Input, Index) ->
   p(Input, Index, 'errcode',
      fun(I, D) ->
         (p_seq([fun 'blank0'/2, p_label('errname', fun 'name'/2), fun 'blank0'/2, p_string(<<":">>), fun 'blank0'/2, p_label('errcode_str', fun 'errcode_str'/2), p_choose([p_charclass(<<",">>), fun 'blank0'/2])]))(I, D)
      end,
      fun(Node, _Idx) ->
         ErrNameList = proplists:get_value('errname', Node),
         ErrCodeStrList = proplists:get_value('errcode_str', Node),
         ErrName = binary_to_list(iolist_to_binary(ErrNameList)),
         Desc = binary_to_list(iolist_to_binary(ErrCodeStrList)),
         ErrList = erlang:get(pd_errlist),
         case ErrNameList =/= [] andalso lists:keyfind(ErrName, 1, ErrList) == false of
            true ->
               ErrCodeId = erlang:get(pd_errcodeid),
               erlang:put(pd_errlist, [{ErrName, ErrCodeId, Desc} | ErrList]),
               erlang:put(pd_errcodeid, ErrCodeId + 1);
            _ ->
               skip
         end,
         Node
      end).

-spec 'blank'(input(), index()) -> parse_result().
'blank'(Input, Index) ->
   p(Input, Index, 'blank',
      fun(I, D) ->
         (p_choose([p_charclass(<<"[\s\t]">>), fun 'newline'/2, fun 'line_comment'/2]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-spec 'blank0'(input(), index()) -> parse_result().
'blank0'(Input, Index) ->
   p(Input, Index, 'blank0',
      fun(I, D) ->
         (p_zero_or_more(fun 'blank'/2))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-spec 'blanks'(input(), index()) -> parse_result().
'blanks'(Input, Index) ->
   p(Input, Index, 'blanks',
      fun(I, D) ->
         (p_one_or_more(fun 'blank'/2))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-spec 'alpha'(input(), index()) -> parse_result().
'alpha'(Input, Index) ->
   p(Input, Index, 'alpha',
      fun(I, D) ->
         (p_choose([p_charclass(<<"[a-z]">>), p_charclass(<<"[A-Z]">>), p_string(<<"_">>), p_string(<<"[">>), p_string(<<"]">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-spec 'alnum'(input(), index()) -> parse_result().
'alnum'(Input, Index) ->
   p(Input, Index, 'alnum',
      fun(I, D) ->
         (p_choose([fun 'alpha'/2, p_charclass(<<"[0-9]">>)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-spec 'word'(input(), index()) -> parse_result().
'word'(Input, Index) ->
   p(Input, Index, 'word',
      fun(I, D) ->
         (p_seq([fun 'alpha'/2, p_zero_or_more(fun 'alnum'/2)]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-spec 'errcode_str'(input(), index()) -> parse_result().
'errcode_str'(Input, Index) ->
   p(Input, Index, 'errcode_str',
      fun(I, D) ->
         (p_seq([p_zero_or_more(p_seq([p_not(fun 'newline'/2), p_not(p_string(<<",">>)), p_not(p_string(<<"]">>)), p_charclass(<<".">>)]))]))(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-spec 'name'(input(), index()) -> parse_result().
'name'(Input, Index) ->
   p(Input, Index, 'name',
      fun(I, D) ->
         (fun 'word'/2)(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-spec 'typename'(input(), index()) -> parse_result().
'typename'(Input, Index) ->
   p(Input, Index, 'typename',
      fun(I, D) ->
         (fun 'word'/2)(I, D)
      end,
      fun(Node, _Idx) ->
         Node
      end).

-file("peg_includes.hrl", 1).
-type index() :: {{line, pos_integer()}, {column, pos_integer()}}.
-type input() :: binary().
-type parse_failure() :: {fail, term()}.
-type parse_success() :: {term(), input(), index()}.
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
-type xform_fun() :: fun((input(), index()) -> term()).

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

-spec memoize(index(), atom(), parse_result()) -> any().
memoize(Index, Name, Result) ->
   case erlang:get(Index) of
      undefined ->
         put(Index, [{Name, Result}]);
      [] ->
         put(Index, [{Name, Result}]);
      Plist ->
         put(Index, [{Name, Result} | Plist])
   end.

-spec get_memo(index(), atom()) -> {ok, term()} | {error, not_found}.
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
-spec p_eof() -> parse_fun().
p_eof() ->
   fun(<<>>, Index) ->
      {eof, [], Index};
      (_, Index) ->
         {fail, {expected, eof, Index}} end.
-endif.

-ifdef(p_optional).
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
-spec p_seq([parse_fun()]) -> parse_fun().
p_seq(P) ->
   fun(Input, Index) ->
      p_all(P, Input, Index, [])
   end.

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
-spec p_choose([parse_fun()]) -> parse_fun().
p_choose(Parsers) ->
   fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
   end.

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
-spec p_zero_or_more(parse_fun()) -> parse_fun().
p_zero_or_more(P) ->
   fun(Input, Index) ->
      p_scan(P, Input, Index, [])
   end.
-endif.

-ifdef(p_one_or_more).
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
-spec p_charclass(string() | binary()) -> parse_fun().
p_charclass(Class) ->
   {ok, RE} = re:compile(Class, [unicode, dotall]),
   fun(Inp, Index) ->
      case re:run(Inp, RE, [anchored]) of
         {match, [{0, Length} | _]} ->
            {Head, Tail} = erlang:split_binary(Inp, Length),
            {Head, Tail, p_advance_index(Head, Index)};
         _ ->
            {fail, {expected, {character_class, binary_to_list(Class)}, Index}}
      end
   end.
-endif.

-ifdef(p_regexp).
-spec p_regexp(binary()) -> parse_fun().
p_regexp(Regexp) ->
   {ok, RE} = re:compile(Regexp, [unicode, dotall, anchored]),
   fun(Inp, Index) ->
      case re:run(Inp, RE) of
         {match, [{0, Length} | _]} ->
            {Head, Tail} = erlang:split_binary(Inp, Length),
            {Head, Tail, p_advance_index(Head, Index)};
         _ ->
            {fail, {expected, {regexp, binary_to_list(Regexp)}, Index}}
      end
   end.
-endif.

-ifdef(line).
-spec line(index() | term()) -> pos_integer() | undefined.
line({{line, L}, _}) -> L;
line(_) -> undefined.
-endif.

-ifdef(column).
-spec column(index() | term()) -> pos_integer() | undefined.
column({_, {column, C}}) -> C;
column(_) -> undefined.
-endif.

-spec p_advance_index(input() | unicode:charlist() | pos_integer(), index()) -> index().
p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput) -> % strings
   lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
   {{line, Line}, {column, Col}} = Index,
   case MatchedInput of
      $\n -> {{line, Line + 1}, {column, 1}};
      _ -> {{line, Line}, {column, Col + 1}}
   end.
