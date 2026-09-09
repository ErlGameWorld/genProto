-module(bench_heavy_gpb).
-export([run/0]).

-include("bench_heavy_pb.hrl").

-define(DEFAULT_ITERS, 50000).
-define(WARMUP, 200).

run() ->
   Iters = iterations(),
   Cases = [
      case_scalar(),
      case_list(),
      case_map(),
      case_oneof(),
      case_nested(),
      case_heavy()
   ],
   Results = [run_case(C, Iters) || C <- Cases],
   Json = json_file(<<"gpb">>, <<"protobuf">>, Iters, Results),
   Out = out_path(),
   ok = ensure_results_dir(),
   ok = file:write_file(Out, Json),
   io:format("bench_heavy_gpb: ~s (~p cases, ~p iters)~n", [Out, length(Results), Iters]),
   lists:foreach(
      fun(R) ->
         io:format("  [~s] ~s body=~pB enc=~s ms (~p ops/s) dec=~s ms (~p ops/s)~n",
            [maps:get(category, R), maps:get(message, R),
             maps:get(body_bytes, R), maps:get(encode_ms, R), maps:get(encode_ops_per_sec, R),
             maps:get(decode_ms, R), maps:get(decode_ops_per_sec, R)])
      end, Results),
   ok.

run_case(#{category := Cat, message := Msg, msgName := MsgName, sample := Sample}, Iters) ->
   Bin0 = bench_heavy_pb:encode_msg(Sample),
   _ = bench_heavy_pb:decode_msg(Bin0, MsgName),
   warmup(Sample, Bin0, MsgName, ?WARMUP),
   {EncUs, Body} = timed_encode(Sample, Iters),
   {DecUs, _} = timed_decode(Body, MsgName, Iters),
   EncMs = EncUs / 1000,
   DecMs = DecUs / 1000,
   #{
      category => Cat,
      message => Msg,
      msgId => 0,
      body_bytes => byte_size(Body),
      encode_ms => fmt(EncMs),
      decode_ms => fmt(DecMs),
      encode_ops_per_sec => ops(Iters, EncMs),
      decode_ops_per_sec => ops(Iters, DecMs)
   }.

warmup(Sample, Bin, MsgName, N) ->
   warmup_loop(Sample, Bin, MsgName, N).

warmup_loop(_, _, _, 0) -> ok;
warmup_loop(Sample, Bin, MsgName, N) ->
   _ = bench_heavy_pb:encode_msg(Sample),
   _ = bench_heavy_pb:decode_msg(Bin, MsgName),
   warmup_loop(Sample, Bin, MsgName, N - 1).

timed_encode(Sample, Iters) ->
   timer:tc(fun() -> encode_loop(Sample, Iters, <<>>) end).

encode_loop(_Sample, 0, Body) -> Body;
encode_loop(Sample, N, _) ->
   encode_loop(Sample, N - 1, bench_heavy_pb:encode_msg(Sample)).

timed_decode(Body, MsgName, Iters) ->
   timer:tc(fun() -> decode_loop(Body, MsgName, Iters) end).

decode_loop(_Body, _MsgName, 0) -> ok;
decode_loop(Body, MsgName, N) ->
   _ = bench_heavy_pb:decode_msg(Body, MsgName),
   decode_loop(Body, MsgName, N - 1).

leaf(I) -> #benchLeaf{id = I, tag = <<"leaf">>, blob = <<1, 2, 3, 4>>}.
phone() -> #phoneNumber{number = #test{aa = <<"bench">>}, type = 7}.

case_scalar() ->
   #{category => <<"scalar">>, message => <<"tBenchScalar">>, msgName => tBenchScalar,
     sample => #tBenchScalar{
        a1 = 1, a2 = -1, a3 = 100, a4 = -100, a5 = 1000, a6 = -1000, a7 = 100000, a8 = -100000,
        b1 = 1, b2 = -1, b3 = 9007199254740991, b4 = -9007199254740991,
        s1 = <<"alpha">>, s2 = <<"beta-beta">>, s3 = <<"gamma-gamma-gamma">>, s4 = <<"utf8-test">>,
        p1 = <<0, 1, 2, 3>>, p2 = binary:copy(<<9>>, 32), p3 = binary:copy(<<7>>, 64), p4 = binary:copy(<<3>>, 128),
        f1 = true, f2 = false, f3 = 1.5, f4 = 2.5
     }}.

case_list() ->
   #{category => <<"list">>, message => <<"tBenchList">>, msgName => tBenchList,
     sample => #tBenchList{
        scores = lists:seq(1, 128),
        labels = [list_to_binary("lbl" ++ integer_to_list(I)) || I <- lists:seq(1, 48)],
        chunks = [<<I, (I + 1), (I + 2), (I + 3)>> || I <- lists:seq(1, 16)],
        items = [leaf(I) || I <- lists:seq(1, 48)],
        extras = [#comboLeaf{id = I, tag = <<"x">>} || I <- lists:seq(1, 32)]
     }}.

case_map() ->
   #{category => <<"map">>, message => <<"tBenchMap">>, msgName => tBenchMap,
     sample => #tBenchMap{
        attrs = [{list_to_binary("k" ++ integer_to_list(I)), I} || I <- lists:seq(1, 32)],
        rev = [{I, list_to_binary("v" ++ integer_to_list(I))} || I <- lists:seq(1, 32)],
        lookup = [{list_to_binary("L" ++ integer_to_list(I)), leaf(I)} || I <- lists:seq(1, 16)],
        phones = [{list_to_binary("p" ++ integer_to_list(I)), phone()} || I <- lists:seq(1, 8)],
        tags = [{list_to_binary("t" ++ integer_to_list(I)),
                 list_to_binary("tag-" ++ integer_to_list(I))} || I <- lists:seq(1, 24)]
     }}.

case_oneof() ->
   #{category => <<"oneof">>, message => <<"tBenchOneof">>, msgName => tBenchOneof,
     sample => #tBenchOneof{
        seq = 7,
        reward = {gold, 999},
        meta = {note, <<"meta-note">>},
        payload = {raw, binary:copy(<<42>>, 64)},
        tip = <<"tip">>
     }}.

case_nested() ->
   #{category => <<"nested">>, message => <<"tBenchNested">>, msgName => tBenchNested,
     sample => #tBenchNested{
        root = leaf(0),
        contact = phone(),
        leaf = #comboLeaf{id = 1, tag = <<"root">>},
        children = [leaf(I) || I <- lists:seq(1, 24)],
        contacts = [phone() || _ <- lists:seq(1, 12)]
     }}.

case_heavy() ->
   #{category => <<"heavy">>, message => <<"tBenchHeavy">>, msgName => tBenchHeavy,
     sample => #tBenchHeavy{
        seq = 42,
        title = <<"bench heavy">>,
        root = leaf(0),
        items = [leaf(I) || I <- lists:seq(1, 32)],
        scores = lists:seq(1, 64),
        labels = [list_to_binary("lbl" ++ integer_to_list(I)) || I <- lists:seq(1, 24)],
        chunks = [<<I, (I + 1), (I + 2)>> || I <- lists:seq(1, 8)],
        attrs = [{list_to_binary("k" ++ integer_to_list(I)), I} || I <- lists:seq(1, 16)],
        rev = [{I, list_to_binary("v" ++ integer_to_list(I))} || I <- lists:seq(1, 16)],
        lookup = [{list_to_binary("L" ++ integer_to_list(I)), leaf(I)} || I <- lists:seq(1, 8)],
        phones = [{list_to_binary("p" ++ integer_to_list(I)), phone()} || I <- lists:seq(1, 4)],
        reward = {gold, 999},
        meta = {note, <<"meta">>},
        payload = binary:copy(<<0>>, 256),
        extras = [#comboLeaf{id = I, tag = <<"x">>} || I <- lists:seq(1, 16)]
     }}.

iterations() ->
   case os:getenv("BENCH_ITERS") of
      false -> ?DEFAULT_ITERS;
      Val -> list_to_integer(Val)
   end.

ops(_Iters, Ms) when Ms =< 0.0 -> 0;
ops(Iters, Ms) -> round(Iters / (Ms / 1000)).

fmt(N) when is_integer(N) -> integer_to_binary(N);
fmt(N) -> list_to_binary(lists:flatten(io_lib:format("~.2f", [N]))).

json_file(Lang, Wire, Iters, Results) ->
   CaseBins = join_bin([json_case(R) || R <- Results], <<",">>),
   iolist_to_binary([
      <<"{\n">>,
      <<"  \"language\": \"">>, Lang, <<"\",\n">>,
      <<"  \"wire_format\": \"">>, Wire, <<"\",\n">>,
      <<"  \"iterations\": ">>, integer_to_binary(Iters), <<",\n">>,
      <<"  \"cases\": [\n">>, CaseBins, <<"\n  ]\n">>,
      <<"}\n">>
   ]).

json_case(R) ->
   iolist_to_binary([
      <<"    {">>,
      kv(<<"category">>, maps:get(category, R)), <<",">>,
      kv(<<"message">>, maps:get(message, R)), <<",">>,
      kv_num(<<"msgId">>, maps:get(msgId, R)), <<",">>,
      kv_num(<<"body_bytes">>, maps:get(body_bytes, R)), <<",">>,
      kv_raw(<<"encode_ms">>, maps:get(encode_ms, R)), <<",">>,
      kv_raw(<<"decode_ms">>, maps:get(decode_ms, R)), <<",">>,
      kv_num(<<"encode_ops_per_sec">>, maps:get(encode_ops_per_sec, R)), <<",">>,
      kv_num(<<"decode_ops_per_sec">>, maps:get(decode_ops_per_sec, R)),
      <<"}">>
   ]).

join_bin([], _) -> <<>>;
join_bin([H | T], Sep) ->
   lists:foldl(fun(X, Acc) -> <<Acc/binary, Sep/binary, "\n", X/binary>> end, H, T).

kv(Key, Val) -> [<<"\"">>, Key, <<"\": \"">>, Val, <<"\"">>].
kv_num(Key, Val) -> [<<"\"">>, Key, <<"\": ">>, integer_to_binary(Val)].
kv_raw(Key, Val) when is_binary(Val) -> [<<"\"">>, Key, <<"\": ">>, Val].

out_path() -> filename:join([bench_dir(), "results", "gpb.json"]).
ensure_results_dir() ->
   ok = filelib:ensure_dir(filename:join([bench_dir(), "results", "dummy"])), ok.
bench_dir() -> filename:join([test_root(), "bench"]).
test_root() -> filename:dirname(filename:dirname(code:which(?MODULE))).
