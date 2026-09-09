-module(codec_test).
-export([run/0, export_vectors/0, verify_cross/0]).

-include("protoMsg.hrl").

run() ->
   ok = round_trip_all(),
   ok = export_vectors(),
   ok = verify_cross(),
   io:format("codec_test: all ok~n"),
   ok.

round_trip_all() ->
   lists:foreach(
      fun({Name, _MsgId, Term}) ->
         Bin = iolist_to_binary(protoMsg:encodeBin(Term)),
         {testHer, Dec} = protoMsg:decode(Bin),
         true = Dec =:= Term,
         io:format("  [erl round-trip] ~s ok~n", [Name])
      end,
      erl_cases()
   ),
   ok.

erl_cases() ->
   [
      {"phoneNumber_simple", 2,
         {phoneNumber, {test, <<"hello">>}, 100}},
      {"tbinary_basic", 37,
         {tbinary, <<1, 2, 3>>, [<<4, 5>>, <<6>>]}},
      {"tmap_full", 38,
         {tmap, #{<<"a">> => 1}, #{1 => <<"x">>}, #{}}},
      {"toneof_gold_nil", 39,
         {toneof, 1, {gold, 100}, undefined, <<"hi">>}},
      {"toneof_all_nil", 39,
         {toneof, 0, undefined, undefined, <<>>}}
   ].

export_vectors() ->
   Cases = [vector_case(Name, MsgId, Term) || {Name, MsgId, Term} <- erl_cases()],
   Path = vectors_path("codec_vectors.json"),
   ok = file:write_file(Path, json_doc(Cases)),
   io:format("  [export] ~s (~p cases)~n", [Path, length(Cases)]),
   ok.

vector_case(Name, MsgId, Term) ->
   Bin = iolist_to_binary(protoMsg:encodeBin(Term)),
   <<MsgId:16/big, Body/binary>> = Bin,
   #{
      name => list_to_binary(Name),
      msgId => MsgId,
      msg => atom_to_binary(element(1, Term), utf8),
      full_hex => bin_to_hex(Bin),
      body_hex => bin_to_hex(Body),
      checks => build_checks(Name, Term)
   }.

build_checks("phoneNumber_simple", {phoneNumber, {test, AA}, Type}) ->
   #{
      type => <<"phoneNumber">>,
      fields => #{<<"type">> => Type, <<"number.aa">> => AA}
   };
build_checks("tbinary_basic", {tbinary, Data, Chunks}) ->
   #{
      type => <<"tbinary">>,
      fields => #{
         <<"data.size">> => byte_size(Data),
         <<"chunks.count">> => length(Chunks)
      }
   };
build_checks("tmap_full", {tmap, A, N, P}) ->
   #{
      type => <<"tmap">>,
      fields => #{
         <<"attrs.a">> => maps:get(<<"a">>, A, undefined),
         <<"names.1">> => maps:get(1, N, undefined),
         <<"phones.empty">> => maps:size(P) =:= 0
      }
   };
build_checks("toneof_gold_nil", {toneof, Reason, Item, Item2, Tip}) ->
   #{
      type => <<"toneof">>,
      fields => #{
         <<"reason">> => Reason,
         <<"item.tag">> => oneof_tag(Item),
         <<"item2.null">> => Item2 =:= undefined,
         <<"tip">> => Tip
      }
   };
build_checks("toneof_all_nil", {toneof, Reason, Item, Item2, Tip}) ->
   #{
      type => <<"toneof">>,
      fields => #{
         <<"reason">> => Reason,
         <<"item.null">> => Item =:= undefined,
         <<"item2.null">> => Item2 =:= undefined,
         <<"tip">> => Tip
      }
   };
build_checks(_, _) ->
   #{type => <<>>, fields => #{}}.

oneof_tag(undefined) -> 0;
oneof_tag({gold, _}) -> 1;
oneof_tag({coupon, _}) -> 2;
oneof_tag({phone, _}) -> 3.

verify_cross() ->
   Files = filelib:wildcard(filename:join([vectors_dir(), "cross_*.json"])),
   lists:foreach(fun verify_cross_file/1, Files),
   case Files of
      [] ->
         io:format("  [cross erl] no cross_*.json yet (optional)~n");
      _ ->
         io:format("  [cross erl] verified ~p file(s)~n", [length(Files)])
   end,
   ok.

verify_cross_file(Path) ->
   {ok, Text} = file:read_file(Path),
   Name = grab_field_string(Text, <<"name">>),
   Hex = grab_field_string(Text, <<"body_hex">>),
   MsgId = grab_field_int(Text, <<"msgId">>),
   Body = hex_to_bin(Hex),
   {testHer, _Dec} = protoMsg:decodeBin(MsgId, Body),
   io:format("  [cross erl decode] ~s ok~n", [Name]),
   ok.

grab_field_string(Text, Field) ->
   Pat = io_lib:format("\"~ts\": \"([^\"]*)\"", [Field]),
   {match, [Val]} = re:run(Text, Pat, [{capture, all_but_first, binary}]),
   Val.

grab_field_int(Text, Field) ->
   Pat = io_lib:format("\"~ts\": ([0-9]+)", [Field]),
   {match, [Val]} = re:run(Text, Pat, [{capture, all_but_first, binary}]),
   binary_to_integer(Val).

vectors_path(File) ->
   filename:join([vectors_dir(), File]).

vectors_dir() ->
   filename:join([test_root(), "vectors"]).

test_root() ->
   filename:dirname(filename:dirname(code:which(?MODULE))).

%% --- json writer -------------------------------------------------------------

json_doc(Cases) ->
   <<"{", "\n", "  \"version\": 1,", "\n",
     "  \"cases\": ", (json_list(Cases))/binary, "\n",
     "}", "\n">> .

json_list([]) -> <<"[]">>;
json_list(Cases) ->
   Encoded = [json_obj(C) || C <- Cases],
   <<"[", (bin_join(Encoded, <<", ">>))/binary, "]">>.

json_obj(Map) ->
   KVs = maps:to_list(Map),
   Fields = [<<(json_key(K))/binary, ": ", (json_val(V))/binary>> || {K, V} <- KVs],
   <<"{", (bin_join(Fields, <<", ">>))/binary, "}">>.

json_key(K) when is_atom(K) -> json_string(atom_to_binary(K, utf8));
json_key(K) when is_binary(K) -> json_string(K).

json_val(B) when is_binary(B) -> json_string(B);
json_val(I) when is_integer(I) -> integer_to_binary(I);
json_val(true) -> <<"true">>;
json_val(false) -> <<"false">>;
json_val(M) when is_map(M) -> json_obj(M);
json_val(L) when is_list(L) -> json_list(L).

json_string(B) ->
   << $" , (json_escape(B))/binary, $">> .

json_escape(B) ->
   << <<(json_esc(C))/binary>> || <<C>> <= B >>.

json_esc($") -> <<$\\, $">>;
json_esc($\\) -> <<"\\\\">>;
json_esc(C) when C >= 32, C =< 126 -> <<C>>;
json_esc(C) -> unicode:characters_to_binary([C]).

bin_join([H | T], Sep) ->
   bin_join(T, Sep, H).

bin_join([], _, Acc) -> Acc;
bin_join([H | T], Sep, Acc) ->
   bin_join(T, Sep, <<Acc/binary, Sep/binary, H/binary>>).

%% --- hex ---------------------------------------------------------------------

bin_to_hex(Bin) ->
   list_to_binary(
      lists:flatten([[hex_digit(B bsr 4), hex_digit(B band 15)] || <<B>> <= Bin])
   ).

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) when N < 16 -> $a + N - 10.

hex_to_bin(Hex) ->
   hex_to_bin(Hex, <<>>).

hex_to_bin(<<>>, Acc) -> Acc;
hex_to_bin(<<C1, C2, Rest/binary>>, Acc) ->
   hex_to_bin(Rest, <<Acc/binary, (hex_val(C1)*16 + hex_val(C2))>>).

hex_val(C) when C >= $0, C =< $9 -> C - $0;
hex_val(C) when C >= $a, C =< $f -> C - $a + 10;
hex_val(C) when C >= $A, C =< $F -> C - $A + 10.
