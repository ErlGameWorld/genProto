-module(new_type_test).
-export([run/0]).

run() ->
   RR = {toneof, 1, {gold, 100}, undefined, <<"hi">>},
   B = protoMsg:encodeBin(RR),
   {testHer, Dec} = protoMsg:decode(B),
   io:format("oneof enc=~p~ndec=~p~n", [B, Dec]),
   true = Dec =:= RR,

   M = {tmap, #{<<"a">> => 1}, #{1 => <<"x">>}, #{}},
   MB = protoMsg:encodeBin(M),
   {testHer, MDec} = protoMsg:decode(MB),
   io:format("map dec=~p~n", [MDec]),
   true = MDec =:= M,

   Bin = {tbinary, <<1, 2, 3>>, [<<4, 5>>, <<6>>]},
   BB = protoMsg:encodeBin(Bin),
   {testHer, BDec} = protoMsg:decode(BB),
   io:format("binary dec=~p~n", [BDec]),
   true = BDec =:= Bin,

   Phone = {phoneNumber, {test, <<"138">>}, 2},
   R2 = {toneof, 7, {phone, Phone}, undefined, <<"ok">>},
   B2 = protoMsg:encodeBin(R2),
   {testHer, D2} = protoMsg:decode(B2),
   io:format("oneof record dec=~p~n", [D2]),
   true = D2 =:= R2,

   io:format("all ok~n"),
   ok.
