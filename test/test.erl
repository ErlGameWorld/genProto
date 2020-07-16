-module(test).

-include("protoMsg.hrl").
-compile(export_all).

encode_int32(N) ->
   TT = #tint32{int1 = 1, int2 = -1, int3 = 128, int4 = -128, int5 = 65536,
      int6 = -65536, int7 = 2100000000, int8 = -2100000000, int9 = 678665, int10 = -678665},
   tt1(N, TT).

tt1(0, TT) ->
   ok;
tt1(N, TT) ->
   protoMsg:encodeIol(TT),
   tt1(N - 1, TT).

decode_int32(N) ->
   TT = #tint32{int1 = 1, int2 = -1, int3 = 128, int4 = -128, int5 = 65536,
      int6 = -65536, int7 = 2100000000, int8 = -2100000000, int9 = 678665, int10 = -678665},
   Bin = protoMsg:encodeIol(TT),
   tt2(N, iolist_to_binary(Bin), 0).

tt2(0, Bin, _A) ->
   {protoMsg:decode(Bin), Bin};
tt2(N, Bin, _A) ->
   A = protoMsg:decode(Bin),
   tt2(N - 1, Bin, A).

encode_addressBook(N) ->
   Add = #addressBook{
      person = [
         #person{
            name = "Alice",
            id = 10000,
            phone = [
               #phoneNumber{number = #test{aa = "123456789"}, type = 1},
               #phoneNumber{number = #test{aa = "87654321"}, type = 2}
            ]
         },
         #person{
            name = "Bob",
            id = 20000,
            phone = [
               #phoneNumber{number = #test{aa = "01234567890"}, type = 3}
            ]
         }
      ]
   },
   tt3(N, Add).

tt3(0, Add) ->
   ok;
tt3(N, Add) ->
   protoMsg:encodeIol(Add),
   tt3(N - 1, Add).

getFun(N) ->
   getFun1(N, #addressBook{}, 0).

getFun1(0, Tuple, _A) ->
   element(1, Tuple);
getFun1(N, Tuple, _A) ->
   A = element(1, Tuple),
   getFun1(N - 1, Tuple, A).

decode_addressBook(N) ->
   AddressBook = #addressBook{
      person = [
         #person{
            name = "Alice",
            id = 10000,
            phone = [
               #phoneNumber{number = #test{aa = "123456789"}, type = 1},
               #phoneNumber{number = #test{aa = "87654321"}, type = 2}
            ]
         },
         #person{
            name = "Bob",
            id = 20000,
            phone = [
               #phoneNumber{number = #test{aa = "01234567890"}, type = 3}
            ]
         }
      ]
   },
   Bin = protoMsg:encodeIol(AddressBook),
   tt4(N, iolist_to_binary(Bin)).
tt4(0, Bin) ->
   Bin;
tt4(N, Bin) ->
   protoMsg:decode(Bin),
   tt4(N - 1, Bin).

test1() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tbool{bool = true}))).

test21() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tint8{int1 = 123, int2 = -22}))).

test22() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tuint8{int1 = 123, int2 = 182}))).

test31() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tint16{int1 = 12343, int2 = -3422}))).
test32() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tuint16{int1 = 43244, int2 = 43243}))).

test41() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tint32{int1 = 12343434, int2 = -34434322}))).


test42() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tuint32{int1 = 432444343, int2 = 432443433}))).

test51() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tint64{int1 = 12344343434, int2 = -344343434322}))).
test52() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tuint64{int1 = 4343432444343, int2 = 4324434343433}))).

tt6(N) ->
   Bin = iolist_to_binary(protoMsg:encodeIol(#tinteger{int1 = -1, int2 = 1, int3 = 128, int4 = -128, int5 = -3244232, int6 = 432423432, int7 = -43434343434434, int8 = 432424242434})),
   <<_MsgId:16/big, MsgBin/binary>> = Bin,
   test6(N, MsgBin).

test6(0, Bin) ->
   io:format("IMY******111 ~p~n", [protoMsg:decode(Bin)]);
test6(N, Bin) ->
   protoMsg:decodeBin(15, Bin),
   test6(N - 1, Bin).

tt66(N) ->
   Bin = iolist_to_binary(protoMsg:encodeIol(#tinteger{int1 = -1, int2 = 1, int3 = 128, int4 = -128, int5 = -3244232, int6 = 432423432, int7 = -43434343434434, int8 = 432424242434})),
   test66(N, Bin).

test66(0, Bin) ->
   <<_MsgId:16/big, MsgBin/binary>> = Bin,
   <<IntBits1:8, Int1:IntBits1/big-signed, IntBits2:8, Int2:IntBits2/big-signed, IntBits3:8, Int3:IntBits3/big-signed, IntBits4:8, Int4:IntBits4/big-signed, IntBits5:8, Int5:IntBits5/big-signed, IntBits6:8, Int6:IntBits6/big-signed, IntBits7:8, Int7:IntBits7/big-signed, IntBits8:8, Int8:IntBits8/big-signed, LeftBin/binary>> = MsgBin,
   A = {tinteger, Int1, Int2, Int3, Int4, Int5, Int6, Int7, Int8},
   io:format("IMY******111 ~p~n", [A]);
test66(N, Bin) ->
   <<_MsgId:16/big, MsgBin/binary>> = Bin,
   %% <<IntBits1:8, Int1:IntBits1/big-signed, IntBits2:8, Int2:IntBits2/big-signed, IntBits3:8, Int3:IntBits3/big-signed, IntBits4:8, Int4:IntBits4/big-signed, IntBits5:8, Int5:IntBits5/big-signed, IntBits6:8, Int6:IntBits6/big-signed, IntBits7:8, Int7:IntBits7/big-signed, IntBits8:8, Int8:IntBits8/big-signed, LeftBin/binary>> = MsgBin,
   %% {tinteger, Int1, Int2, Int3, Int4, Int5, Int6, Int7, Int8},
   <<IntBits1:8, V1:IntBits1/big-signed, IntBits2:8, V2:IntBits2/big-signed, IntBits3:8, V3:IntBits3/big-signed, IntBits4:8, V4:IntBits4/big-signed, IntBits5:8, V5:IntBits5/big-signed, IntBits6:8, V6:IntBits6/big-signed, IntBits7:8, V7:IntBits7/big-signed, IntBits8:8, V8:IntBits8/big-signed, LeftBin1/binary>> = MsgBin,
   {tinteger, V1, V2, V3, V4, V5, V6, V7, V8},
   test66(N - 1, Bin).

tt67(N) ->
   Bin = iolist_to_binary(protoMsg:encodeIol(#tinteger{int1 = -1, int2 = 1, int3 = 128, int4 = -128, int5 = -3244232, int6 = 432423432, int7 = -43434343434434, int8 = 432424242434})),
   test67(N, Bin).

test67(0, Bin) ->
   A = protoMsg:decode(Bin),
   io:format("IMY******111 ~p~n", [A]);
test67(N, Bin) ->
   _A = protoMsg:decode(Bin),
   test67(N - 1, Bin).

test7() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tnumber{int1 = -1, int2 = 1, int3 = 128, int4 = -128, int5 = -3244232, int6 = 432423432, int7 = -43434343434434, int8 = 432424242434, float1 = -34234343.343, float2 = 43242342342342.434}))).

test81() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tfloat{int1 = -34234343.343, int2 = 42342342.434}))).
test82() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tdouble{int1 = -342343433333.343, int2 = 423423423333.434}))).

test9() ->
   protoMsg:decode(iolist_to_binary(protoMsg:encodeIol(#tstring{int1 = "dfdf143242", int2 = "发地方撒发送"}))).

allType() ->
   AllType = #allType{
      bool = false
      , int8 = -66
      , uint8 = 66
      , int16 = -666
      , uint16 = 666
      , int32 = -66666
      , uint32 = 66666
      , int64 = -5294967296
      , uint64 = 5294967296
      , inte8 = -88
      , uinte8 = 88
      , inte16 = -8888
      , uinte16 = 8888
      , inte32 = -888888
      , uinte32 = 888888
      , inte64 = -8888888888
      , uinte64 = 8888888888
      , num8 = -99
      , unum8 = 99
      , num16 = -9999
      , unum16 = 9999
      , num32 = -999999
      , unum32 = 999999
      , num64 = -9999999999
      , unum64 = 9999999999
      , numfloat = 999999.99999
      , numdouble = 9999999999.99999
      , float = 123456.789321
      , double = 4300000000.789321
      , string1 = "this is a test!!!!"
      , string2 = "这是一个测试， 等会看结果~！！！"
      , lbool = [true, false, true, false]
      , lint8 = [123, -123, 66, -66, 88, -88]
      , luint8 = [1, 2, 3, 123, 67, 88]
      , lint16 = [-12345, 12345, 6666, -6666]
      , luint16 = [12345, 12345, 6666, 6666]
      , lint32 = [-12345, 12345, 6666, -6666, -4000000000, 66666666]
      , luint32 = [12345, 12345, 6666, 6666, 4000000000, 66666666]
      , lint64 = [-12345, 12345, -6666, 6666, 400000000000, -66666666666666666]
      , luint64 = [12345, 12345, 6666, 6666, 400000000000, 66666666666666666]
      , linte8 = [123, 12, -66, 66, 34]
      , linte16 = [12334, 12, -6666, 6666, 3412]
      , linte32 = [12334, 12, -666666666, 6666, 3412]
      , linte64 = [12334, 12, -666666666666, 6666, 3412]
      , lnum8 = [123, 12.123, -66.456789, 66, 34]
      , lnum16 = [12334, -12, -6666.6666, 6666, 3412]
      , lnum32 = [12334, 12.7777, -666666666.666777, 6666, 3412]
      , lnum64 = [12334, 12, -666666666666.88888, 6666, 3412.9999]
      , lnfloat32 = [-666666.88888, 4434.434, 434.43, 3434]
      , lnfloat64 = [-666666666666.88888, 4434.434, 434.43, 11111111111.34343, 5566]
      , lfloat = [1.1, 2.2, 3.3, 666666.666]
      , ldouble = [111111111.1, 22222222.2, 3.3, 66666622333333.666]
      , lstring = ["fdsafsdfsfs", "电风扇打法胜多负少的", <<"fdsfasdfsfs">>, <<"大丰收大丰收的方式"/utf8>>]
      , lunion = [#union{}, #union{type = 1, test = "aaaaa"}, #union{type = 2, test = "嘿嘿嘿嘿"}]
   },
   List = protoMsg:encodeIol(AllType),
   iolist_to_binary(List),
   %%io:format("~p~n", [List]),
   AllType1 = protoMsg:decode(iolist_to_binary(List)).
   %%AllType1.

tall1(0) ->
   ok;
tall1(N) ->
   AllType = #allType{
      bool = false
      , int8 = -66
      , uint8 = 66
      , int16 = -666
      , uint16 = 666
      , int32 = -66666
      , uint32 = 66666
      , int64 = -5294967296
      , uint64 = 5294967296
      , inte8 = -88
      , uinte8 = 88
      , inte16 = -8888
      , uinte16 = 8888
      , inte32 = -888888
      , uinte32 = 888888
      , inte64 = -8888888888
      , uinte64 = 8888888888
      , num8 = -99
      , unum8 = 99
      , num16 = -9999
      , unum16 = 9999
      , num32 = -999999
      , unum32 = 999999
      , num64 = -9999999999
      , unum64 = 9999999999
      , numfloat = 999999.99999
      , numdouble = 9999999999.99999
      , float = 123456.789321
      , double = 4300000000.789321
      , string1 = "this is a test!!!!"
      , string2 = "这是一个测试， 等会看结果~！！！"
      , lbool = [true, false, true, false]
      , lint8 = [123, -123, 66, -66, 88, -88]
      , luint8 = [1, 2, 3, 123, 67, 88]
      , lint16 = [-12345, 12345, 6666, -6666]
      , luint16 = [12345, 12345, 6666, 6666]
      , lint32 = [-12345, 12345, 6666, -6666, -4000000000, 66666666]
      , luint32 = [12345, 12345, 6666, 6666, 4000000000, 66666666]
      , lint64 = [-12345, 12345, -6666, 6666, 400000000000, -66666666666666666]
      , luint64 = [12345, 12345, 6666, 6666, 400000000000, 66666666666666666]
      , linte8 = [123, 12, -66, 66, 34]
      , linte16 = [12334, 12, -6666, 6666, 3412]
      , linte32 = [12334, 12, -666666666, 6666, 3412]
      , linte64 = [12334, 12, -666666666666, 6666, 3412]
      , lnum8 = [123, 12.123, -66.456789, 66, 34]
      , lnum16 = [12334, -12, -6666.6666, 6666, 3412]
      , lnum32 = [12334, 12.7777, -666666666.666777, 6666, 3412]
      , lnum64 = [12334, 12, -666666666666.88888, 6666, 3412.9999]
      , lnfloat32 = [-666666.88888, 4434.434, 434.43, 3434]
      , lnfloat64 = [-666666666666.88888, 4434.434, 434.43, 11111111111.34343, 5566]
      , lfloat = [1.1, 2.2, 3.3, 666666.666]
      , ldouble = [111111111.1, 22222222.2, 3.3, 66666622333333.666]
      , lstring = ["fdsafsdfsfs", "电风扇打法胜多负少的", <<"fdsfasdfsfs">>, <<"大丰收大丰收的方式"/utf8>>]
      , lunion = [#union{}, #union{type = 1, test = "aaaaa"}, #union{type = 2, test = "嘿嘿嘿嘿"}]
   },
   %protoMsg:encodeIol(AllType),
   term_to_binary(AllType),
   tall1(N - 1).


tall(N) ->
   AllType = #allType{
      bool = false
      , int8 = -66
      , uint8 = 66
      , int16 = -666
      , uint16 = 666
      , int32 = -66666
      , uint32 = 66666
      , int64 = -5294967296
      , uint64 = 5294967296
      , inte8 = -88
      , uinte8 = 88
      , inte16 = -8888
      , uinte16 = 8888
      , inte32 = -888888
      , uinte32 = 888888
      , inte64 = -8888888888
      , uinte64 = 8888888888
      , num8 = -99
      , unum8 = 99
      , num16 = -9999
      , unum16 = 9999
      , num32 = -999999
      , unum32 = 999999
      , num64 = -9999999999
      , unum64 = 9999999999
      , numfloat = 999999.99999
      , numdouble = 9999999999.99999
      , float = 123456.789321
      , double = 4300000000.789321
      , string1 = "this is a test!!!!"
      , string2 = "这是一个测试， 等会看结果~！！！"
      , lbool = [true, false, true, false]
      , lint8 = [123, -123, 66, -66, 88, -88]
      , luint8 = [1, 2, 3, 123, 67, 88]
      , lint16 = [-12345, 12345, 6666, -6666]
      , luint16 = [12345, 12345, 6666, 6666]
      , lint32 = [-12345, 12345, 6666, -6666, -4000000000, 66666666]
      , luint32 = [12345, 12345, 6666, 6666, 4000000000, 66666666]
      , lint64 = [-12345, 12345, -6666, 6666, 400000000000, -66666666666666666]
      , luint64 = [12345, 12345, 6666, 6666, 400000000000, 66666666666666666]
      , linte8 = [123, 12, -66, 66, 34]
      , linte16 = [12334, 12, -6666, 6666, 3412]
      , linte32 = [12334, 12, -666666666, 6666, 3412]
      , linte64 = [12334, 12, -666666666666, 6666, 3412]
      , lnum8 = [123, 12.123, -66.456789, 66, 34]
      , lnum16 = [12334, -12, -6666.6666, 6666, 3412]
      , lnum32 = [12334, 12.7777, -666666666.666777, 6666, 3412]
      , lnum64 = [12334, 12, -666666666666.88888, 6666, 3412.9999]
      , lnfloat32 = [-666666.88888, 4434.434, 434.43, 3434]
      , lnfloat64 = [-666666666666.88888, 4434.434, 434.43, 11111111111.34343, 5566]
      , lfloat = [1.1, 2.2, 3.3, 666666.666]
      , ldouble = [111111111.1, 22222222.2, 3.3, 66666622333333.666]
      , lstring = ["fdsafsdfsfs", "电风扇打法胜多负少的", <<"fdsfasdfsfs">>, <<"大丰收大丰收的方式"/utf8>>]
      , lunion = [#union{}, #union{type = 1, test = "aaaaa"}, #union{type = 2, test = "嘿嘿嘿嘿"}]
   },
   %List = protoMsg:encodeIol(AllType),
   tall(N, term_to_binary(AllType)).

tall(0, Bin) ->
   Bin;
tall(N, Bin) ->
   %protoMsg:decode(Bin),
   binary_to_term(Bin),
   tall(N - 1, Bin).


hh1(0) ->
   ok;
hh1(N) ->
   A = [tt1, tt2, tt3, tt4, {tet, tt1}, {tet, tt2}, {tet, tt3}, yyy],
   test(A),
   hh1(N - 1).

hh2(0) ->
   ok;
hh2(N) ->
   A = [tt1, tt2, tt3, tt4, {tet, tt1}, {tet, tt2}, {tet, tt3}, yyy],
   tet(A),
   hh2(N - 1).

test([]) ->
   ok;
test([A | T]) ->
   case A of
      tt1 ->
         ok;
      tt2 ->
         ok;
      tt3 ->
         ok;
      tt4 ->
         ok;
      {tet, tt1} ->
         ok;
      {tet, tt2} ->
         ok;
      {tet, tt3} ->
         ok;
      _ ->
         ok
   end,
   test(T).


tet([]) ->
   ok;
tet([tt1 | T]) ->
   ok,
   tet(T);
tet([tt2 | T]) ->
   ok,
   tet(T);
tet([tt3 | T]) ->
   ok,
   tet(T);
tet([tt4 | T]) ->
   ok,
   tet(T);
tet([{tet, tt1} | T]) ->
   ok,
   tet(T);
tet([{tet, tt2} | T]) ->
   ok,
   tet(T);
tet([{tet, tt3} | T]) ->
   ok,
   tet(T);
tet([YY | T]) ->
   ok,
   tet(T).

ttt11(N) ->
   Add = #addressBook{
      person = [
         #person{
            name = "Alice",
            id = 10000,
            phone = [
               #phoneNumber{number = #test{aa = "123456789"}, type = 1},
               #phoneNumber{number = #test{aa = "87654321"}, type = 2}
            ]
         },
         #person{
            name = "Bob",
            id = 20000,
            phone = [
               #phoneNumber{number = #test{aa = "01234567890"}, type = 3}
            ]
         }
      ]
   },
   ttt11(N, Add).

ttt11(0, Add) ->
   ok;
ttt11(N, Add) ->
   protoMsg:encodeIol(Add),
   ttt11(N - 1, Add).

%%tt1(Add) ->
   %"others" => [
   %	#{
   %		"name" => "Carol",
   %		"id" => 30000,
   %		"phone" => [
   %			#{ "number" => #{"aa" => "9876543210"} }
   %		]
   %	}
   %]
   %AddressBook = #person{name = "1232134", id = 11111,email = "aaa" ,phone = [#phoneNumber{}] },
   %AddressBook = #phoneNumber{number =#test{aa = "dffsaf"},type = 12 },
   %%protoMsg:encodeIol(Add).
%ok = file:write_file("fff.bin", Bin),
%print_bin(Bin),
%Bin = protoCode:encode(AddressBook),
%ok = file:write_file("fff.bin", Bin),
%print_bin(Bin),
%MsgId = protoMsg:getMsgId(element(1, AddressBook)),
%<<MsgId:16/little, Bin/binary>>.


tt(N) ->
   AddressBook = #addressBook{
      person = [
         #person{
            name = "Alice",
            id = 10000,
            phone = [
               #phoneNumber{number = #test{aa = "123456789"}, type = 1},
               #phoneNumber{number = #test{aa = "87654321"}, type = 2}
            ]
         },
         #person{
            name = "Bob",
            id = 20000,
            phone = [
               #phoneNumber{number = #test{aa = "01234567890"}, type = 3}
            ]
         }
      ]
   },
   %"others" => [
   %	#{
   %		"name" => "Carol",
   %		"id" => 30000,
   %		"phone" => [
   %			#{ "number" => #{"aa" => "9876543210"} }
   %		]
   %	}
   %]
   %AddressBook = #person{name = "1232134", id = 11111,email = "aaa" ,phone = [#phoneNumber{}] },
   %AddressBook = #phoneNumber{number =#test{aa = "dffsaf"},type = 12 },
   Bin = protoMsg:encodeIol(AddressBook),
   %ok = file:write_file("fff.bin", Bin),
   %print_bin(Bin),
   tt(N, iolist_to_binary(Bin)).
tt(0, Bin) ->
   protoMsg:decode(Bin);
tt(N, Bin) ->
   protoMsg:decode(Bin),
   tt(N - 1, Bin).


%{Len, List, RestBin} = protoMsg("AddressBook", Bin),
%io:format("Len:~p, RestBin:~p~n", [Len, RestBin]),
%io:format("List:~p~n", [List]),
%{Map, _, _} = sproto:decode2("AddressBook", Bin),
%Map.


ttt1(0) ->
   AddressBook = #addressBook{
      person = [
         #person{
            name = "Alice",
            id = 10000,
            phone = [
               #phoneNumber{number = #test{aa = "你好啊 嘿嘿"}, type = 1},
               #phoneNumber{number = #test{aa = "87654321"}, type = 2}
            ]
         },
         #person{
            name = "Bob",
            id = 20000,
            phone = [
               #phoneNumber{number = #test{aa = "范德萨地方范德萨发"}, type = 3}
            ]
         }
      ]
   },
   term_to_binary(AddressBook);
ttt1(N) ->
   ttt1(),
   ttt1(N - 1).

ttt1() ->
   AddressBook = #addressBook{
      person = [
         #person{
            name = "Alice",
            id = 10000,
            phone = [
               #phoneNumber{number = #test{aa = "你好啊 嘿嘿"}, type = 1},
               #phoneNumber{number = #test{aa = "87654321"}, type = 2}
            ]
         },
         #person{
            name = "Bob",
            id = 20000,
            phone = [
               #phoneNumber{number = #test{aa = "范德萨地方范德萨发"}, type = 3}
            ]
         }
      ]
   },
   %"others" => [
   %	#{
   %		"name" => "Carol",
   %		"id" => 30000,
   %		"phone" => [
   %			#{ "number" => #{"aa" => "9876543210"} }
   %		]
   %	}
   %]
   %AddressBook = #person{name = "1232134", id = 11111,email = "aaa" ,phone = [#phoneNumber{}] },
   %AddressBook = #phoneNumber{number =#test{aa = "dffsaf"},type = 12 },
   Bin = term_to_binary(AddressBook).
%ok = file:write_file("fff.bin", Bin),
%print_bin(Bin),
%Bin = protoCode:encode(AddressBook),
%ok = file:write_file("fff.bin", Bin),
%print_bin(Bin),
%MsgId = protoMsg:getMsgId(element(1, AddressBook)),
%<<MsgId:16/little, Bin/binary>>.


ttt(N) ->
   AddressBook = #addressBook{
      person = [
         #person{
            name = "Alice",
            id = 10000,
            phone = [
               #phoneNumber{number = #test{aa = "123456789"}, type = 1},
               #phoneNumber{number = #test{aa = "87654321"}, type = 2}
            ]
         },
         #person{
            name = "Bob",
            id = 20000,
            phone = [
               #phoneNumber{number = #test{aa = "01234567890"}, type = 3}
            ]
         }
      ]
   },
   %"others" => [
   %	#{
   %		"name" => "Carol",
   %		"id" => 30000,
   %		"phone" => [
   %			#{ "number" => #{"aa" => "9876543210"} }
   %		]
   %	}
   %]
   %AddressBook = #person{name = "1232134", id = 11111,email = "aaa" ,phone = [#phoneNumber{}] },
   %AddressBook = #phoneNumber{number =#test{aa = "dffsaf"},type = 12 },
   %ok = file:write_file("fff.bin", Bin),
   %print_bin(Bin),
   ttt(N, term_to_binary(AddressBook)).
ttt(0, Bin) ->
   binary_to_term(Bin);
ttt(N, Bin) ->
   binary_to_term(Bin),
   ttt(N - 1, Bin).

print_bin(Bin) ->
   ByteList = lists:reverse(bin_to_hex(Bin, [])),
   Fun = fun(Byte, Acc) ->
      io:format("~2.16.0b ", [Byte]),
      case Acc rem 8 =:= 0 of
         true -> io:format("~n", []);
         false -> ok
      end,
      Acc + 1
         end,
   lists:foldl(Fun, 1, ByteList),
   io:format("bytes:~p~n", [byte_size(Bin)]).

bin_to_hex(<<>>, Acc) ->
   Acc;
bin_to_hex(Bin, Acc) ->
   <<A:8, Bin2/binary>> = Bin,
   bin_to_hex(Bin2, [A | Acc]).

a1(B) ->
   Bin = list_to_binary([X rem 256 || X <- lists:seq(1, 10000)]),
   a1(B, Bin).

a1(0, Bin) ->
   <<ListBin:6400/big-binary, Left/binary>> = Bin,
   [X || <<X:32/big-unsigned>> <= ListBin];
a1(N, Bin) ->
   <<ListBin:6400/big-binary, Left/binary>> = Bin,
   A = [X || <<X:32/big-unsigned>> <= ListBin],
   B = [X || <<X:16/big-unsigned>> <= ListBin],
   io:format("IMY********** ~p~n", [A == B]),
   a1(N - 1, Bin).

a2(B) ->
   Bin = list_to_binary([X rem 256 || X <- lists:seq(1, 10000)]),
   a2(B, Bin).

a2(0, Bin) ->
   Len = 200,
   <<ListBin:Len/big-binary-unit:32, Left/binary>> = Bin,
   [X  || <<X:32/big-unsigned>> <= ListBin];
a2(N, Bin) ->
   Len = 200,
   <<ListBin:Len/big-binary-unit:32, Left/binary>> = Bin,
   [X  || <<X:32/big-unsigned>> <= ListBin],
   a2(N - 1, Bin).