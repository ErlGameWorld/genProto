-module(test).

-include("protoMsg.hrl").
-compile(export_all).

test1() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tbool{bool = true}))).

test21() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tint8{int1 = 123, int2 = -22}))).

test22() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tuint8{int1 = 123, int2 = 182}))).

test31() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tint16{int1 = 12343, int2 = -3422}))).
test32() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tuint16{int1 = 43244, int2 = 43243}))).

test41() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tint32{int1 = 12343434, int2 = -34434322}))).
test42() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tuint32{int1 = 432444343, int2 = 432443433}))).

test51() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tint64{int1 = 12344343434, int2 = -344343434322}))).
test52() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tuint64{int1 = 4343432444343, int2 = 4324434343433}))).

test6() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tinteger{int1 = -1, int2 = 1, int3 = 128, int4 = -128, int5 = -3244232, int6 = 432423432, int7 = -43434343434434, int8 = 432424242434}))).

test7() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tnumber{int1 = -1, int2 = 1, int3 = 128, int4 = -128, int5 = -3244232, int6 = 432423432, int7 = -43434343434434, int8 = 432424242434, float1 = -34234343.343, float2 = 43242342342342.434}))).

test81() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tfloat{int1 = -34234343.343, int2 = 42342342.434}))).
test82() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tdouble{int1 = -342343433333.343, int2 = 423423423333.434}))).

test9() ->
	protoMsg:decode(iolist_to_binary(protoMsg:encode(#tstring{int1 = "dfdf143242", int2 = "发地方撒发送"}))).

allType() ->
	AllType = #allType{
		bool = false
		,int8 = -66
		,uint8 = 66
		,int16 = -666
		,uint16 = 666
		,int32 = -66666
		,uint32 = 66666
		,int64 = -5294967296
		,uint64 = 5294967296
		,inte8 = -88
		,uinte8 = 88
		,inte16 = -8888
		,uinte16 = 8888
		,inte32 = -888888
		,uinte32 = 888888
		,inte64 = -8888888888
		,uinte64 = 8888888888
		,num8 = -99
		,unum8 = 99
		,num16 = -9999
		,unum16 = 9999
		,num32 = -999999
		,unum32 = 999999
		,num64 = -9999999999
		,unum64 = 9999999999
		,numfloat = 999999.99999
		,numdouble = 9999999999.99999
		,float = 123456.789321
		,double = 4300000000.789321
		,string1 = "this is a test!!!!"
		,string2 = "这是一个测试， 等会看结果~！！！"
		,lbool = [true, false, true, false]
		,lint8 = [123, -123, 66, -66, 88, -88]
		,luint8 = [1,2,3,123,67,88]
		,lint16 = [-12345, 12345, 6666, -6666]
		,luint16 = [12345, 12345, 6666, 6666]
		,lint32 = [-12345, 12345, 6666, -6666, -4000000000, 66666666]
		,luint32 = [12345, 12345, 6666, 6666, 4000000000, 66666666]
		,lint64 = [-12345, 12345, -6666, 6666, 400000000000, -66666666666666666]
		,luint64 = [12345, 12345, 6666, 6666, 400000000000, 66666666666666666]
		,linte8 = [123, 12, -66, 66, 34]
		,linte16 = [12334, 12, -6666, 6666, 3412]
		,linte32 = [12334, 12, -666666666, 6666, 3412]
		,linte64 = [12334, 12, -666666666666, 6666, 3412]
		,lnum8 = [123, 12.123, -66.456789, 66, 34]
		,lnum16 = [12334, -12, -6666.6666, 6666, 3412]
		,lnum32 = [12334, 12.7777, -666666666.666777, 6666, 3412]
		,lnum64 = [12334, 12, -666666666666.88888, 6666, 3412.9999]
		,lnfloat32 = [-666666.88888,4434.434, 434.43, 3434]
		,lnfloat64 = [-666666666666.88888,4434.434, 434.43, 11111111111.34343,5566]
		,lfloat = [1.1, 2.2, 3.3, 666666.666]
		,ldouble = [111111111.1, 22222222.2, 3.3, 66666622333333.666]
		,lstring = ["fdsafsdfsfs", "电风扇打法胜多负少的", <<"fdsfasdfsfs">>, <<"大丰收大丰收的方式">>]
		,lunion = [#union{}, #union{type = 1, test = "aaaaa"}, #union{type = 2, test = "嘿嘿嘿嘿"}]
	},
	List = protoMsg:encode(AllType),
	io:format("~p~n",[List]),
	protoMsg:decode(iolist_to_binary(List)).

tall1(0) ->
	ok;
tall1(N) ->
	AllType = #allType{
		bool = false
		,int8 = -66
		,uint8 = 66
		,int16 = -666
		,uint16 = 666
		,int32 = -66666
		,uint32 = 66666
		,int64 = -5294967296
		,uint64 = 5294967296
		,inte8 = -88
		,uinte8 = 88
		,inte16 = -8888
		,uinte16 = 8888
		,inte32 = -888888
		,uinte32 = 888888
		,inte64 = -8888888888
		,uinte64 = 8888888888
		,num8 = -99
		,unum8 = 99
		,num16 = -9999
		,unum16 = 9999
		,num32 = -999999
		,unum32 = 999999
		,num64 = -9999999999
		,unum64 = 9999999999
		,numfloat = 999999.99999
		,numdouble = 9999999999.99999
		,float = 123456.789321
		,double = 4300000000.789321
		,string1 = "this is a test!!!!"
		,string2 = "这是一个测试， 等会看结果~！！！"
		,lbool = [true, false, true, false]
		,lint8 = [123, -123, 66, -66, 88, -88]
		,luint8 = [1,2,3,123,67,88]
		,lint16 = [-12345, 12345, 6666, -6666]
		,luint16 = [12345, 12345, 6666, 6666]
		,lint32 = [-12345, 12345, 6666, -6666, -4000000000, 66666666]
		,luint32 = [12345, 12345, 6666, 6666, 4000000000, 66666666]
		,lint64 = [-12345, 12345, -6666, 6666, 400000000000, -66666666666666666]
		,luint64 = [12345, 12345, 6666, 6666, 400000000000, 66666666666666666]
		,linte8 = [123, 12, -66, 66, 34]
		,linte16 = [12334, 12, -6666, 6666, 3412]
		,linte32 = [12334, 12, -666666666, 6666, 3412]
		,linte64 = [12334, 12, -666666666666, 6666, 3412]
		,lnum8 = [123, 12.123, -66.456789, 66, 34]
		,lnum16 = [12334, -12, -6666.6666, 6666, 3412]
		,lnum32 = [12334, 12.7777, -666666666.666777, 6666, 3412]
		,lnum64 = [12334, 12, -666666666666.88888, 6666, 3412.9999]
		,lnfloat32 = [-666666.88888,4434.434, 434.43, 3434]
		,lnfloat64 = [-666666666666.88888,4434.434, 434.43, 11111111111.34343,5566]
		,lfloat = [1.1, 2.2, 3.3, 666666.666]
		,ldouble = [111111111.1, 22222222.2, 3.3, 66666622333333.666]
		,lstring = ["fdsafsdfsfs", "电风扇打法胜多负少的", <<"fdsfasdfsfs">>, <<"大丰收大丰收的方式">>]
		,lunion = [#union{}, #union{type = 1, test = "aaaaa"}, #union{type = 2, test = "嘿嘿嘿嘿"}]
	},
	%protoMsg:encode(AllType),
	term_to_binary(AllType),
	tall1(N - 1).


tall(N) ->
	AllType = #allType{
		bool = false
		,int8 = -66
		,uint8 = 66
		,int16 = -666
		,uint16 = 666
		,int32 = -66666
		,uint32 = 66666
		,int64 = -5294967296
		,uint64 = 5294967296
		,inte8 = -88
		,uinte8 = 88
		,inte16 = -8888
		,uinte16 = 8888
		,inte32 = -888888
		,uinte32 = 888888
		,inte64 = -8888888888
		,uinte64 = 8888888888
		,num8 = -99
		,unum8 = 99
		,num16 = -9999
		,unum16 = 9999
		,num32 = -999999
		,unum32 = 999999
		,num64 = -9999999999
		,unum64 = 9999999999
		,numfloat = 999999.99999
		,numdouble = 9999999999.99999
		,float = 123456.789321
		,double = 4300000000.789321
		,string1 = "this is a test!!!!"
		,string2 = "这是一个测试， 等会看结果~！！！"
		,lbool = [true, false, true, false]
		,lint8 = [123, -123, 66, -66, 88, -88]
		,luint8 = [1,2,3,123,67,88]
		,lint16 = [-12345, 12345, 6666, -6666]
		,luint16 = [12345, 12345, 6666, 6666]
		,lint32 = [-12345, 12345, 6666, -6666, -4000000000, 66666666]
		,luint32 = [12345, 12345, 6666, 6666, 4000000000, 66666666]
		,lint64 = [-12345, 12345, -6666, 6666, 400000000000, -66666666666666666]
		,luint64 = [12345, 12345, 6666, 6666, 400000000000, 66666666666666666]
		,linte8 = [123, 12, -66, 66, 34]
		,linte16 = [12334, 12, -6666, 6666, 3412]
		,linte32 = [12334, 12, -666666666, 6666, 3412]
		,linte64 = [12334, 12, -666666666666, 6666, 3412]
		,lnum8 = [123, 12.123, -66.456789, 66, 34]
		,lnum16 = [12334, -12, -6666.6666, 6666, 3412]
		,lnum32 = [12334, 12.7777, -666666666.666777, 6666, 3412]
		,lnum64 = [12334, 12, -666666666666.88888, 6666, 3412.9999]
		,lnfloat32 = [-666666.88888,4434.434, 434.43, 3434]
		,lnfloat64 = [-666666666666.88888,4434.434, 434.43, 11111111111.34343,5566]
		,lfloat = [1.1, 2.2, 3.3, 666666.666]
		,ldouble = [111111111.1, 22222222.2, 3.3, 66666622333333.666]
		,lstring = ["fdsafsdfsfs", "电风扇打法胜多负少的", <<"fdsfasdfsfs">>, <<"大丰收大丰收的方式">>]
		,lunion = [#union{}, #union{type = 1, test = "aaaaa"}, #union{type = 2, test = "嘿嘿嘿嘿"}]
	},
	%List = protoMsg:encode(AllType),
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
	A = [tt1,tt2,tt3,tt4,{tet,tt1},{tet, tt2},{tet,tt3}, yyy],
	test(A),
	hh1(N -1).

hh2(0) ->
	ok;
hh2(N) ->
	A = [tt1,tt2,tt3,tt4,{tet,tt1},{tet, tt2},{tet,tt3}, yyy],
	tet(A),
	hh2(N -1).

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

tt1(0)->
	AddressBook = #addressBook{
		person = [
			#person{
				name = "Alice",
				id = 10000,
				phone = [
					#phoneNumber{ number = #test{aa = "123456789"} , type = 1 },
					#phoneNumber{ number = #test{aa = "87654321"} , type = 2 }
				]
			},
			#person{
				name = "Bob",
				id = 20000,
				phone = [
					#phoneNumber{number = #test{aa = "01234567890"} , type = 3 }
				]
			}
		]
	},
	Bin = protoMsg:encode(AddressBook);
tt1(N) ->
    tt1(),
    tt1(N- 1).

tt1() ->
    AddressBook = #addressBook{
        person = [
            #person{
                name = "Alice",
                id = 10000,
                phone = [
                    #phoneNumber{ number = #test{aa = "123456789"} , type = 1 },
                    #phoneNumber{ number = #test{aa = "87654321"} , type = 2 }
                ]
            },
            #person{
                name = "Bob",
                id = 20000,
                phone = [
                    #phoneNumber{number = #test{aa = "01234567890"} , type = 3 }
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
    Bin = protoMsg:encode(AddressBook).
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
					#phoneNumber{ number = #test{aa = "123456789"} , type = 1 },
					#phoneNumber{ number = #test{aa = "87654321"} , type = 2 }
				]
			},
			#person{
				name = "Bob",
				id = 20000,
				phone = [
					#phoneNumber{number = #test{aa = "01234567890"} , type = 3 }
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
	Bin = protoMsg:encode(AddressBook),
	%ok = file:write_file("fff.bin", Bin),
	%print_bin(Bin),
    tt(N, iolist_to_binary(Bin)).
tt(0, Bin) ->
	protoMsg:decode(Bin);
tt(N, Bin) ->
    protoMsg:decode(Bin),
    tt(N-1, Bin).


	%{Len, List, RestBin} = protoMsg("AddressBook", Bin),
	%io:format("Len:~p, RestBin:~p~n", [Len, RestBin]),
	%io:format("List:~p~n", [List]),
	%{Map, _, _} = sproto:decode2("AddressBook", Bin),
	%Map.


ttt1(0)->
	AddressBook = #addressBook{
		person = [
			#person{
				name = "Alice",
				id = 10000,
				phone = [
					#phoneNumber{ number = #test{aa = "你好啊 嘿嘿"} , type = 1 },
					#phoneNumber{ number = #test{aa = "87654321"} , type = 2 }
				]
			},
			#person{
				name = "Bob",
				id = 20000,
				phone = [
					#phoneNumber{number = #test{aa = "范德萨地方范德萨发"} , type = 3 }
				]
			}
		]
	},
	term_to_binary(AddressBook);
ttt1(N) ->
	ttt1(),
	ttt1(N- 1).

ttt1() ->
	AddressBook = #addressBook{
		person = [
			#person{
				name = "Alice",
				id = 10000,
				phone = [
					#phoneNumber{ number = #test{aa = "你好啊 嘿嘿"} , type = 1 },
					#phoneNumber{ number = #test{aa = "87654321"} , type = 2 }
				]
			},
			#person{
				name = "Bob",
				id = 20000,
				phone = [
					#phoneNumber{number = #test{aa = "范德萨地方范德萨发"} , type = 3 }
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
					#phoneNumber{ number = #test{aa = "123456789"} , type = 1 },
					#phoneNumber{ number = #test{aa = "87654321"} , type = 2 }
				]
			},
			#person{
				name = "Bob",
				id = 20000,
				phone = [
					#phoneNumber{number = #test{aa = "01234567890"} , type = 3 }
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
	ttt(N-1, Bin).

print_bin(Bin) ->
	ByteList = lists:reverse(bin_to_hex(Bin, [])),
	Fun = fun(Byte, Acc) ->
	 	io:format("~2.16.0b ", [Byte]),
	 	case Acc rem 8  =:= 0 of
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
	bin_to_hex(Bin2, [A|Acc]).