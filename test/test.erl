-module(test).

-include("protoMsg.hrl").
-compile(export_all).

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