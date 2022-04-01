-opaque int8() :: -128..127.
-opaque int16() :: -32768..32767.
-opaque int32() :: -2147483648..2147483647.
-opaque int64() :: -9223372036854775808..9223372036854775807.
-opaque uint8() :: 0..255.
-opaque uint16() :: 0..65536.
-opaque uint32() :: 0..4294967295.
-opaque uint64() :: 0..18446744073709551615.
-opaque double() :: float().

-define(ERR1, 1001).		%% 辅导费
-define(ERR2, 1002).		%% 444 
-define(ERR3, 1003).		%% 辅导费
-define(ERR4, 1004).		%% dfsf 
-define(ERR5, 1005).		%% 其他注释辅导费
-define(ERR6, 1006).		%% dfff
-define(ERR7, 1007).		%% def
-define(ERR8, 1008).		%% 其他注释辅导费 


-record(test, {
	aa = "" :: string()
}).
-record(phoneNumber, {
	number = undefined :: #test{}
	, type = 0 :: int32()
}).
-record(person, {
	name = "" :: string()
	, id = 0 :: int32()
	, email = "" :: string()
	, phone = [] :: [#phoneNumber{}]
}).
-record(addressBook, {
	person = [] :: [#person{}]
	, other = [] :: [#person{}]
}).
-record(union, {
	test = "" :: string()
	, type = 0 :: int32()
}).
-record(tbool, {
	bool = false :: boolean()
}).
-record(tint8, {
	int1 = 0 :: int8()
	, int2 = 0 :: int8()
}).
-record(tuint8, {
	int1 = 0 :: uint8()
	, int2 = 0 :: uint8()
}).
-record(tint16, {
	int1 = 0 :: int16()
	, int2 = 0 :: int16()
}).
-record(tuint16, {
	int1 = 0 :: uint16()
	, int2 = 0 :: uint16()
}).
-record(tint32, {
	int1 = 0 :: int32()
	, int2 = 0 :: int32()
	, int3 = 0 :: int32()
	, int4 = 0 :: int32()
	, int5 = 0 :: int32()
	, int6 = 0 :: int32()
	, int7 = 0 :: int32()
	, int8 = 0 :: int32()
	, int9 = 0 :: int32()
	, int10 = 0 :: int32()
}).
-record(tuint32, {
	int1 = 0 :: uint32()
	, int2 = 0 :: uint32()
}).
-record(tint64, {
	int1 = 0 :: int64()
	, int2 = 0 :: int64()
}).
-record(tuint64, {
	int1 = 0 :: uint64()
	, int2 = 0 :: uint64()
}).
-record(tinteger, {
	int1 = 0 :: integer()
	, int2 = 0 :: integer()
	, int3 = 0 :: integer()
	, int4 = 0 :: integer()
	, int5 = 0 :: integer()
	, int6 = 0 :: integer()
	, int7 = 0 :: integer()
	, int8 = 0 :: integer()
}).
-record(tnumber, {
	int1 = 0 :: number()
	, int2 = 0 :: number()
	, int3 = 0 :: number()
	, int4 = 0 :: number()
	, int5 = 0 :: number()
	, int6 = 0 :: number()
	, int7 = 0 :: number()
	, int8 = 0 :: number()
	, float1 = 0 :: number()
	, float2 = 0 :: number()
}).
-record(tfloat, {
	int1 = 0.0 :: float()
	, int2 = 0.0 :: float()
}).
-record(tdouble, {
	int1 = 0.0 :: double()
	, int2 = 0.0 :: double()
}).
-record(tstring, {
	int1 = "" :: string()
	, int2 = "" :: string()
}).
-record(tlistbool, {
	int1 = [] :: [boolean()]
}).
-record(tlistint8, {
	int1 = [] :: [int8()]
}).
-record(tlistuint8, {
	int1 = [] :: [uint8()]
}).
-record(tlistint16, {
	int1 = [] :: [int16()]
}).
-record(tlistuint16, {
	int1 = [] :: [uint16()]
}).
-record(tlistint32, {
	int1 = [] :: [int32()]
}).
-record(tlistuint32, {
	int1 = [] :: [uint32()]
}).
-record(tlistint64, {
	int1 = [] :: [int64()]
}).
-record(tlistuint64, {
	int1 = [] :: [uint64()]
}).
-record(tlistinteger, {
	int1 = [] :: [integer()]
}).
-record(tlistnumber, {
	int1 = [] :: [number()]
}).
-record(tlistfloat, {
	int1 = [] :: [float()]
}).
-record(tlistdouble, {
	int1 = [] :: [double()]
}).
-record(tliststring, {
	int1 = [] :: [string()]
}).
-record(tlistunion, {
	int1 = [] :: [#union{}]
}).
-record(allType, {
	bool = false :: boolean()
	, int8 = 0 :: int8()
	, uint8 = 0 :: uint8()
	, int16 = 0 :: int16()
	, uint16 = 0 :: uint16()
	, int32 = 0 :: int32()
	, uint32 = 0 :: uint32()
	, int64 = 0 :: int64()
	, uint64 = 0 :: uint64()
	, inte8 = 0 :: integer()
	, uinte8 = 0 :: integer()
	, inte16 = 0 :: integer()
	, uinte16 = 0 :: integer()
	, inte32 = 0 :: integer()
	, uinte32 = 0 :: integer()
	, inte64 = 0 :: integer()
	, uinte64 = 0 :: integer()
	, num8 = 0 :: number()
	, unum8 = 0 :: number()
	, num16 = 0 :: number()
	, unum16 = 0 :: number()
	, num32 = 0 :: number()
	, unum32 = 0 :: number()
	, num64 = 0 :: number()
	, unum64 = 0 :: number()
	, numfloat = 0 :: number()
	, numdouble = 0 :: number()
	, float = 0.0 :: float()
	, double = 0.0 :: double()
	, string1 = "" :: string()
	, string2 = "" :: string()
	, union = undefined :: #union{}
	, lbool = [] :: [boolean()]
	, lint8 = [] :: [int8()]
	, luint8 = [] :: [uint8()]
	, lint16 = [] :: [int16()]
	, luint16 = [] :: [uint16()]
	, lint32 = [] :: [int32()]
	, luint32 = [] :: [uint32()]
	, lint64 = [] :: [int64()]
	, luint64 = [] :: [uint64()]
	, linte8 = [] :: [integer()]
	, linte16 = [] :: [integer()]
	, linte32 = [] :: [integer()]
	, linte64 = [] :: [integer()]
	, lnum8 = [] :: [number()]
	, lnum16 = [] :: [number()]
	, lnum32 = [] :: [number()]
	, lnum64 = [] :: [number()]
	, lnfloat32 = [] :: [number()]
	, lnfloat64 = [] :: [number()]
	, lfloat = [] :: [float()]
	, ldouble = [] :: [double()]
	, lstring = [] :: [string()]
	, lunion = [] :: [#union{}]
}).
-record(testnull, {
	}).
-record(person1, {
	name = "" :: string()
	, id = 0 :: int32()
	, email = "" :: string()
	, phone = [] :: [#phoneNumber{}]
}).
