-opaque int8() :: -128..127.
-opaque int16() :: -32768..32767.
-opaque int32() :: -2147483648..2147483647.
-opaque int64() :: -9223372036854775808..9223372036854775807.
-opaque uint8() :: 0..255.
-opaque uint16() :: 0..65536.
-opaque uint32() :: 0..4294967295.
-opaque uint64() :: 0..18446744073709551615.
-opaque single() :: float().
-opaque double() :: float().

-record(test ,{
	aa = "" :: string()
	}).
-record(phoneNumber ,{
	number = undefined  :: #test{}
	,type = 0 :: int32()
	}).
-record(person ,{
	name = "" :: string()
	,id = 0 :: int32()
	,email = "" :: string()
	,phone = []  :: [#phoneNumber{}]
	}).
-record(addressBook ,{
	person = []  :: [#person{}]
	,other = []  :: [#person{}]
	}).
