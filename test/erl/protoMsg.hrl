-ifndef(__protoMsg_h__).
-define(__protoMsg_h__, true).

-type int8() :: -128..127.
-type int16() :: -32768..32767.
-type int32() :: -2147483648..2147483647.
-type int64() :: -9223372036854775808..9223372036854775807.
-type uint8() :: 0..255.
-type uint16() :: 0..65535.
-type uint32() :: 0..4294967295.
-type uint64() :: 0..18446744073709551615.
-type double() :: float().

-define(ERR1, 1).		%% 辅导费
-define(ERR2, 2).		%% 444 
-define(ERR3, 3).		%% 辅导费
-define(ERR4, 4).		%% dfsf 
-define(ERR5, 5).		%% 其他注释辅导费 err6:dfff
-define(ERR7, 6).		%% def
-define(ERR8, 7).		%% 其他注释辅导费
-define(ERR6, 1001).		%% dfff


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
	flag = false :: boolean()
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
	flag = false :: boolean()
	, v8 = 0 :: int8()
	, uv8 = 0 :: uint8()
	, v16 = 0 :: int16()
	, uv16 = 0 :: uint16()
	, v32 = 0 :: int32()
	, uv32 = 0 :: uint32()
	, v64 = 0 :: int64()
	, uv64 = 0 :: uint64()
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
	, f32 = 0.0 :: float()
	, f64 = 0.0 :: double()
	, string1 = "" :: string()
	, string2 = "" :: string()
	, uval = undefined :: #union{}
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
-record(tbinary, {
	data = <<>> :: binary()
	, chunks = [] :: [binary()]
}).
-record(tmap, {
	attrs = undefined :: undefined | #{string() => int32()}
	, names = undefined :: undefined | #{int32() => string()}
	, phones = undefined :: undefined | #{string() => #phoneNumber{}}
}).
-record(toneof, {
	reason = 0 :: int32()
	, item = undefined :: undefined | {gold, int32()} | {coupon, string()} | {phone, #phoneNumber{}}
	, item2 = undefined :: undefined | {gold, int32()} | {coupon, string()} | {phone, #phoneNumber{}}
	, tip = "" :: string()
}).
-record(person1, {
	name = "" :: string()
	, id = 0 :: int32()
	, email = "" :: string()
	, phone = [] :: [#phoneNumber{}]
}).
-record(comboLeaf, {
	id = 0 :: int32()
	, tag = "" :: string()
}).
-record(comboNode, {
	head = undefined :: #comboLeaf{}
	, leaves = [] :: [#comboLeaf{}]
	, index = undefined :: undefined | #{string() => int32()}
}).
-record(comboWrap, {
	name = "" :: string()
	, node = undefined :: #comboNode{}
	, rev = undefined :: undefined | #{int32() => string()}
}).
-record(tMapMatrix, {
	s_i = undefined :: undefined | #{string() => int32()}
	, i_s = undefined :: undefined | #{int32() => string()}
	, s_s = undefined :: undefined | #{string() => string()}
	, i_i = undefined :: undefined | #{int32() => int32()}
	, i64_i = undefined :: undefined | #{int64() => int32()}
	, u_s = undefined :: undefined | #{uint32() => string()}
	, s_b = undefined :: undefined | #{string() => binary()}
	, i_b = undefined :: undefined | #{int32() => binary()}
	, s_rec = undefined :: undefined | #{string() => #phoneNumber{}}
	, i_leaf = undefined :: undefined | #{int32() => #comboLeaf{}}
}).
-record(tListMatrix, {
	lb = [] :: [boolean()]
	, li8 = [] :: [int8()]
	, lu8 = [] :: [uint8()]
	, li16 = [] :: [int16()]
	, lu16 = [] :: [uint16()]
	, li32 = [] :: [int32()]
	, lu32 = [] :: [uint32()]
	, li64 = [] :: [int64()]
	, lu64 = [] :: [uint64()]
	, lint = [] :: [integer()]
	, lnum = [] :: [number()]
	, lflt = [] :: [float()]
	, ldbl = [] :: [double()]
	, lstr = [] :: [string()]
	, lbin = [] :: [binary()]
	, luni = [] :: [#union{}]
	, lph = [] :: [#phoneNumber{}]
	, lleaf = [] :: [#comboLeaf{}]
	, lnode = [] :: [#comboNode{}]
	, lli32_1 = [] :: [int32()]
	, lli32_2 = [] :: [int32()]
	, lli32_3 = [] :: [int32()]
	, lmap_1 = undefined :: undefined | #{string() => int32()}
	, lmap_2 = undefined :: undefined | #{string() => int32()}
	, lmap_3 = undefined :: undefined | #{string() => int32()}
}).
-record(tOneofMatrix, {
	payload = undefined :: undefined | {flag, boolean()} | {num, int32()} | {text, string()} | {raw, binary()} | {phone, #phoneNumber{}} | {leaf, #comboLeaf{}} | {node, #comboNode{}} | {ids, [int32()]} | {tags, [string()]}
}).
-record(tNestDeep, {
	ms_leaf = undefined :: undefined | #{string() => #comboLeaf{}}
	, wrap = undefined :: #comboWrap{}
	, nested = undefined :: undefined | {as_node, #comboNode{}} | {as_leaf, #comboLeaf{}}
	, lwrap_1 = [] :: [#comboWrap{}]
	, lwrap_2 = [] :: [#comboWrap{}]
	, lwrap_3 = [] :: [#comboWrap{}]
}).
-record(tInterleave, {
	n1 = 0 :: int32()
	, s1 = "" :: string()
	, m1 = undefined :: undefined | #{string() => int32()}
	, l1 = [] :: [int32()]
	, o1 = undefined :: undefined | {a, int32()} | {b, string()}
	, n2 = 0 :: int32()
	, s2 = "" :: string()
	, m2 = undefined :: undefined | #{string() => int32()}
	, l2 = [] :: [int32()]
	, o2 = undefined :: undefined | {a, int32()} | {b, string()}
	, n3 = 0 :: int32()
	, s3 = "" :: string()
	, m3 = undefined :: undefined | #{string() => int32()}
	, l3 = [] :: [int32()]
	, o3 = undefined :: undefined | {a, int32()} | {b, string()}
	, b1 = <<>> :: binary()
	, b2 = <<>> :: binary()
	, b3 = <<>> :: binary()
}).
-record(tMultiOneof, {
	a = undefined :: undefined | {x, int32()} | {y, string()}
	, g1 = 0 :: int32()
	, b = undefined :: undefined | {x, int32()} | {y, string()} | {z, binary()}
	, g2 = 0 :: int32()
	, c = undefined :: undefined | {x, int32()} | {p, #phoneNumber{}}
	, g3 = 0 :: int32()
	, d = undefined :: undefined | {l, #comboLeaf{}} | {ids, [int32()]}
	, g4 = 0 :: int32()
	, e = undefined :: undefined | {s, string()} | {x, int32()}
}).
-record(tEmptyEdge, {
	empty_map = undefined :: undefined | #{string() => int32()}
	, empty_list = [] :: [int32()]
	, empty_strs = [] :: [string()]
	, empty_bin = <<>> :: binary()
	, empty_str = "" :: string()
	, none = undefined :: undefined | {x, int32()} | {y, string()}
	, empty_phone = undefined :: #phoneNumber{}
	, empty_leaf = undefined :: #comboLeaf{}
}).
-record(tScalarRun, {
	a1 = 0 :: int32()
	, a2 = 0 :: int32()
	, a3 = 0 :: int32()
	, b1 = "" :: string()
	, b2 = "" :: string()
	, b3 = "" :: string()
	, c1 = undefined :: undefined | #{string() => int32()}
	, c2 = undefined :: undefined | #{string() => int32()}
	, c3 = undefined :: undefined | #{string() => int32()}
}).
-record(tRepeatAll, {
	b1 = false :: boolean()
	, b2 = false :: boolean()
	, b3 = false :: boolean()
	, i32_1 = 0 :: int32()
	, i32_2 = 0 :: int32()
	, i32_3 = 0 :: int32()
	, gap_a = "" :: string()
	, i32_4 = 0 :: int32()
	, i8_1 = 0 :: int8()
	, i8_2 = 0 :: int8()
	, u8_1 = 0 :: uint8()
	, i8_3 = 0 :: int8()
	, u8_2 = 0 :: uint8()
	, u8_3 = 0 :: uint8()
	, i16_1 = 0 :: int16()
	, i16_2 = 0 :: int16()
	, i16_3 = 0 :: int16()
	, u16_1 = 0 :: uint16()
	, gap_b = "" :: string()
	, u16_2 = 0 :: uint16()
	, u16_3 = 0 :: uint16()
	, i32_5 = 0 :: int32()
	, i32_6 = 0 :: int32()
	, i32_7 = 0 :: int32()
	, u32_1 = 0 :: uint32()
	, u32_2 = 0 :: uint32()
	, u32_3 = 0 :: uint32()
	, i64_1 = 0 :: int64()
	, i64_2 = 0 :: int64()
	, gap_c = "" :: string()
	, i64_3 = 0 :: int64()
	, u64_1 = 0 :: uint64()
	, u64_2 = 0 :: uint64()
	, u64_3 = 0 :: uint64()
	, inte_1 = 0 :: integer()
	, inte_2 = 0 :: integer()
	, inte_3 = 0 :: integer()
	, num_1 = 0 :: number()
	, gap_d = "" :: string()
	, num_2 = 0 :: number()
	, num_3 = 0 :: number()
	, flt_1 = 0.0 :: float()
	, flt_2 = 0.0 :: float()
	, flt_3 = 0.0 :: float()
	, dbl_1 = 0.0 :: double()
	, dbl_2 = 0.0 :: double()
	, dbl_3 = 0.0 :: double()
	, str_1 = "" :: string()
	, str_2 = "" :: string()
	, str_3 = "" :: string()
	, gap_e = 0 :: int32()
	, str_4 = "" :: string()
	, bin_1 = <<>> :: binary()
	, bin_2 = <<>> :: binary()
	, bin_3 = <<>> :: binary()
	, uni_1 = undefined :: #union{}
	, uni_2 = undefined :: #union{}
	, uni_3 = undefined :: #union{}
	, li32_1 = [] :: [int32()]
	, li32_2 = [] :: [int32()]
	, li32_3 = [] :: [int32()]
	, lstr_1 = [] :: [string()]
	, gap_f = 0 :: int32()
	, lstr_2 = [] :: [string()]
	, lstr_3 = [] :: [string()]
	, lbin_1 = [] :: [binary()]
	, lbin_2 = [] :: [binary()]
	, lbin_3 = [] :: [binary()]
	, lbool_1 = [] :: [boolean()]
	, lbool_2 = [] :: [boolean()]
	, lbool_3 = [] :: [boolean()]
	, lint_1 = [] :: [integer()]
	, lint_2 = [] :: [integer()]
	, lint_3 = [] :: [integer()]
	, lnum_1 = [] :: [number()]
	, lnum_2 = [] :: [number()]
	, lnum_3 = [] :: [number()]
	, lph_1 = [] :: [#phoneNumber{}]
	, lph_2 = [] :: [#phoneNumber{}]
	, gap_g = "" :: string()
	, lph_3 = [] :: [#phoneNumber{}]
	, lleaf_1 = [] :: [#comboLeaf{}]
	, lleaf_2 = [] :: [#comboLeaf{}]
	, lleaf_3 = [] :: [#comboLeaf{}]
	, ms_i_1 = undefined :: undefined | #{string() => int32()}
	, ms_i_2 = undefined :: undefined | #{string() => int32()}
	, ms_i_3 = undefined :: undefined | #{string() => int32()}
	, mi_s_1 = undefined :: undefined | #{int32() => string()}
	, gap_h = 0 :: int32()
	, mi_s_2 = undefined :: undefined | #{int32() => string()}
	, mi_s_3 = undefined :: undefined | #{int32() => string()}
	, ms_s_1 = undefined :: undefined | #{string() => string()}
	, ms_s_2 = undefined :: undefined | #{string() => string()}
	, ms_s_3 = undefined :: undefined | #{string() => string()}
	, mi_i_1 = undefined :: undefined | #{int32() => int32()}
	, mi_i_2 = undefined :: undefined | #{int32() => int32()}
	, mi_i_3 = undefined :: undefined | #{int32() => int32()}
	, ms_b_1 = undefined :: undefined | #{string() => binary()}
	, ms_b_2 = undefined :: undefined | #{string() => binary()}
	, ms_b_3 = undefined :: undefined | #{string() => binary()}
	, mi_b_1 = undefined :: undefined | #{int32() => binary()}
	, mi_b_2 = undefined :: undefined | #{int32() => binary()}
	, mi_b_3 = undefined :: undefined | #{int32() => binary()}
	, ms_p_1 = undefined :: undefined | #{string() => #phoneNumber{}}
	, ms_p_2 = undefined :: undefined | #{string() => #phoneNumber{}}
	, ms_p_3 = undefined :: undefined | #{string() => #phoneNumber{}}
	, mi_l_1 = undefined :: undefined | #{int32() => #comboLeaf{}}
	, mi_l_2 = undefined :: undefined | #{int32() => #comboLeaf{}}
	, mi_l_3 = undefined :: undefined | #{int32() => #comboLeaf{}}
	, pick1 = undefined :: undefined | {gold, int32()} | {coupon, string()} | {voucher, binary()}
	, gap_i = 0 :: int32()
	, pick2 = undefined :: undefined | {gold, int32()} | {coupon, string()} | {phone, #phoneNumber{}}
	, gap_j = "" :: string()
	, pick3 = undefined :: undefined | {gold, int32()} | {leaf, #comboLeaf{}} | {ids, [int32()]}
	, ph_1 = undefined :: #phoneNumber{}
	, ph_2 = undefined :: #phoneNumber{}
	, gap_k = "" :: string()
	, ph_3 = undefined :: #phoneNumber{}
	, leaf_1 = undefined :: #comboLeaf{}
	, leaf_2 = undefined :: #comboLeaf{}
	, leaf_3 = undefined :: #comboLeaf{}
	, node_1 = undefined :: #comboNode{}
	, node_2 = undefined :: #comboNode{}
	, node_3 = undefined :: #comboNode{}
	, wrap_1 = undefined :: #comboWrap{}
	, wrap_2 = undefined :: #comboWrap{}
	, wrap_3 = undefined :: #comboWrap{}
}).
-record(benchLeaf, {
	id = 0 :: int32()
	, tag = "" :: string()
	, blob = <<>> :: binary()
}).
-record(tBenchScalar, {
	a1 = 0 :: int32()
	, a2 = 0 :: int32()
	, a3 = 0 :: int32()
	, a4 = 0 :: int32()
	, a5 = 0 :: int32()
	, a6 = 0 :: int32()
	, a7 = 0 :: int32()
	, a8 = 0 :: int32()
	, b1 = 0 :: int64()
	, b2 = 0 :: int64()
	, b3 = 0 :: int64()
	, b4 = 0 :: int64()
	, s1 = "" :: string()
	, s2 = "" :: string()
	, s3 = "" :: string()
	, s4 = "" :: string()
	, p1 = <<>> :: binary()
	, p2 = <<>> :: binary()
	, p3 = <<>> :: binary()
	, p4 = <<>> :: binary()
	, f1 = false :: boolean()
	, f2 = false :: boolean()
	, f3 = 0.0 :: float()
	, f4 = 0.0 :: double()
}).
-record(tBenchList, {
	scores = [] :: [int32()]
	, labels = [] :: [string()]
	, chunks = [] :: [binary()]
	, items = [] :: [#benchLeaf{}]
	, extras = [] :: [#comboLeaf{}]
}).
-record(tBenchMap, {
	attrs = undefined :: undefined | #{string() => int32()}
	, rev = undefined :: undefined | #{int32() => string()}
	, lookup = undefined :: undefined | #{string() => #benchLeaf{}}
	, phones = undefined :: undefined | #{string() => #phoneNumber{}}
	, tags = undefined :: undefined | #{string() => string()}
}).
-record(tBenchOneof, {
	seq = 0 :: int32()
	, reward = undefined :: undefined | {gold, int32()} | {coupon, string()} | {leaf, #benchLeaf{}} | {phone, #phoneNumber{}}
	, meta = undefined :: undefined | {code, int32()} | {note, string()}
	, payload = undefined :: undefined | {raw, binary()} | {text, string()} | {blob, #benchLeaf{}}
	, tip = "" :: string()
}).
-record(tBenchNested, {
	root = undefined :: #benchLeaf{}
	, contact = undefined :: #phoneNumber{}
	, leaf = undefined :: #comboLeaf{}
	, children = [] :: [#benchLeaf{}]
	, contacts = [] :: [#phoneNumber{}]
}).
-record(tBenchHeavy, {
	seq = 0 :: int32()
	, title = "" :: string()
	, root = undefined :: #benchLeaf{}
	, items = [] :: [#benchLeaf{}]
	, scores = [] :: [int32()]
	, labels = [] :: [string()]
	, chunks = [] :: [binary()]
	, attrs = undefined :: undefined | #{string() => int32()}
	, rev = undefined :: undefined | #{int32() => string()}
	, lookup = undefined :: undefined | #{string() => #benchLeaf{}}
	, phones = undefined :: undefined | #{string() => #phoneNumber{}}
	, reward = undefined :: undefined | {gold, int32()} | {coupon, string()} | {leaf, #benchLeaf{}} | {phone, #phoneNumber{}}
	, meta = undefined :: undefined | {code, int32()} | {note, string()}
	, payload = <<>> :: binary()
	, extras = [] :: [#comboLeaf{}]
}).

-endif.