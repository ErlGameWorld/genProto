require("bintalk.types")
require("bintalk.reader")
require("bintalk.writer")
local EnumName = {
	EN1 = 0,
	EN2 = 1,
	EN3 = 2,
}
EnumName = { _enum = EnumName }
BintalkTypes.EnumName = setmetatable(EnumName, BintalkTypes._enum_mt)
local EnumName16 = {
	EN16_a01 = 0,
	EN16_a02 = 1,
	EN16_a03 = 2,
	EN16_a04 = 3,
	EN16_a05 = 4,
	EN16_a06 = 5,
	EN16_a07 = 6,
	EN16_a08 = 7,
	EN16_a11 = 8,
	EN16_a12 = 9,
	EN16_a13 = 10,
	EN16_a14 = 11,
	EN16_a15 = 12,
	EN16_a16 = 13,
	EN16_a17 = 14,
	EN16_a18 = 15,
	EN16_a21 = 16,
	EN16_a22 = 17,
	EN16_a23 = 18,
	EN16_a24 = 19,
	EN16_a25 = 20,
	EN16_a26 = 21,
	EN16_a27 = 22,
	EN16_a28 = 23,
	EN16_a31 = 24,
	EN16_a32 = 25,
	EN16_a33 = 26,
	EN16_a34 = 27,
	EN16_a35 = 28,
	EN16_a36 = 29,
	EN16_a37 = 30,
	EN16_a38 = 31,
	EN16_a41 = 32,
	EN16_a42 = 33,
	EN16_a43 = 34,
	EN16_a44 = 35,
	EN16_a45 = 36,
	EN16_a46 = 37,
	EN16_a47 = 38,
	EN16_a48 = 39,
	EN16_a51 = 40,
	EN16_a52 = 41,
	EN16_a53 = 42,
	EN16_a54 = 43,
	EN16_a55 = 44,
	EN16_a56 = 45,
	EN16_a57 = 46,
	EN16_a58 = 47,
	EN16_a61 = 48,
	EN16_a62 = 49,
	EN16_a63 = 50,
	EN16_a64 = 51,
	EN16_a65 = 52,
	EN16_a66 = 53,
	EN16_a67 = 54,
	EN16_a68 = 55,
	EN16_a71 = 56,
	EN16_a72 = 57,
	EN16_a73 = 58,
	EN16_a74 = 59,
	EN16_a75 = 60,
	EN16_a76 = 61,
	EN16_a77 = 62,
	EN16_a78 = 63,
	EN16_b01 = 64,
	EN16_b02 = 65,
	EN16_b03 = 66,
	EN16_b04 = 67,
	EN16_b05 = 68,
	EN16_b06 = 69,
	EN16_b07 = 70,
	EN16_b08 = 71,
	EN16_b11 = 72,
	EN16_b12 = 73,
	EN16_b13 = 74,
	EN16_b14 = 75,
	EN16_b15 = 76,
	EN16_b16 = 77,
	EN16_b17 = 78,
	EN16_b18 = 79,
	EN16_b21 = 80,
	EN16_b22 = 81,
	EN16_b23 = 82,
	EN16_b24 = 83,
	EN16_b25 = 84,
	EN16_b26 = 85,
	EN16_b27 = 86,
	EN16_b28 = 87,
	EN16_b31 = 88,
	EN16_b32 = 89,
	EN16_b33 = 90,
	EN16_b34 = 91,
	EN16_b35 = 92,
	EN16_b36 = 93,
	EN16_b37 = 94,
	EN16_b38 = 95,
	EN16_b41 = 96,
	EN16_b42 = 97,
	EN16_b43 = 98,
	EN16_b44 = 99,
	EN16_b45 = 100,
	EN16_b46 = 101,
	EN16_b47 = 102,
	EN16_b48 = 103,
	EN16_b51 = 104,
	EN16_b52 = 105,
	EN16_b53 = 106,
	EN16_b54 = 107,
	EN16_b55 = 108,
	EN16_b56 = 109,
	EN16_b57 = 110,
	EN16_b58 = 111,
	EN16_b61 = 112,
	EN16_b62 = 113,
	EN16_b63 = 114,
	EN16_b64 = 115,
	EN16_b65 = 116,
	EN16_b66 = 117,
	EN16_b67 = 118,
	EN16_b68 = 119,
	EN16_b71 = 120,
	EN16_b72 = 121,
	EN16_b73 = 122,
	EN16_b74 = 123,
	EN16_b75 = 124,
	EN16_b76 = 125,
	EN16_b77 = 126,
	EN16_b78 = 127,
	EN16_c01 = 128,
	EN16_c02 = 129,
	EN16_c03 = 130,
	EN16_c04 = 131,
	EN16_c05 = 132,
	EN16_c06 = 133,
	EN16_c07 = 134,
	EN16_c08 = 135,
	EN16_c11 = 136,
	EN16_c12 = 137,
	EN16_c13 = 138,
	EN16_c14 = 139,
	EN16_c15 = 140,
	EN16_c16 = 141,
	EN16_c17 = 142,
	EN16_c18 = 143,
	EN16_c21 = 144,
	EN16_c22 = 145,
	EN16_c23 = 146,
	EN16_c24 = 147,
	EN16_c25 = 148,
	EN16_c26 = 149,
	EN16_c27 = 150,
	EN16_c28 = 151,
	EN16_c31 = 152,
	EN16_c32 = 153,
	EN16_c33 = 154,
	EN16_c34 = 155,
	EN16_c35 = 156,
	EN16_c36 = 157,
	EN16_c37 = 158,
	EN16_c38 = 159,
	EN16_c41 = 160,
	EN16_c42 = 161,
	EN16_c43 = 162,
	EN16_c44 = 163,
	EN16_c45 = 164,
	EN16_c46 = 165,
	EN16_c47 = 166,
	EN16_c48 = 167,
	EN16_c51 = 168,
	EN16_c52 = 169,
	EN16_c53 = 170,
	EN16_c54 = 171,
	EN16_c55 = 172,
	EN16_c56 = 173,
	EN16_c57 = 174,
	EN16_c58 = 175,
	EN16_c61 = 176,
	EN16_c62 = 177,
	EN16_c63 = 178,
	EN16_c64 = 179,
	EN16_c65 = 180,
	EN16_c66 = 181,
	EN16_c67 = 182,
	EN16_c68 = 183,
	EN16_c71 = 184,
	EN16_c72 = 185,
	EN16_c73 = 186,
	EN16_c74 = 187,
	EN16_c75 = 188,
	EN16_c76 = 189,
	EN16_c77 = 190,
	EN16_c78 = 191,
	EN16_d01 = 192,
	EN16_d02 = 193,
	EN16_d03 = 194,
	EN16_d04 = 195,
	EN16_d05 = 196,
	EN16_d06 = 197,
	EN16_d07 = 198,
	EN16_d08 = 199,
	EN16_d11 = 200,
	EN16_d12 = 201,
	EN16_d13 = 202,
	EN16_d14 = 203,
	EN16_d15 = 204,
	EN16_d16 = 205,
	EN16_d17 = 206,
	EN16_d18 = 207,
	EN16_d21 = 208,
	EN16_d22 = 209,
	EN16_d23 = 210,
	EN16_d24 = 211,
	EN16_d25 = 212,
	EN16_d26 = 213,
	EN16_d27 = 214,
	EN16_d28 = 215,
	EN16_d31 = 216,
	EN16_d32 = 217,
	EN16_d33 = 218,
	EN16_d34 = 219,
	EN16_d35 = 220,
	EN16_d36 = 221,
	EN16_d37 = 222,
	EN16_d38 = 223,
	EN16_d41 = 224,
	EN16_d42 = 225,
	EN16_d43 = 226,
	EN16_d44 = 227,
	EN16_d45 = 228,
	EN16_d46 = 229,
	EN16_d47 = 230,
	EN16_d48 = 231,
	EN16_d51 = 232,
	EN16_d52 = 233,
	EN16_d53 = 234,
	EN16_d54 = 235,
	EN16_d55 = 236,
	EN16_d56 = 237,
	EN16_d57 = 238,
	EN16_d58 = 239,
	EN16_d61 = 240,
	EN16_d62 = 241,
	EN16_d63 = 242,
	EN16_d64 = 243,
	EN16_d65 = 244,
	EN16_d66 = 245,
	EN16_d67 = 246,
	EN16_d68 = 247,
	EN16_d71 = 248,
	EN16_d72 = 249,
	EN16_d73 = 250,
	EN16_d74 = 251,
	EN16_d75 = 252,
	EN16_d76 = 253,
	EN16_d77 = 254,
	EN16_d78 = 255,
	Max = 256,
}
EnumName16 = { _enum = EnumName16 }
BintalkTypes.EnumName16 = setmetatable(EnumName16, BintalkTypes._enum_mt)
local StructType = {
	aaa_ = BintalkTypes.string(),
	bbb_ = BintalkTypes.int32(),
}
BintalkTypes.StructType = function(v)
	local _values = {
		aaa_ = BintalkTypes.string(),
		bbb_ = BintalkTypes.int32(),
	}
	local s = {_defaults = StructType, _values = _values}
	setmetatable(s, BintalkTypes._struct_mt)
	if not v then return s end
	if v.aaa_ then
		s.aaa_ = BintalkTypes.string(v.aaa_)
	end
	if v.bbb_ then
		s.bbb_ = BintalkTypes.int32(v.bbb_)
	end
	return s
end
BintalkWriter.StructType = function(v, b)
	BintalkWriter.string(v.aaa_, b)
	BintalkWriter.int32(v.bbb_, b)
end
BintalkReader.StructType = function(b, p)
	local v = BintalkTypes.StructType()
	v.aaa_, p = BintalkReader.string(b, p, 0XFFFFFFFF)
	v.bbb_, p = BintalkReader.int32(b, p, 0X0)
	return v, p
end
local StructBase = {
	double_ = BintalkTypes.double(),
	float_ = BintalkTypes.float(),
	int64_ = BintalkTypes.int64(),
	uint64_ = BintalkTypes.uint64(),
	int32_ = BintalkTypes.int32(),
	uint32_ = BintalkTypes.uint32(),
	int16_ = BintalkTypes.int16(),
	uint16_ = BintalkTypes.uint16(),
	int8_ = BintalkTypes.int8(),
	uint8_ = BintalkTypes.uint8(),
	bool_ = BintalkTypes.bool(),
	enum_ = BintalkTypes.enum(),
	enum16_ = BintalkTypes.enum16(),
	struct_ = BintalkTypes.StructType(),
	string_ = BintalkTypes.string(),
	string1_ = BintalkTypes.string(),
	bin_ = BintalkTypes.binary(),
	bin1_ = BintalkTypes.binary(),
	doubleArray_ = BintalkTypes.array(BintalkTypes.double),
	floatArray_ = BintalkTypes.array(BintalkTypes.float),
	int64Array_ = BintalkTypes.array(BintalkTypes.int64),
	uint64Array_ = BintalkTypes.array(BintalkTypes.uint64),
	int32Array_ = BintalkTypes.array(BintalkTypes.int32),
	uint32Array_ = BintalkTypes.array(BintalkTypes.uint32),
	int16Array_ = BintalkTypes.array(BintalkTypes.int16),
	uint16Array_ = BintalkTypes.array(BintalkTypes.uint16),
	int8Array_ = BintalkTypes.array(BintalkTypes.int8),
	uint8Array_ = BintalkTypes.array(BintalkTypes.uint8),
	boolArray_ = BintalkTypes.array(BintalkTypes.bool),
	strArray_ = BintalkTypes.array(BintalkTypes.string),
	strArray1_ = BintalkTypes.array(BintalkTypes.string),
	binArray_ = BintalkTypes.array(BintalkTypes.binary),
	bin1Array_ = BintalkTypes.array(BintalkTypes.binary),
	enumArray_ = BintalkTypes.array(BintalkTypes.enum),
	structArray_ = BintalkTypes.array(BintalkTypes.StructType),
}
BintalkTypes.StructBase = function(v)
	local _values = {
		double_ = BintalkTypes.double(),
		float_ = BintalkTypes.float(),
		int64_ = BintalkTypes.int64(),
		uint64_ = BintalkTypes.uint64(),
		int32_ = BintalkTypes.int32(),
		uint32_ = BintalkTypes.uint32(),
		int16_ = BintalkTypes.int16(),
		uint16_ = BintalkTypes.uint16(),
		int8_ = BintalkTypes.int8(),
		uint8_ = BintalkTypes.uint8(),
		bool_ = BintalkTypes.bool(),
		enum_ = BintalkTypes.enum(),
		enum16_ = BintalkTypes.enum16(),
		struct_ = BintalkTypes.StructType(),
		string_ = BintalkTypes.string(),
		string1_ = BintalkTypes.string(),
		bin_ = BintalkTypes.binary(),
		bin1_ = BintalkTypes.binary(),
		doubleArray_ = BintalkTypes.array(BintalkTypes.double),
		floatArray_ = BintalkTypes.array(BintalkTypes.float),
		int64Array_ = BintalkTypes.array(BintalkTypes.int64),
		uint64Array_ = BintalkTypes.array(BintalkTypes.uint64),
		int32Array_ = BintalkTypes.array(BintalkTypes.int32),
		uint32Array_ = BintalkTypes.array(BintalkTypes.uint32),
		int16Array_ = BintalkTypes.array(BintalkTypes.int16),
		uint16Array_ = BintalkTypes.array(BintalkTypes.uint16),
		int8Array_ = BintalkTypes.array(BintalkTypes.int8),
		uint8Array_ = BintalkTypes.array(BintalkTypes.uint8),
		boolArray_ = BintalkTypes.array(BintalkTypes.bool),
		strArray_ = BintalkTypes.array(BintalkTypes.string),
		strArray1_ = BintalkTypes.array(BintalkTypes.string),
		binArray_ = BintalkTypes.array(BintalkTypes.binary),
		bin1Array_ = BintalkTypes.array(BintalkTypes.binary),
		enumArray_ = BintalkTypes.array(BintalkTypes.enum),
		structArray_ = BintalkTypes.array(BintalkTypes.StructType),
	}
	local s = {_defaults = StructBase, _values = _values}
	setmetatable(s, BintalkTypes._struct_mt)
	if not v then return s end
	if v.double_ then
		s.double_ = BintalkTypes.double(v.double_)
	end
	if v.float_ then
		s.float_ = BintalkTypes.float(v.float_)
	end
	if v.int64_ then
		s.int64_ = BintalkTypes.int64(v.int64_)
	end
	if v.uint64_ then
		s.uint64_ = BintalkTypes.uint64(v.uint64_)
	end
	if v.int32_ then
		s.int32_ = BintalkTypes.int32(v.int32_)
	end
	if v.uint32_ then
		s.uint32_ = BintalkTypes.uint32(v.uint32_)
	end
	if v.int16_ then
		s.int16_ = BintalkTypes.int16(v.int16_)
	end
	if v.uint16_ then
		s.uint16_ = BintalkTypes.uint16(v.uint16_)
	end
	if v.int8_ then
		s.int8_ = BintalkTypes.int8(v.int8_)
	end
	if v.uint8_ then
		s.uint8_ = BintalkTypes.uint8(v.uint8_)
	end
	if v.bool_ then
		s.bool_ = BintalkTypes.bool(v.bool_)
	end
	if v.enum_ then
		s.enum_ = BintalkTypes.enum(v.enum_)
	end
	if v.enum16_ then
		s.enum16_ = BintalkTypes.enum16(v.enum16_)
	end
	if v.struct_ then
		s.struct_ = BintalkTypes.StructType(v.struct_)
	end
	if v.string_ then
		s.string_ = BintalkTypes.string(v.string_)
	end
	if v.string1_ then
		s.string1_ = BintalkTypes.string(v.string1_)
	end
	if v.bin_ then
		s.bin_ = BintalkTypes.binary(v.bin_)
	end
	if v.bin1_ then
		s.bin1_ = BintalkTypes.binary(v.bin1_)
	end
	if v.doubleArray_ then
		s.doubleArray_ = BintalkTypes.array(BintalkTypes.double, v.doubleArray_)
	end
	if v.floatArray_ then
		s.floatArray_ = BintalkTypes.array(BintalkTypes.float, v.floatArray_)
	end
	if v.int64Array_ then
		s.int64Array_ = BintalkTypes.array(BintalkTypes.int64, v.int64Array_)
	end
	if v.uint64Array_ then
		s.uint64Array_ = BintalkTypes.array(BintalkTypes.uint64, v.uint64Array_)
	end
	if v.int32Array_ then
		s.int32Array_ = BintalkTypes.array(BintalkTypes.int32, v.int32Array_)
	end
	if v.uint32Array_ then
		s.uint32Array_ = BintalkTypes.array(BintalkTypes.uint32, v.uint32Array_)
	end
	if v.int16Array_ then
		s.int16Array_ = BintalkTypes.array(BintalkTypes.int16, v.int16Array_)
	end
	if v.uint16Array_ then
		s.uint16Array_ = BintalkTypes.array(BintalkTypes.uint16, v.uint16Array_)
	end
	if v.int8Array_ then
		s.int8Array_ = BintalkTypes.array(BintalkTypes.int8, v.int8Array_)
	end
	if v.uint8Array_ then
		s.uint8Array_ = BintalkTypes.array(BintalkTypes.uint8, v.uint8Array_)
	end
	if v.boolArray_ then
		s.boolArray_ = BintalkTypes.array(BintalkTypes.bool, v.boolArray_)
	end
	if v.strArray_ then
		s.strArray_ = BintalkTypes.array(BintalkTypes.string, v.strArray_)
	end
	if v.strArray1_ then
		s.strArray1_ = BintalkTypes.array(BintalkTypes.string, v.strArray1_)
	end
	if v.binArray_ then
		s.binArray_ = BintalkTypes.array(BintalkTypes.binary, v.binArray_)
	end
	if v.bin1Array_ then
		s.bin1Array_ = BintalkTypes.array(BintalkTypes.binary, v.bin1Array_)
	end
	if v.enumArray_ then
		s.enumArray_ = BintalkTypes.array(BintalkTypes.enum, v.enumArray_)
	end
	if v.structArray_ then
		s.structArray_ = BintalkTypes.array(BintalkTypes.StructType, v.structArray_)
	end
	return s
end
BintalkWriter.StructBase = function(v, b)
	BintalkWriter.double(v.double_, b)
	BintalkWriter.float(v.float_, b)
	BintalkWriter.int64(v.int64_, b)
	BintalkWriter.uint64(v.uint64_, b)
	BintalkWriter.int32(v.int32_, b)
	BintalkWriter.uint32(v.uint32_, b)
	BintalkWriter.int16(v.int16_, b)
	BintalkWriter.uint16(v.uint16_, b)
	BintalkWriter.int8(v.int8_, b)
	BintalkWriter.uint8(v.uint8_, b)
	BintalkWriter.bool(v.bool_, b)
	BintalkWriter.enum(v.enum_, b)
	BintalkWriter.enum16(v.enum16_, b)
	BintalkWriter.StructType(v.struct_, b)
	BintalkWriter.string(v.string_, b)
	BintalkWriter.string(v.string1_, b)
	BintalkWriter.binary(v.bin_, b)
	BintalkWriter.binary(v.bin1_, b)
	BintalkWriter.array(BintalkWriter.double, v.doubleArray_, b)
	BintalkWriter.array(BintalkWriter.float, v.floatArray_, b)
	BintalkWriter.array(BintalkWriter.int64, v.int64Array_, b)
	BintalkWriter.array(BintalkWriter.uint64, v.uint64Array_, b)
	BintalkWriter.array(BintalkWriter.int32, v.int32Array_, b)
	BintalkWriter.array(BintalkWriter.uint32, v.uint32Array_, b)
	BintalkWriter.array(BintalkWriter.int16, v.int16Array_, b)
	BintalkWriter.array(BintalkWriter.uint16, v.uint16Array_, b)
	BintalkWriter.array(BintalkWriter.int8, v.int8Array_, b)
	BintalkWriter.array(BintalkWriter.uint8, v.uint8Array_, b)
	BintalkWriter.array(BintalkWriter.bool, v.boolArray_, b)
	BintalkWriter.array(BintalkWriter.string, v.strArray_, b)
	BintalkWriter.array(BintalkWriter.string, v.strArray1_, b)
	BintalkWriter.array(BintalkWriter.binary, v.binArray_, b)
	BintalkWriter.array(BintalkWriter.binary, v.bin1Array_, b)
	BintalkWriter.array(BintalkWriter.enum, v.enumArray_, b)
	BintalkWriter.array(BintalkWriter.StructType, v.structArray_, b)
end
BintalkReader.StructBase = function(b, p)
	local v = BintalkTypes.StructBase()
	v.double_, p = BintalkReader.double(b, p, 0X0)
	v.float_, p = BintalkReader.float(b, p, 0X0)
	v.int64_, p = BintalkReader.int64(b, p, 0X0)
	v.uint64_, p = BintalkReader.uint64(b, p, 0X0)
	v.int32_, p = BintalkReader.int32(b, p, 0X0)
	v.uint32_, p = BintalkReader.uint32(b, p, 0X0)
	v.int16_, p = BintalkReader.int16(b, p, 0X0)
	v.uint16_, p = BintalkReader.uint16(b, p, 0X0)
	v.int8_, p = BintalkReader.int8(b, p, 0X0)
	v.uint8_, p = BintalkReader.uint8(b, p, 0X0)
	v.bool_, p = BintalkReader.bool(b, p, 0X0)
	v.enum_, p = BintalkReader.enum(b, p, 0X2)
	v.enum16_, p = BintalkReader.enum16(b, p, 0X100)
	v.struct_, p = BintalkReader.StructType(b, p, 0X0)
	v.string_, p = BintalkReader.string(b, p, 0XFFFFFFFF)
	v.string1_, p = BintalkReader.string(b, p, 0X20)
	v.bin_, p = BintalkReader.binary(b, p, 0XFFFFFFFF)
	v.bin1_, p = BintalkReader.binary(b, p, 0X20)
	v.doubleArray_, p = BintalkReader.array(b, p, BintalkReader.double, 0XFFFFFFFF, 0X0)
	v.floatArray_, p = BintalkReader.array(b, p, BintalkReader.float, 0XFFFFFFFF, 0X0)
	v.int64Array_, p = BintalkReader.array(b, p, BintalkReader.int64, 0XFFFFFFFF, 0X0)
	v.uint64Array_, p = BintalkReader.array(b, p, BintalkReader.uint64, 0XFFFFFFFF, 0X0)
	v.int32Array_, p = BintalkReader.array(b, p, BintalkReader.int32, 0XFFFFFFFF, 0X0)
	v.uint32Array_, p = BintalkReader.array(b, p, BintalkReader.uint32, 0XFFFFFFFF, 0X0)
	v.int16Array_, p = BintalkReader.array(b, p, BintalkReader.int16, 0XFFFFFFFF, 0X0)
	v.uint16Array_, p = BintalkReader.array(b, p, BintalkReader.uint16, 0XFFFFFFFF, 0X0)
	v.int8Array_, p = BintalkReader.array(b, p, BintalkReader.int8, 0XFFFFFFFF, 0X0)
	v.uint8Array_, p = BintalkReader.array(b, p, BintalkReader.uint8, 0XFFFFFFFF, 0X0)
	v.boolArray_, p = BintalkReader.array(b, p, BintalkReader.bool, 0XFFFFFFFF, 0X0)
	v.strArray_, p = BintalkReader.array(b, p, BintalkReader.string, 0XFFFFFFFF, 0XFFFFFFFF)
	v.strArray1_, p = BintalkReader.array(b, p, BintalkReader.string, 0X8, 0X10)
	v.binArray_, p = BintalkReader.array(b, p, BintalkReader.binary, 0XFFFFFFFF, 0XFFFFFFFF)
	v.bin1Array_, p = BintalkReader.array(b, p, BintalkReader.binary, 0XFFFFFFFF, 0X20)
	v.enumArray_, p = BintalkReader.array(b, p, BintalkReader.enum, 0XFFFFFFFF, 0X2)
	v.structArray_, p = BintalkReader.array(b, p, BintalkReader.StructType, 0XFFFFFFFF, 0X0)
	return v, p
end
