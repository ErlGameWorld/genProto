test_mt = {}
test_mt.__index = test_mt

function test()
	return setmetatable({
		msgId = 1,
		aa = "",
	}, test_mt)
end

function test_mt:encode(byteArray)
	byteArray.write_string(self.aa)
	return byteArray
end

function test_mt:decode(byteArray)
	self.aa = byteArray.read_string()
end

function test_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(1)
	self:encode(byteArray)
	return byteArray.getBytes()
end

phoneNumber_mt = {}
phoneNumber_mt.__index = phoneNumber_mt

function phoneNumber()
	return setmetatable({
		msgId = 2,
		number = {},
		type = 0,
	}, phoneNumber_mt)
end

function phoneNumber_mt:encode(byteArray)
	if self.number and next(self.number) then
		byteArray.write_uint8(1)
		byteArray = self.number:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	byteArray.write_int32(self.type)
	return byteArray
end

function phoneNumber_mt:decode(byteArray)
	local isNilnumber = byteArray.read_uint8()
	if isNilnumber > 0 then
		self.number = test()
		self.number:decode(byteArray)
	else
		self.number = {}
	end
	self.type = byteArray.read_int32()
end

function phoneNumber_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2)
	self:encode(byteArray)
	return byteArray.getBytes()
end

person_mt = {}
person_mt.__index = person_mt

function person()
	return setmetatable({
		msgId = 3,
		name = "",
		id = 0,
		email = "",
		phone = {},
	}, person_mt)
end

function person_mt:encode(byteArray)
	byteArray.write_string(self.name)
	byteArray.write_int32(self.id)
	byteArray.write_string(self.email)
	byteArray.write_uint16(#(self.phone))
	for _, v in ipairs(self.phone) do
		byteArray = v:encode(byteArray)
	end
	return byteArray
end

function person_mt:decode(byteArray)
	self.name = byteArray.read_string()
	self.id = byteArray.read_int32()
	self.email = byteArray.read_string()
	local cntOfphone = byteArray.read_uint16()
	self.phone = {}
	for i = 1, cntOfphone do
		local temp = phoneNumber()
			temp:decode(byteArray)
		self.phone[i] = temp
	end
end

function person_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(3)
	self:encode(byteArray)
	return byteArray.getBytes()
end

addressBook_mt = {}
addressBook_mt.__index = addressBook_mt

function addressBook()
	return setmetatable({
		msgId = 4,
		person = {},
		other = {},
	}, addressBook_mt)
end

function addressBook_mt:encode(byteArray)
	byteArray.write_uint16(#(self.person))
	for _, v in ipairs(self.person) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.other))
	for _, v in ipairs(self.other) do
		byteArray = v:encode(byteArray)
	end
	return byteArray
end

function addressBook_mt:decode(byteArray)
	local cntOfperson = byteArray.read_uint16()
	self.person = {}
	for i = 1, cntOfperson do
		local temp = person()
			temp:decode(byteArray)
		self.person[i] = temp
	end
	local cntOfother = byteArray.read_uint16()
	self.other = {}
	for i = 1, cntOfother do
		local temp = person()
			temp:decode(byteArray)
		self.other[i] = temp
	end
end

function addressBook_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(4)
	self:encode(byteArray)
	return byteArray.getBytes()
end

union_mt = {}
union_mt.__index = union_mt

function union()
	return setmetatable({
		msgId = 5,
		test = "",
		type = 0,
	}, union_mt)
end

function union_mt:encode(byteArray)
	byteArray.write_string(self.test)
	byteArray.write_int32(self.type)
	return byteArray
end

function union_mt:decode(byteArray)
	self.test = byteArray.read_string()
	self.type = byteArray.read_int32()
end

function union_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(5)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tbool_mt = {}
tbool_mt.__index = tbool_mt

function tbool()
	return setmetatable({
		msgId = 6,
		flag = false,
	}, tbool_mt)
end

function tbool_mt:encode(byteArray)
	byteArray.write_bool(self.flag)
	return byteArray
end

function tbool_mt:decode(byteArray)
	self.flag = byteArray.read_bool()
end

function tbool_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(6)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tint8_mt = {}
tint8_mt.__index = tint8_mt

function tint8()
	return setmetatable({
		msgId = 7,
		int1 = 0,
		int2 = 0,
	}, tint8_mt)
end

function tint8_mt:encode(byteArray)
	byteArray.write_int8(self.int1)
	byteArray.write_int8(self.int2)
	return byteArray
end

function tint8_mt:decode(byteArray)
	self.int1 = byteArray.read_int8()
	self.int2 = byteArray.read_int8()
end

function tint8_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(7)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tuint8_mt = {}
tuint8_mt.__index = tuint8_mt

function tuint8()
	return setmetatable({
		msgId = 8,
		int1 = 0,
		int2 = 0,
	}, tuint8_mt)
end

function tuint8_mt:encode(byteArray)
	byteArray.write_uint8(self.int1)
	byteArray.write_uint8(self.int2)
	return byteArray
end

function tuint8_mt:decode(byteArray)
	self.int1 = byteArray.read_uint8()
	self.int2 = byteArray.read_uint8()
end

function tuint8_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(8)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tint16_mt = {}
tint16_mt.__index = tint16_mt

function tint16()
	return setmetatable({
		msgId = 9,
		int1 = 0,
		int2 = 0,
	}, tint16_mt)
end

function tint16_mt:encode(byteArray)
	byteArray.write_int16(self.int1)
	byteArray.write_int16(self.int2)
	return byteArray
end

function tint16_mt:decode(byteArray)
	self.int1 = byteArray.read_int16()
	self.int2 = byteArray.read_int16()
end

function tint16_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(9)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tuint16_mt = {}
tuint16_mt.__index = tuint16_mt

function tuint16()
	return setmetatable({
		msgId = 10,
		int1 = 0,
		int2 = 0,
	}, tuint16_mt)
end

function tuint16_mt:encode(byteArray)
	byteArray.write_uint16(self.int1)
	byteArray.write_uint16(self.int2)
	return byteArray
end

function tuint16_mt:decode(byteArray)
	self.int1 = byteArray.read_uint16()
	self.int2 = byteArray.read_uint16()
end

function tuint16_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(10)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tint32_mt = {}
tint32_mt.__index = tint32_mt

function tint32()
	return setmetatable({
		msgId = 11,
		int1 = 0,
		int2 = 0,
		int3 = 0,
		int4 = 0,
		int5 = 0,
		int6 = 0,
		int7 = 0,
		int8 = 0,
		int9 = 0,
		int10 = 0,
	}, tint32_mt)
end

function tint32_mt:encode(byteArray)
	byteArray.write_int32(self.int1)
	byteArray.write_int32(self.int2)
	byteArray.write_int32(self.int3)
	byteArray.write_int32(self.int4)
	byteArray.write_int32(self.int5)
	byteArray.write_int32(self.int6)
	byteArray.write_int32(self.int7)
	byteArray.write_int32(self.int8)
	byteArray.write_int32(self.int9)
	byteArray.write_int32(self.int10)
	return byteArray
end

function tint32_mt:decode(byteArray)
	self.int1 = byteArray.read_int32()
	self.int2 = byteArray.read_int32()
	self.int3 = byteArray.read_int32()
	self.int4 = byteArray.read_int32()
	self.int5 = byteArray.read_int32()
	self.int6 = byteArray.read_int32()
	self.int7 = byteArray.read_int32()
	self.int8 = byteArray.read_int32()
	self.int9 = byteArray.read_int32()
	self.int10 = byteArray.read_int32()
end

function tint32_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(11)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tuint32_mt = {}
tuint32_mt.__index = tuint32_mt

function tuint32()
	return setmetatable({
		msgId = 12,
		int1 = 0,
		int2 = 0,
	}, tuint32_mt)
end

function tuint32_mt:encode(byteArray)
	byteArray.write_uint32(self.int1)
	byteArray.write_uint32(self.int2)
	return byteArray
end

function tuint32_mt:decode(byteArray)
	self.int1 = byteArray.read_uint32()
	self.int2 = byteArray.read_uint32()
end

function tuint32_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(12)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tint64_mt = {}
tint64_mt.__index = tint64_mt

function tint64()
	return setmetatable({
		msgId = 13,
		int1 = 0,
		int2 = 0,
	}, tint64_mt)
end

function tint64_mt:encode(byteArray)
	byteArray.write_int64(self.int1)
	byteArray.write_int64(self.int2)
	return byteArray
end

function tint64_mt:decode(byteArray)
	self.int1 = byteArray.read_int64()
	self.int2 = byteArray.read_int64()
end

function tint64_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(13)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tuint64_mt = {}
tuint64_mt.__index = tuint64_mt

function tuint64()
	return setmetatable({
		msgId = 14,
		int1 = 0,
		int2 = 0,
	}, tuint64_mt)
end

function tuint64_mt:encode(byteArray)
	byteArray.write_uint64(self.int1)
	byteArray.write_uint64(self.int2)
	return byteArray
end

function tuint64_mt:decode(byteArray)
	self.int1 = byteArray.read_uint64()
	self.int2 = byteArray.read_uint64()
end

function tuint64_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(14)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tinteger_mt = {}
tinteger_mt.__index = tinteger_mt

function tinteger()
	return setmetatable({
		msgId = 15,
		int1 = 0,
		int2 = 0,
		int3 = 0,
		int4 = 0,
		int5 = 0,
		int6 = 0,
		int7 = 0,
		int8 = 0,
	}, tinteger_mt)
end

function tinteger_mt:encode(byteArray)
	byteArray.write_integer(self.int1)
	byteArray.write_integer(self.int2)
	byteArray.write_integer(self.int3)
	byteArray.write_integer(self.int4)
	byteArray.write_integer(self.int5)
	byteArray.write_integer(self.int6)
	byteArray.write_integer(self.int7)
	byteArray.write_integer(self.int8)
	return byteArray
end

function tinteger_mt:decode(byteArray)
	self.int1 = byteArray.read_integer()
	self.int2 = byteArray.read_integer()
	self.int3 = byteArray.read_integer()
	self.int4 = byteArray.read_integer()
	self.int5 = byteArray.read_integer()
	self.int6 = byteArray.read_integer()
	self.int7 = byteArray.read_integer()
	self.int8 = byteArray.read_integer()
end

function tinteger_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(15)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tnumber_mt = {}
tnumber_mt.__index = tnumber_mt

function tnumber()
	return setmetatable({
		msgId = 16,
		int1 = 0,
		int2 = 0,
		int3 = 0,
		int4 = 0,
		int5 = 0,
		int6 = 0,
		int7 = 0,
		int8 = 0,
		float1 = 0,
		float2 = 0,
	}, tnumber_mt)
end

function tnumber_mt:encode(byteArray)
	byteArray.write_number(self.int1)
	byteArray.write_number(self.int2)
	byteArray.write_number(self.int3)
	byteArray.write_number(self.int4)
	byteArray.write_number(self.int5)
	byteArray.write_number(self.int6)
	byteArray.write_number(self.int7)
	byteArray.write_number(self.int8)
	byteArray.write_number(self.float1)
	byteArray.write_number(self.float2)
	return byteArray
end

function tnumber_mt:decode(byteArray)
	self.int1 = byteArray.read_number()
	self.int2 = byteArray.read_number()
	self.int3 = byteArray.read_number()
	self.int4 = byteArray.read_number()
	self.int5 = byteArray.read_number()
	self.int6 = byteArray.read_number()
	self.int7 = byteArray.read_number()
	self.int8 = byteArray.read_number()
	self.float1 = byteArray.read_number()
	self.float2 = byteArray.read_number()
end

function tnumber_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(16)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tfloat_mt = {}
tfloat_mt.__index = tfloat_mt

function tfloat()
	return setmetatable({
		msgId = 17,
		int1 = 0,
		int2 = 0,
	}, tfloat_mt)
end

function tfloat_mt:encode(byteArray)
	byteArray.write_float(self.int1)
	byteArray.write_float(self.int2)
	return byteArray
end

function tfloat_mt:decode(byteArray)
	self.int1 = byteArray.read_float()
	self.int2 = byteArray.read_float()
end

function tfloat_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(17)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tdouble_mt = {}
tdouble_mt.__index = tdouble_mt

function tdouble()
	return setmetatable({
		msgId = 18,
		int1 = 0,
		int2 = 0,
	}, tdouble_mt)
end

function tdouble_mt:encode(byteArray)
	byteArray.write_double(self.int1)
	byteArray.write_double(self.int2)
	return byteArray
end

function tdouble_mt:decode(byteArray)
	self.int1 = byteArray.read_double()
	self.int2 = byteArray.read_double()
end

function tdouble_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(18)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tstring_mt = {}
tstring_mt.__index = tstring_mt

function tstring()
	return setmetatable({
		msgId = 19,
		int1 = "",
		int2 = "",
	}, tstring_mt)
end

function tstring_mt:encode(byteArray)
	byteArray.write_string(self.int1)
	byteArray.write_string(self.int2)
	return byteArray
end

function tstring_mt:decode(byteArray)
	self.int1 = byteArray.read_string()
	self.int2 = byteArray.read_string()
end

function tstring_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(19)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistbool_mt = {}
tlistbool_mt.__index = tlistbool_mt

function tlistbool()
	return setmetatable({
		msgId = 20,
		int1 = {},
	}, tlistbool_mt)
end

function tlistbool_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	byteArray.write_bool_list(self.int1)
	return byteArray
end

function tlistbool_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = byteArray.read_bool_list(cntOfint1)
end

function tlistbool_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(20)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistint8_mt = {}
tlistint8_mt.__index = tlistint8_mt

function tlistint8()
	return setmetatable({
		msgId = 21,
		int1 = {},
	}, tlistint8_mt)
end

function tlistint8_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	byteArray.write_int8_list(self.int1)
	return byteArray
end

function tlistint8_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = byteArray.read_int8_list(cntOfint1)
end

function tlistint8_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(21)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistuint8_mt = {}
tlistuint8_mt.__index = tlistuint8_mt

function tlistuint8()
	return setmetatable({
		msgId = 22,
		int1 = {},
	}, tlistuint8_mt)
end

function tlistuint8_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	byteArray.write_uint8_list(self.int1)
	return byteArray
end

function tlistuint8_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = byteArray.read_uint8_list(cntOfint1)
end

function tlistuint8_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(22)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistint16_mt = {}
tlistint16_mt.__index = tlistint16_mt

function tlistint16()
	return setmetatable({
		msgId = 23,
		int1 = {},
	}, tlistint16_mt)
end

function tlistint16_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	byteArray.write_int16_list(self.int1)
	return byteArray
end

function tlistint16_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = byteArray.read_int16_list(cntOfint1)
end

function tlistint16_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(23)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistuint16_mt = {}
tlistuint16_mt.__index = tlistuint16_mt

function tlistuint16()
	return setmetatable({
		msgId = 24,
		int1 = {},
	}, tlistuint16_mt)
end

function tlistuint16_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	byteArray.write_uint16_list(self.int1)
	return byteArray
end

function tlistuint16_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = byteArray.read_uint16_list(cntOfint1)
end

function tlistuint16_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(24)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistint32_mt = {}
tlistint32_mt.__index = tlistint32_mt

function tlistint32()
	return setmetatable({
		msgId = 25,
		int1 = {},
	}, tlistint32_mt)
end

function tlistint32_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	byteArray.write_int32_list(self.int1)
	return byteArray
end

function tlistint32_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = byteArray.read_int32_list(cntOfint1)
end

function tlistint32_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(25)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistuint32_mt = {}
tlistuint32_mt.__index = tlistuint32_mt

function tlistuint32()
	return setmetatable({
		msgId = 26,
		int1 = {},
	}, tlistuint32_mt)
end

function tlistuint32_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	byteArray.write_uint32_list(self.int1)
	return byteArray
end

function tlistuint32_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = byteArray.read_uint32_list(cntOfint1)
end

function tlistuint32_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(26)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistint64_mt = {}
tlistint64_mt.__index = tlistint64_mt

function tlistint64()
	return setmetatable({
		msgId = 27,
		int1 = {},
	}, tlistint64_mt)
end

function tlistint64_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	byteArray.write_int64_list(self.int1)
	return byteArray
end

function tlistint64_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = byteArray.read_int64_list(cntOfint1)
end

function tlistint64_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(27)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistuint64_mt = {}
tlistuint64_mt.__index = tlistuint64_mt

function tlistuint64()
	return setmetatable({
		msgId = 28,
		int1 = {},
	}, tlistuint64_mt)
end

function tlistuint64_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	byteArray.write_uint64_list(self.int1)
	return byteArray
end

function tlistuint64_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = byteArray.read_uint64_list(cntOfint1)
end

function tlistuint64_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(28)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistinteger_mt = {}
tlistinteger_mt.__index = tlistinteger_mt

function tlistinteger()
	return setmetatable({
		msgId = 29,
		int1 = {},
	}, tlistinteger_mt)
end

function tlistinteger_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	for _, v in ipairs(self.int1) do
		byteArray.write_integer(v)
	end
	return byteArray
end

function tlistinteger_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = {}
	for i = 1, cntOfint1 do
		self.int1[i] = byteArray.read_integer()
	end
end

function tlistinteger_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(29)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistnumber_mt = {}
tlistnumber_mt.__index = tlistnumber_mt

function tlistnumber()
	return setmetatable({
		msgId = 30,
		int1 = {},
	}, tlistnumber_mt)
end

function tlistnumber_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	for _, v in ipairs(self.int1) do
		byteArray.write_number(v)
	end
	return byteArray
end

function tlistnumber_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = {}
	for i = 1, cntOfint1 do
		self.int1[i] = byteArray.read_number()
	end
end

function tlistnumber_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(30)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistfloat_mt = {}
tlistfloat_mt.__index = tlistfloat_mt

function tlistfloat()
	return setmetatable({
		msgId = 31,
		int1 = {},
	}, tlistfloat_mt)
end

function tlistfloat_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	byteArray.write_float_list(self.int1)
	return byteArray
end

function tlistfloat_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = byteArray.read_float_list(cntOfint1)
end

function tlistfloat_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(31)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistdouble_mt = {}
tlistdouble_mt.__index = tlistdouble_mt

function tlistdouble()
	return setmetatable({
		msgId = 32,
		int1 = {},
	}, tlistdouble_mt)
end

function tlistdouble_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	byteArray.write_double_list(self.int1)
	return byteArray
end

function tlistdouble_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = byteArray.read_double_list(cntOfint1)
end

function tlistdouble_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(32)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tliststring_mt = {}
tliststring_mt.__index = tliststring_mt

function tliststring()
	return setmetatable({
		msgId = 33,
		int1 = {},
	}, tliststring_mt)
end

function tliststring_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	for _, v in ipairs(self.int1) do
		byteArray.write_string(v)
	end
	return byteArray
end

function tliststring_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = {}
	for i = 1, cntOfint1 do
		self.int1[i] = byteArray.read_string()
	end
end

function tliststring_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(33)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tlistunion_mt = {}
tlistunion_mt.__index = tlistunion_mt

function tlistunion()
	return setmetatable({
		msgId = 34,
		int1 = {},
	}, tlistunion_mt)
end

function tlistunion_mt:encode(byteArray)
	byteArray.write_uint16(#(self.int1))
	for _, v in ipairs(self.int1) do
		byteArray = v:encode(byteArray)
	end
	return byteArray
end

function tlistunion_mt:decode(byteArray)
	local cntOfint1 = byteArray.read_uint16()
	self.int1 = {}
	for i = 1, cntOfint1 do
		local temp = union()
			temp:decode(byteArray)
		self.int1[i] = temp
	end
end

function tlistunion_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(34)
	self:encode(byteArray)
	return byteArray.getBytes()
end

allType_mt = {}
allType_mt.__index = allType_mt

function allType()
	return setmetatable({
		msgId = 35,
		flag = false,
		v8 = 0,
		uv8 = 0,
		v16 = 0,
		uv16 = 0,
		v32 = 0,
		uv32 = 0,
		v64 = 0,
		uv64 = 0,
		inte8 = 0,
		uinte8 = 0,
		inte16 = 0,
		uinte16 = 0,
		inte32 = 0,
		uinte32 = 0,
		inte64 = 0,
		uinte64 = 0,
		num8 = 0,
		unum8 = 0,
		num16 = 0,
		unum16 = 0,
		num32 = 0,
		unum32 = 0,
		num64 = 0,
		unum64 = 0,
		numfloat = 0,
		numdouble = 0,
		f32 = 0,
		f64 = 0,
		string1 = "",
		string2 = "",
		uval = {},
		lbool = {},
		lint8 = {},
		luint8 = {},
		lint16 = {},
		luint16 = {},
		lint32 = {},
		luint32 = {},
		lint64 = {},
		luint64 = {},
		linte8 = {},
		linte16 = {},
		linte32 = {},
		linte64 = {},
		lnum8 = {},
		lnum16 = {},
		lnum32 = {},
		lnum64 = {},
		lnfloat32 = {},
		lnfloat64 = {},
		lfloat = {},
		ldouble = {},
		lstring = {},
		lunion = {},
	}, allType_mt)
end

function allType_mt:encode(byteArray)
	byteArray.write_bool(self.flag)
	byteArray.write_int8(self.v8)
	byteArray.write_uint8(self.uv8)
	byteArray.write_int16(self.v16)
	byteArray.write_uint16(self.uv16)
	byteArray.write_int32(self.v32)
	byteArray.write_uint32(self.uv32)
	byteArray.write_int64(self.v64)
	byteArray.write_uint64(self.uv64)
	byteArray.write_integer(self.inte8)
	byteArray.write_integer(self.uinte8)
	byteArray.write_integer(self.inte16)
	byteArray.write_integer(self.uinte16)
	byteArray.write_integer(self.inte32)
	byteArray.write_integer(self.uinte32)
	byteArray.write_integer(self.inte64)
	byteArray.write_integer(self.uinte64)
	byteArray.write_number(self.num8)
	byteArray.write_number(self.unum8)
	byteArray.write_number(self.num16)
	byteArray.write_number(self.unum16)
	byteArray.write_number(self.num32)
	byteArray.write_number(self.unum32)
	byteArray.write_number(self.num64)
	byteArray.write_number(self.unum64)
	byteArray.write_number(self.numfloat)
	byteArray.write_number(self.numdouble)
	byteArray.write_float(self.f32)
	byteArray.write_double(self.f64)
	byteArray.write_string(self.string1)
	byteArray.write_string(self.string2)
	if self.uval and next(self.uval) then
		byteArray.write_uint8(1)
		byteArray = self.uval:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	byteArray.write_uint16(#(self.lbool))
	byteArray.write_bool_list(self.lbool)
	byteArray.write_uint16(#(self.lint8))
	byteArray.write_int8_list(self.lint8)
	byteArray.write_uint16(#(self.luint8))
	byteArray.write_uint8_list(self.luint8)
	byteArray.write_uint16(#(self.lint16))
	byteArray.write_int16_list(self.lint16)
	byteArray.write_uint16(#(self.luint16))
	byteArray.write_uint16_list(self.luint16)
	byteArray.write_uint16(#(self.lint32))
	byteArray.write_int32_list(self.lint32)
	byteArray.write_uint16(#(self.luint32))
	byteArray.write_uint32_list(self.luint32)
	byteArray.write_uint16(#(self.lint64))
	byteArray.write_int64_list(self.lint64)
	byteArray.write_uint16(#(self.luint64))
	byteArray.write_uint64_list(self.luint64)
	byteArray.write_uint16(#(self.linte8))
	for _, v in ipairs(self.linte8) do
		byteArray.write_integer(v)
	end
	byteArray.write_uint16(#(self.linte16))
	for _, v in ipairs(self.linte16) do
		byteArray.write_integer(v)
	end
	byteArray.write_uint16(#(self.linte32))
	for _, v in ipairs(self.linte32) do
		byteArray.write_integer(v)
	end
	byteArray.write_uint16(#(self.linte64))
	for _, v in ipairs(self.linte64) do
		byteArray.write_integer(v)
	end
	byteArray.write_uint16(#(self.lnum8))
	for _, v in ipairs(self.lnum8) do
		byteArray.write_number(v)
	end
	byteArray.write_uint16(#(self.lnum16))
	for _, v in ipairs(self.lnum16) do
		byteArray.write_number(v)
	end
	byteArray.write_uint16(#(self.lnum32))
	for _, v in ipairs(self.lnum32) do
		byteArray.write_number(v)
	end
	byteArray.write_uint16(#(self.lnum64))
	for _, v in ipairs(self.lnum64) do
		byteArray.write_number(v)
	end
	byteArray.write_uint16(#(self.lnfloat32))
	for _, v in ipairs(self.lnfloat32) do
		byteArray.write_number(v)
	end
	byteArray.write_uint16(#(self.lnfloat64))
	for _, v in ipairs(self.lnfloat64) do
		byteArray.write_number(v)
	end
	byteArray.write_uint16(#(self.lfloat))
	byteArray.write_float_list(self.lfloat)
	byteArray.write_uint16(#(self.ldouble))
	byteArray.write_double_list(self.ldouble)
	byteArray.write_uint16(#(self.lstring))
	for _, v in ipairs(self.lstring) do
		byteArray.write_string(v)
	end
	byteArray.write_uint16(#(self.lunion))
	for _, v in ipairs(self.lunion) do
		byteArray = v:encode(byteArray)
	end
	return byteArray
end

function allType_mt:decode(byteArray)
	self.flag = byteArray.read_bool()
	self.v8 = byteArray.read_int8()
	self.uv8 = byteArray.read_uint8()
	self.v16 = byteArray.read_int16()
	self.uv16 = byteArray.read_uint16()
	self.v32 = byteArray.read_int32()
	self.uv32 = byteArray.read_uint32()
	self.v64 = byteArray.read_int64()
	self.uv64 = byteArray.read_uint64()
	self.inte8 = byteArray.read_integer()
	self.uinte8 = byteArray.read_integer()
	self.inte16 = byteArray.read_integer()
	self.uinte16 = byteArray.read_integer()
	self.inte32 = byteArray.read_integer()
	self.uinte32 = byteArray.read_integer()
	self.inte64 = byteArray.read_integer()
	self.uinte64 = byteArray.read_integer()
	self.num8 = byteArray.read_number()
	self.unum8 = byteArray.read_number()
	self.num16 = byteArray.read_number()
	self.unum16 = byteArray.read_number()
	self.num32 = byteArray.read_number()
	self.unum32 = byteArray.read_number()
	self.num64 = byteArray.read_number()
	self.unum64 = byteArray.read_number()
	self.numfloat = byteArray.read_number()
	self.numdouble = byteArray.read_number()
	self.f32 = byteArray.read_float()
	self.f64 = byteArray.read_double()
	self.string1 = byteArray.read_string()
	self.string2 = byteArray.read_string()
	local isNiluval = byteArray.read_uint8()
	if isNiluval > 0 then
		self.uval = union()
		self.uval:decode(byteArray)
	else
		self.uval = {}
	end
	local cntOflbool = byteArray.read_uint16()
	self.lbool = byteArray.read_bool_list(cntOflbool)
	local cntOflint8 = byteArray.read_uint16()
	self.lint8 = byteArray.read_int8_list(cntOflint8)
	local cntOfluint8 = byteArray.read_uint16()
	self.luint8 = byteArray.read_uint8_list(cntOfluint8)
	local cntOflint16 = byteArray.read_uint16()
	self.lint16 = byteArray.read_int16_list(cntOflint16)
	local cntOfluint16 = byteArray.read_uint16()
	self.luint16 = byteArray.read_uint16_list(cntOfluint16)
	local cntOflint32 = byteArray.read_uint16()
	self.lint32 = byteArray.read_int32_list(cntOflint32)
	local cntOfluint32 = byteArray.read_uint16()
	self.luint32 = byteArray.read_uint32_list(cntOfluint32)
	local cntOflint64 = byteArray.read_uint16()
	self.lint64 = byteArray.read_int64_list(cntOflint64)
	local cntOfluint64 = byteArray.read_uint16()
	self.luint64 = byteArray.read_uint64_list(cntOfluint64)
	local cntOflinte8 = byteArray.read_uint16()
	self.linte8 = {}
	for i = 1, cntOflinte8 do
		self.linte8[i] = byteArray.read_integer()
	end
	local cntOflinte16 = byteArray.read_uint16()
	self.linte16 = {}
	for i = 1, cntOflinte16 do
		self.linte16[i] = byteArray.read_integer()
	end
	local cntOflinte32 = byteArray.read_uint16()
	self.linte32 = {}
	for i = 1, cntOflinte32 do
		self.linte32[i] = byteArray.read_integer()
	end
	local cntOflinte64 = byteArray.read_uint16()
	self.linte64 = {}
	for i = 1, cntOflinte64 do
		self.linte64[i] = byteArray.read_integer()
	end
	local cntOflnum8 = byteArray.read_uint16()
	self.lnum8 = {}
	for i = 1, cntOflnum8 do
		self.lnum8[i] = byteArray.read_number()
	end
	local cntOflnum16 = byteArray.read_uint16()
	self.lnum16 = {}
	for i = 1, cntOflnum16 do
		self.lnum16[i] = byteArray.read_number()
	end
	local cntOflnum32 = byteArray.read_uint16()
	self.lnum32 = {}
	for i = 1, cntOflnum32 do
		self.lnum32[i] = byteArray.read_number()
	end
	local cntOflnum64 = byteArray.read_uint16()
	self.lnum64 = {}
	for i = 1, cntOflnum64 do
		self.lnum64[i] = byteArray.read_number()
	end
	local cntOflnfloat32 = byteArray.read_uint16()
	self.lnfloat32 = {}
	for i = 1, cntOflnfloat32 do
		self.lnfloat32[i] = byteArray.read_number()
	end
	local cntOflnfloat64 = byteArray.read_uint16()
	self.lnfloat64 = {}
	for i = 1, cntOflnfloat64 do
		self.lnfloat64[i] = byteArray.read_number()
	end
	local cntOflfloat = byteArray.read_uint16()
	self.lfloat = byteArray.read_float_list(cntOflfloat)
	local cntOfldouble = byteArray.read_uint16()
	self.ldouble = byteArray.read_double_list(cntOfldouble)
	local cntOflstring = byteArray.read_uint16()
	self.lstring = {}
	for i = 1, cntOflstring do
		self.lstring[i] = byteArray.read_string()
	end
	local cntOflunion = byteArray.read_uint16()
	self.lunion = {}
	for i = 1, cntOflunion do
		local temp = union()
			temp:decode(byteArray)
		self.lunion[i] = temp
	end
end

function allType_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(35)
	self:encode(byteArray)
	return byteArray.getBytes()
end

testnull_mt = {}
testnull_mt.__index = testnull_mt

function testnull()
	return setmetatable({
		msgId = 36,
	}, testnull_mt)
end

function testnull_mt:encode(byteArray)
	return byteArray
end

function testnull_mt:decode(byteArray)
end

function testnull_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(36)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tbinary_mt = {}
tbinary_mt.__index = tbinary_mt

function tbinary()
	return setmetatable({
		msgId = 37,
		data = "",
		chunks = {},
	}, tbinary_mt)
end

function tbinary_mt:encode(byteArray)
	byteArray.write_binary(self.data)
	byteArray.write_uint16(#(self.chunks))
	for _, v in ipairs(self.chunks) do
		byteArray.write_binary(v)
	end
	return byteArray
end

function tbinary_mt:decode(byteArray)
	self.data = byteArray.read_binary()
	local cntOfchunks = byteArray.read_uint16()
	self.chunks = {}
	for i = 1, cntOfchunks do
		self.chunks[i] = byteArray.read_binary()
	end
end

function tbinary_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(37)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tmap_mt = {}
tmap_mt.__index = tmap_mt

function tmap()
	return setmetatable({
		msgId = 38,
		attrs = nil,
		names = nil,
		phones = nil,
	}, tmap_mt)
end

function tmap_mt:encode(byteArray)
	do
		local __map = self.attrs or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.attrs[__k])
		end
	end
	do
		local __map = self.names or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_string(self.names[__k])
		end
	end
	do
		local __map = self.phones or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray = self.phones[__k]:encode(byteArray)
		end
	end
	return byteArray
end

function tmap_mt:decode(byteArray)
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.attrs = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.names = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = phoneNumber()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.phones = __map
	end
end

function tmap_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(38)
	self:encode(byteArray)
	return byteArray.getBytes()
end

toneof_mt = {}
toneof_mt.__index = toneof_mt

function toneof()
	return setmetatable({
		msgId = 39,
		reason = 0,
		item = nil,
		item2 = nil,
		tip = "",
	}, toneof_mt)
end

function toneof_mt:encode(byteArray)
	byteArray.write_int32(self.reason)
	if not self.item then
		byteArray.write_uint8(0)
	else
		if self.item[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.item[2])
		elseif self.item[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.item[2])
		elseif self.item[1] == 3 then
			byteArray.write_uint8(3)
			byteArray = self.item[2]:encode(byteArray)
		else
			error("invalid oneof")
		end
	end
	if not self.item2 then
		byteArray.write_uint8(0)
	else
		if self.item2[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.item2[2])
		elseif self.item2[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.item2[2])
		elseif self.item2[1] == 3 then
			byteArray.write_uint8(3)
			byteArray = self.item2[2]:encode(byteArray)
		else
			error("invalid oneof")
		end
	end
	byteArray.write_string(self.tip)
	return byteArray
end

function toneof_mt:decode(byteArray)
	self.reason = byteArray.read_int32()
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.item = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.item = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.item = {2, __ov}
		elseif __tag == 3 then
			__ov = phoneNumber()
			__ov:decode(byteArray)
			self.item = {3, __ov}
		else
			error("invalid oneof tag")
		end
	end
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.item2 = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.item2 = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.item2 = {2, __ov}
		elseif __tag == 3 then
			__ov = phoneNumber()
			__ov:decode(byteArray)
			self.item2 = {3, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.tip = byteArray.read_string()
end

function toneof_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(39)
	self:encode(byteArray)
	return byteArray.getBytes()
end

person1_mt = {}
person1_mt.__index = person1_mt

function person1()
	return setmetatable({
		msgId = 1001,
		name = "",
		id = 0,
		email = "",
		phone = {},
	}, person1_mt)
end

function person1_mt:encode(byteArray)
	byteArray.write_string(self.name)
	byteArray.write_int32(self.id)
	byteArray.write_string(self.email)
	byteArray.write_uint16(#(self.phone))
	for _, v in ipairs(self.phone) do
		byteArray = v:encode(byteArray)
	end
	return byteArray
end

function person1_mt:decode(byteArray)
	self.name = byteArray.read_string()
	self.id = byteArray.read_int32()
	self.email = byteArray.read_string()
	local cntOfphone = byteArray.read_uint16()
	self.phone = {}
	for i = 1, cntOfphone do
		local temp = phoneNumber()
			temp:decode(byteArray)
		self.phone[i] = temp
	end
end

function person1_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(1001)
	self:encode(byteArray)
	return byteArray.getBytes()
end

comboLeaf_mt = {}
comboLeaf_mt.__index = comboLeaf_mt

function comboLeaf()
	return setmetatable({
		msgId = 2001,
		id = 0,
		tag = "",
	}, comboLeaf_mt)
end

function comboLeaf_mt:encode(byteArray)
	byteArray.write_int32(self.id)
	byteArray.write_string(self.tag)
	return byteArray
end

function comboLeaf_mt:decode(byteArray)
	self.id = byteArray.read_int32()
	self.tag = byteArray.read_string()
end

function comboLeaf_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2001)
	self:encode(byteArray)
	return byteArray.getBytes()
end

comboNode_mt = {}
comboNode_mt.__index = comboNode_mt

function comboNode()
	return setmetatable({
		msgId = 2002,
		head = {},
		leaves = {},
		index = nil,
	}, comboNode_mt)
end

function comboNode_mt:encode(byteArray)
	if self.head and next(self.head) then
		byteArray.write_uint8(1)
		byteArray = self.head:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	byteArray.write_uint16(#(self.leaves))
	for _, v in ipairs(self.leaves) do
		byteArray = v:encode(byteArray)
	end
	do
		local __map = self.index or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.index[__k])
		end
	end
	return byteArray
end

function comboNode_mt:decode(byteArray)
	local isNilhead = byteArray.read_uint8()
	if isNilhead > 0 then
		self.head = comboLeaf()
		self.head:decode(byteArray)
	else
		self.head = {}
	end
	local cntOfleaves = byteArray.read_uint16()
	self.leaves = {}
	for i = 1, cntOfleaves do
		local temp = comboLeaf()
			temp:decode(byteArray)
		self.leaves[i] = temp
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.index = __map
	end
end

function comboNode_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2002)
	self:encode(byteArray)
	return byteArray.getBytes()
end

comboWrap_mt = {}
comboWrap_mt.__index = comboWrap_mt

function comboWrap()
	return setmetatable({
		msgId = 2003,
		name = "",
		node = {},
		rev = nil,
	}, comboWrap_mt)
end

function comboWrap_mt:encode(byteArray)
	byteArray.write_string(self.name)
	if self.node and next(self.node) then
		byteArray.write_uint8(1)
		byteArray = self.node:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	do
		local __map = self.rev or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_string(self.rev[__k])
		end
	end
	return byteArray
end

function comboWrap_mt:decode(byteArray)
	self.name = byteArray.read_string()
	local isNilnode = byteArray.read_uint8()
	if isNilnode > 0 then
		self.node = comboNode()
		self.node:decode(byteArray)
	else
		self.node = {}
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.rev = __map
	end
end

function comboWrap_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2003)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tMapMatrix_mt = {}
tMapMatrix_mt.__index = tMapMatrix_mt

function tMapMatrix()
	return setmetatable({
		msgId = 2004,
		s_i = nil,
		i_s = nil,
		s_s = nil,
		i_i = nil,
		i64_i = nil,
		u_s = nil,
		s_b = nil,
		i_b = nil,
		s_rec = nil,
		i_leaf = nil,
	}, tMapMatrix_mt)
end

function tMapMatrix_mt:encode(byteArray)
	do
		local __map = self.s_i or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.s_i[__k])
		end
	end
	do
		local __map = self.i_s or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_string(self.i_s[__k])
		end
	end
	do
		local __map = self.s_s or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_string(self.s_s[__k])
		end
	end
	do
		local __map = self.i_i or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_int32(self.i_i[__k])
		end
	end
	do
		local __map = self.i64_i or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int64(__k)
			byteArray.write_int32(self.i64_i[__k])
		end
	end
	do
		local __map = self.u_s or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_uint32(__k)
			byteArray.write_string(self.u_s[__k])
		end
	end
	do
		local __map = self.s_b or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_binary(self.s_b[__k])
		end
	end
	do
		local __map = self.i_b or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_binary(self.i_b[__k])
		end
	end
	do
		local __map = self.s_rec or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray = self.s_rec[__k]:encode(byteArray)
		end
	end
	do
		local __map = self.i_leaf or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray = self.i_leaf[__k]:encode(byteArray)
		end
	end
	return byteArray
end

function tMapMatrix_mt:decode(byteArray)
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.s_i = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.i_s = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.s_s = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.i_i = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int64()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.i64_i = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_uint32()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.u_s = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_binary()
			__map[__k] = __v
		end
		self.s_b = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_binary()
			__map[__k] = __v
		end
		self.i_b = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = phoneNumber()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.s_rec = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = comboLeaf()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.i_leaf = __map
	end
end

function tMapMatrix_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2004)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tListMatrix_mt = {}
tListMatrix_mt.__index = tListMatrix_mt

function tListMatrix()
	return setmetatable({
		msgId = 2005,
		lb = {},
		li8 = {},
		lu8 = {},
		li16 = {},
		lu16 = {},
		li32 = {},
		lu32 = {},
		li64 = {},
		lu64 = {},
		lint = {},
		lnum = {},
		lflt = {},
		ldbl = {},
		lstr = {},
		lbin = {},
		luni = {},
		lph = {},
		lleaf = {},
		lnode = {},
		lli32_1 = {},
		lli32_2 = {},
		lli32_3 = {},
		lmap_1 = nil,
		lmap_2 = nil,
		lmap_3 = nil,
	}, tListMatrix_mt)
end

function tListMatrix_mt:encode(byteArray)
	byteArray.write_uint16(#(self.lb))
	byteArray.write_bool_list(self.lb)
	byteArray.write_uint16(#(self.li8))
	byteArray.write_int8_list(self.li8)
	byteArray.write_uint16(#(self.lu8))
	byteArray.write_uint8_list(self.lu8)
	byteArray.write_uint16(#(self.li16))
	byteArray.write_int16_list(self.li16)
	byteArray.write_uint16(#(self.lu16))
	byteArray.write_uint16_list(self.lu16)
	byteArray.write_uint16(#(self.li32))
	byteArray.write_int32_list(self.li32)
	byteArray.write_uint16(#(self.lu32))
	byteArray.write_uint32_list(self.lu32)
	byteArray.write_uint16(#(self.li64))
	byteArray.write_int64_list(self.li64)
	byteArray.write_uint16(#(self.lu64))
	byteArray.write_uint64_list(self.lu64)
	byteArray.write_uint16(#(self.lint))
	for _, v in ipairs(self.lint) do
		byteArray.write_integer(v)
	end
	byteArray.write_uint16(#(self.lnum))
	for _, v in ipairs(self.lnum) do
		byteArray.write_number(v)
	end
	byteArray.write_uint16(#(self.lflt))
	byteArray.write_float_list(self.lflt)
	byteArray.write_uint16(#(self.ldbl))
	byteArray.write_double_list(self.ldbl)
	byteArray.write_uint16(#(self.lstr))
	for _, v in ipairs(self.lstr) do
		byteArray.write_string(v)
	end
	byteArray.write_uint16(#(self.lbin))
	for _, v in ipairs(self.lbin) do
		byteArray.write_binary(v)
	end
	byteArray.write_uint16(#(self.luni))
	for _, v in ipairs(self.luni) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.lph))
	for _, v in ipairs(self.lph) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.lleaf))
	for _, v in ipairs(self.lleaf) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.lnode))
	for _, v in ipairs(self.lnode) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.lli32_1))
	byteArray.write_int32_list(self.lli32_1)
	byteArray.write_uint16(#(self.lli32_2))
	byteArray.write_int32_list(self.lli32_2)
	byteArray.write_uint16(#(self.lli32_3))
	byteArray.write_int32_list(self.lli32_3)
	do
		local __map = self.lmap_1 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.lmap_1[__k])
		end
	end
	do
		local __map = self.lmap_2 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.lmap_2[__k])
		end
	end
	do
		local __map = self.lmap_3 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.lmap_3[__k])
		end
	end
	return byteArray
end

function tListMatrix_mt:decode(byteArray)
	local cntOflb = byteArray.read_uint16()
	self.lb = byteArray.read_bool_list(cntOflb)
	local cntOfli8 = byteArray.read_uint16()
	self.li8 = byteArray.read_int8_list(cntOfli8)
	local cntOflu8 = byteArray.read_uint16()
	self.lu8 = byteArray.read_uint8_list(cntOflu8)
	local cntOfli16 = byteArray.read_uint16()
	self.li16 = byteArray.read_int16_list(cntOfli16)
	local cntOflu16 = byteArray.read_uint16()
	self.lu16 = byteArray.read_uint16_list(cntOflu16)
	local cntOfli32 = byteArray.read_uint16()
	self.li32 = byteArray.read_int32_list(cntOfli32)
	local cntOflu32 = byteArray.read_uint16()
	self.lu32 = byteArray.read_uint32_list(cntOflu32)
	local cntOfli64 = byteArray.read_uint16()
	self.li64 = byteArray.read_int64_list(cntOfli64)
	local cntOflu64 = byteArray.read_uint16()
	self.lu64 = byteArray.read_uint64_list(cntOflu64)
	local cntOflint = byteArray.read_uint16()
	self.lint = {}
	for i = 1, cntOflint do
		self.lint[i] = byteArray.read_integer()
	end
	local cntOflnum = byteArray.read_uint16()
	self.lnum = {}
	for i = 1, cntOflnum do
		self.lnum[i] = byteArray.read_number()
	end
	local cntOflflt = byteArray.read_uint16()
	self.lflt = byteArray.read_float_list(cntOflflt)
	local cntOfldbl = byteArray.read_uint16()
	self.ldbl = byteArray.read_double_list(cntOfldbl)
	local cntOflstr = byteArray.read_uint16()
	self.lstr = {}
	for i = 1, cntOflstr do
		self.lstr[i] = byteArray.read_string()
	end
	local cntOflbin = byteArray.read_uint16()
	self.lbin = {}
	for i = 1, cntOflbin do
		self.lbin[i] = byteArray.read_binary()
	end
	local cntOfluni = byteArray.read_uint16()
	self.luni = {}
	for i = 1, cntOfluni do
		local temp = union()
			temp:decode(byteArray)
		self.luni[i] = temp
	end
	local cntOflph = byteArray.read_uint16()
	self.lph = {}
	for i = 1, cntOflph do
		local temp = phoneNumber()
			temp:decode(byteArray)
		self.lph[i] = temp
	end
	local cntOflleaf = byteArray.read_uint16()
	self.lleaf = {}
	for i = 1, cntOflleaf do
		local temp = comboLeaf()
			temp:decode(byteArray)
		self.lleaf[i] = temp
	end
	local cntOflnode = byteArray.read_uint16()
	self.lnode = {}
	for i = 1, cntOflnode do
		local temp = comboNode()
			temp:decode(byteArray)
		self.lnode[i] = temp
	end
	local cntOflli32_1 = byteArray.read_uint16()
	self.lli32_1 = byteArray.read_int32_list(cntOflli32_1)
	local cntOflli32_2 = byteArray.read_uint16()
	self.lli32_2 = byteArray.read_int32_list(cntOflli32_2)
	local cntOflli32_3 = byteArray.read_uint16()
	self.lli32_3 = byteArray.read_int32_list(cntOflli32_3)
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.lmap_1 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.lmap_2 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.lmap_3 = __map
	end
end

function tListMatrix_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2005)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tOneofMatrix_mt = {}
tOneofMatrix_mt.__index = tOneofMatrix_mt

function tOneofMatrix()
	return setmetatable({
		msgId = 2006,
		payload = nil,
	}, tOneofMatrix_mt)
end

function tOneofMatrix_mt:encode(byteArray)
	if not self.payload then
		byteArray.write_uint8(0)
	else
		if self.payload[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_bool(self.payload[2])
		elseif self.payload[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_int32(self.payload[2])
		elseif self.payload[1] == 3 then
			byteArray.write_uint8(3)
			byteArray.write_string(self.payload[2])
		elseif self.payload[1] == 4 then
			byteArray.write_uint8(4)
			byteArray.write_binary(self.payload[2])
		elseif self.payload[1] == 5 then
			byteArray.write_uint8(5)
			byteArray = self.payload[2]:encode(byteArray)
		elseif self.payload[1] == 6 then
			byteArray.write_uint8(6)
			byteArray = self.payload[2]:encode(byteArray)
		elseif self.payload[1] == 7 then
			byteArray.write_uint8(7)
			byteArray = self.payload[2]:encode(byteArray)
		elseif self.payload[1] == 8 then
			byteArray.write_uint8(8)
			byteArray.write_uint16(#(self.payload[2]))
			byteArray.write_int32_list(self.payload[2])
		elseif self.payload[1] == 9 then
			byteArray.write_uint8(9)
			byteArray.write_uint16(#(self.payload[2]))
			for _, __v in ipairs(self.payload[2]) do
				byteArray.write_string(__v)
			end
		else
			error("invalid oneof")
		end
	end
	return byteArray
end

function tOneofMatrix_mt:decode(byteArray)
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.payload = nil
		elseif __tag == 1 then
			__ov = byteArray.read_bool()
			self.payload = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_int32()
			self.payload = {2, __ov}
		elseif __tag == 3 then
			__ov = byteArray.read_string()
			self.payload = {3, __ov}
		elseif __tag == 4 then
			__ov = byteArray.read_binary()
			self.payload = {4, __ov}
		elseif __tag == 5 then
			__ov = phoneNumber()
			__ov:decode(byteArray)
			self.payload = {5, __ov}
		elseif __tag == 6 then
			__ov = comboLeaf()
			__ov:decode(byteArray)
			self.payload = {6, __ov}
		elseif __tag == 7 then
			__ov = comboNode()
			__ov:decode(byteArray)
			self.payload = {7, __ov}
		elseif __tag == 8 then
			do
				local __cnt = byteArray.read_uint16()
				__ov = byteArray.read_int32_list(__cnt)
			end
			self.payload = {8, __ov}
		elseif __tag == 9 then
			do
				local __cnt = byteArray.read_uint16()
				local __arr = {}
				for __i = 1, __cnt do __arr[__i] = byteArray.read_string() end
				__ov = __arr
			end
			self.payload = {9, __ov}
		else
			error("invalid oneof tag")
		end
	end
end

function tOneofMatrix_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2006)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tNestDeep_mt = {}
tNestDeep_mt.__index = tNestDeep_mt

function tNestDeep()
	return setmetatable({
		msgId = 2007,
		ms_leaf = nil,
		wrap = {},
		nested = nil,
		lwrap_1 = {},
		lwrap_2 = {},
		lwrap_3 = {},
	}, tNestDeep_mt)
end

function tNestDeep_mt:encode(byteArray)
	do
		local __map = self.ms_leaf or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray = self.ms_leaf[__k]:encode(byteArray)
		end
	end
	if self.wrap and next(self.wrap) then
		byteArray.write_uint8(1)
		byteArray = self.wrap:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if not self.nested then
		byteArray.write_uint8(0)
	else
		if self.nested[1] == 1 then
			byteArray.write_uint8(1)
			byteArray = self.nested[2]:encode(byteArray)
		elseif self.nested[1] == 2 then
			byteArray.write_uint8(2)
			byteArray = self.nested[2]:encode(byteArray)
		else
			error("invalid oneof")
		end
	end
	byteArray.write_uint16(#(self.lwrap_1))
	for _, v in ipairs(self.lwrap_1) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.lwrap_2))
	for _, v in ipairs(self.lwrap_2) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.lwrap_3))
	for _, v in ipairs(self.lwrap_3) do
		byteArray = v:encode(byteArray)
	end
	return byteArray
end

function tNestDeep_mt:decode(byteArray)
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = comboLeaf()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.ms_leaf = __map
	end
	local isNilwrap = byteArray.read_uint8()
	if isNilwrap > 0 then
		self.wrap = comboWrap()
		self.wrap:decode(byteArray)
	else
		self.wrap = {}
	end
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.nested = nil
		elseif __tag == 1 then
			__ov = comboNode()
			__ov:decode(byteArray)
			self.nested = {1, __ov}
		elseif __tag == 2 then
			__ov = comboLeaf()
			__ov:decode(byteArray)
			self.nested = {2, __ov}
		else
			error("invalid oneof tag")
		end
	end
	local cntOflwrap_1 = byteArray.read_uint16()
	self.lwrap_1 = {}
	for i = 1, cntOflwrap_1 do
		local temp = comboWrap()
			temp:decode(byteArray)
		self.lwrap_1[i] = temp
	end
	local cntOflwrap_2 = byteArray.read_uint16()
	self.lwrap_2 = {}
	for i = 1, cntOflwrap_2 do
		local temp = comboWrap()
			temp:decode(byteArray)
		self.lwrap_2[i] = temp
	end
	local cntOflwrap_3 = byteArray.read_uint16()
	self.lwrap_3 = {}
	for i = 1, cntOflwrap_3 do
		local temp = comboWrap()
			temp:decode(byteArray)
		self.lwrap_3[i] = temp
	end
end

function tNestDeep_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2007)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tInterleave_mt = {}
tInterleave_mt.__index = tInterleave_mt

function tInterleave()
	return setmetatable({
		msgId = 2008,
		n1 = 0,
		s1 = "",
		m1 = nil,
		l1 = {},
		o1 = nil,
		n2 = 0,
		s2 = "",
		m2 = nil,
		l2 = {},
		o2 = nil,
		n3 = 0,
		s3 = "",
		m3 = nil,
		l3 = {},
		o3 = nil,
		b1 = "",
		b2 = "",
		b3 = "",
	}, tInterleave_mt)
end

function tInterleave_mt:encode(byteArray)
	byteArray.write_int32(self.n1)
	byteArray.write_string(self.s1)
	do
		local __map = self.m1 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.m1[__k])
		end
	end
	byteArray.write_uint16(#(self.l1))
	byteArray.write_int32_list(self.l1)
	if not self.o1 then
		byteArray.write_uint8(0)
	else
		if self.o1[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.o1[2])
		elseif self.o1[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.o1[2])
		else
			error("invalid oneof")
		end
	end
	byteArray.write_int32(self.n2)
	byteArray.write_string(self.s2)
	do
		local __map = self.m2 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.m2[__k])
		end
	end
	byteArray.write_uint16(#(self.l2))
	byteArray.write_int32_list(self.l2)
	if not self.o2 then
		byteArray.write_uint8(0)
	else
		if self.o2[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.o2[2])
		elseif self.o2[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.o2[2])
		else
			error("invalid oneof")
		end
	end
	byteArray.write_int32(self.n3)
	byteArray.write_string(self.s3)
	do
		local __map = self.m3 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.m3[__k])
		end
	end
	byteArray.write_uint16(#(self.l3))
	byteArray.write_int32_list(self.l3)
	if not self.o3 then
		byteArray.write_uint8(0)
	else
		if self.o3[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.o3[2])
		elseif self.o3[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.o3[2])
		else
			error("invalid oneof")
		end
	end
	byteArray.write_binary(self.b1)
	byteArray.write_binary(self.b2)
	byteArray.write_binary(self.b3)
	return byteArray
end

function tInterleave_mt:decode(byteArray)
	self.n1 = byteArray.read_int32()
	self.s1 = byteArray.read_string()
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.m1 = __map
	end
	local cntOfl1 = byteArray.read_uint16()
	self.l1 = byteArray.read_int32_list(cntOfl1)
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.o1 = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.o1 = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.o1 = {2, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.n2 = byteArray.read_int32()
	self.s2 = byteArray.read_string()
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.m2 = __map
	end
	local cntOfl2 = byteArray.read_uint16()
	self.l2 = byteArray.read_int32_list(cntOfl2)
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.o2 = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.o2 = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.o2 = {2, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.n3 = byteArray.read_int32()
	self.s3 = byteArray.read_string()
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.m3 = __map
	end
	local cntOfl3 = byteArray.read_uint16()
	self.l3 = byteArray.read_int32_list(cntOfl3)
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.o3 = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.o3 = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.o3 = {2, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.b1 = byteArray.read_binary()
	self.b2 = byteArray.read_binary()
	self.b3 = byteArray.read_binary()
end

function tInterleave_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2008)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tMultiOneof_mt = {}
tMultiOneof_mt.__index = tMultiOneof_mt

function tMultiOneof()
	return setmetatable({
		msgId = 2009,
		a = nil,
		g1 = 0,
		b = nil,
		g2 = 0,
		c = nil,
		g3 = 0,
		d = nil,
		g4 = 0,
		e = nil,
	}, tMultiOneof_mt)
end

function tMultiOneof_mt:encode(byteArray)
	if not self.a then
		byteArray.write_uint8(0)
	else
		if self.a[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.a[2])
		elseif self.a[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.a[2])
		else
			error("invalid oneof")
		end
	end
	byteArray.write_int32(self.g1)
	if not self.b then
		byteArray.write_uint8(0)
	else
		if self.b[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.b[2])
		elseif self.b[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.b[2])
		elseif self.b[1] == 3 then
			byteArray.write_uint8(3)
			byteArray.write_binary(self.b[2])
		else
			error("invalid oneof")
		end
	end
	byteArray.write_int32(self.g2)
	if not self.c then
		byteArray.write_uint8(0)
	else
		if self.c[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.c[2])
		elseif self.c[1] == 2 then
			byteArray.write_uint8(2)
			byteArray = self.c[2]:encode(byteArray)
		else
			error("invalid oneof")
		end
	end
	byteArray.write_int32(self.g3)
	if not self.d then
		byteArray.write_uint8(0)
	else
		if self.d[1] == 1 then
			byteArray.write_uint8(1)
			byteArray = self.d[2]:encode(byteArray)
		elseif self.d[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_uint16(#(self.d[2]))
			byteArray.write_int32_list(self.d[2])
		else
			error("invalid oneof")
		end
	end
	byteArray.write_int32(self.g4)
	if not self.e then
		byteArray.write_uint8(0)
	else
		if self.e[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_string(self.e[2])
		elseif self.e[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_int32(self.e[2])
		else
			error("invalid oneof")
		end
	end
	return byteArray
end

function tMultiOneof_mt:decode(byteArray)
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.a = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.a = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.a = {2, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.g1 = byteArray.read_int32()
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.b = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.b = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.b = {2, __ov}
		elseif __tag == 3 then
			__ov = byteArray.read_binary()
			self.b = {3, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.g2 = byteArray.read_int32()
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.c = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.c = {1, __ov}
		elseif __tag == 2 then
			__ov = phoneNumber()
			__ov:decode(byteArray)
			self.c = {2, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.g3 = byteArray.read_int32()
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.d = nil
		elseif __tag == 1 then
			__ov = comboLeaf()
			__ov:decode(byteArray)
			self.d = {1, __ov}
		elseif __tag == 2 then
			do
				local __cnt = byteArray.read_uint16()
				__ov = byteArray.read_int32_list(__cnt)
			end
			self.d = {2, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.g4 = byteArray.read_int32()
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.e = nil
		elseif __tag == 1 then
			__ov = byteArray.read_string()
			self.e = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_int32()
			self.e = {2, __ov}
		else
			error("invalid oneof tag")
		end
	end
end

function tMultiOneof_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2009)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tEmptyEdge_mt = {}
tEmptyEdge_mt.__index = tEmptyEdge_mt

function tEmptyEdge()
	return setmetatable({
		msgId = 2010,
		empty_map = nil,
		empty_list = {},
		empty_strs = {},
		empty_bin = "",
		empty_str = "",
		none = nil,
		empty_phone = {},
		empty_leaf = {},
	}, tEmptyEdge_mt)
end

function tEmptyEdge_mt:encode(byteArray)
	do
		local __map = self.empty_map or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.empty_map[__k])
		end
	end
	byteArray.write_uint16(#(self.empty_list))
	byteArray.write_int32_list(self.empty_list)
	byteArray.write_uint16(#(self.empty_strs))
	for _, v in ipairs(self.empty_strs) do
		byteArray.write_string(v)
	end
	byteArray.write_binary(self.empty_bin)
	byteArray.write_string(self.empty_str)
	if not self.none then
		byteArray.write_uint8(0)
	else
		if self.none[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.none[2])
		elseif self.none[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.none[2])
		else
			error("invalid oneof")
		end
	end
	if self.empty_phone and next(self.empty_phone) then
		byteArray.write_uint8(1)
		byteArray = self.empty_phone:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.empty_leaf and next(self.empty_leaf) then
		byteArray.write_uint8(1)
		byteArray = self.empty_leaf:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	return byteArray
end

function tEmptyEdge_mt:decode(byteArray)
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.empty_map = __map
	end
	local cntOfempty_list = byteArray.read_uint16()
	self.empty_list = byteArray.read_int32_list(cntOfempty_list)
	local cntOfempty_strs = byteArray.read_uint16()
	self.empty_strs = {}
	for i = 1, cntOfempty_strs do
		self.empty_strs[i] = byteArray.read_string()
	end
	self.empty_bin = byteArray.read_binary()
	self.empty_str = byteArray.read_string()
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.none = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.none = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.none = {2, __ov}
		else
			error("invalid oneof tag")
		end
	end
	local isNilempty_phone = byteArray.read_uint8()
	if isNilempty_phone > 0 then
		self.empty_phone = phoneNumber()
		self.empty_phone:decode(byteArray)
	else
		self.empty_phone = {}
	end
	local isNilempty_leaf = byteArray.read_uint8()
	if isNilempty_leaf > 0 then
		self.empty_leaf = comboLeaf()
		self.empty_leaf:decode(byteArray)
	else
		self.empty_leaf = {}
	end
end

function tEmptyEdge_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2010)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tScalarRun_mt = {}
tScalarRun_mt.__index = tScalarRun_mt

function tScalarRun()
	return setmetatable({
		msgId = 2011,
		a1 = 0,
		a2 = 0,
		a3 = 0,
		b1 = "",
		b2 = "",
		b3 = "",
		c1 = nil,
		c2 = nil,
		c3 = nil,
	}, tScalarRun_mt)
end

function tScalarRun_mt:encode(byteArray)
	byteArray.write_int32(self.a1)
	byteArray.write_int32(self.a2)
	byteArray.write_int32(self.a3)
	byteArray.write_string(self.b1)
	byteArray.write_string(self.b2)
	byteArray.write_string(self.b3)
	do
		local __map = self.c1 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.c1[__k])
		end
	end
	do
		local __map = self.c2 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.c2[__k])
		end
	end
	do
		local __map = self.c3 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.c3[__k])
		end
	end
	return byteArray
end

function tScalarRun_mt:decode(byteArray)
	self.a1 = byteArray.read_int32()
	self.a2 = byteArray.read_int32()
	self.a3 = byteArray.read_int32()
	self.b1 = byteArray.read_string()
	self.b2 = byteArray.read_string()
	self.b3 = byteArray.read_string()
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.c1 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.c2 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.c3 = __map
	end
end

function tScalarRun_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2011)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tRepeatAll_mt = {}
tRepeatAll_mt.__index = tRepeatAll_mt

function tRepeatAll()
	return setmetatable({
		msgId = 2012,
		b1 = false,
		b2 = false,
		b3 = false,
		i32_1 = 0,
		i32_2 = 0,
		i32_3 = 0,
		gap_a = "",
		i32_4 = 0,
		i8_1 = 0,
		i8_2 = 0,
		u8_1 = 0,
		i8_3 = 0,
		u8_2 = 0,
		u8_3 = 0,
		i16_1 = 0,
		i16_2 = 0,
		i16_3 = 0,
		u16_1 = 0,
		gap_b = "",
		u16_2 = 0,
		u16_3 = 0,
		i32_5 = 0,
		i32_6 = 0,
		i32_7 = 0,
		u32_1 = 0,
		u32_2 = 0,
		u32_3 = 0,
		i64_1 = 0,
		i64_2 = 0,
		gap_c = "",
		i64_3 = 0,
		u64_1 = 0,
		u64_2 = 0,
		u64_3 = 0,
		inte_1 = 0,
		inte_2 = 0,
		inte_3 = 0,
		num_1 = 0,
		gap_d = "",
		num_2 = 0,
		num_3 = 0,
		flt_1 = 0,
		flt_2 = 0,
		flt_3 = 0,
		dbl_1 = 0,
		dbl_2 = 0,
		dbl_3 = 0,
		str_1 = "",
		str_2 = "",
		str_3 = "",
		gap_e = 0,
		str_4 = "",
		bin_1 = "",
		bin_2 = "",
		bin_3 = "",
		uni_1 = {},
		uni_2 = {},
		uni_3 = {},
		li32_1 = {},
		li32_2 = {},
		li32_3 = {},
		lstr_1 = {},
		gap_f = 0,
		lstr_2 = {},
		lstr_3 = {},
		lbin_1 = {},
		lbin_2 = {},
		lbin_3 = {},
		lbool_1 = {},
		lbool_2 = {},
		lbool_3 = {},
		lint_1 = {},
		lint_2 = {},
		lint_3 = {},
		lnum_1 = {},
		lnum_2 = {},
		lnum_3 = {},
		lph_1 = {},
		lph_2 = {},
		gap_g = "",
		lph_3 = {},
		lleaf_1 = {},
		lleaf_2 = {},
		lleaf_3 = {},
		ms_i_1 = nil,
		ms_i_2 = nil,
		ms_i_3 = nil,
		mi_s_1 = nil,
		gap_h = 0,
		mi_s_2 = nil,
		mi_s_3 = nil,
		ms_s_1 = nil,
		ms_s_2 = nil,
		ms_s_3 = nil,
		mi_i_1 = nil,
		mi_i_2 = nil,
		mi_i_3 = nil,
		ms_b_1 = nil,
		ms_b_2 = nil,
		ms_b_3 = nil,
		mi_b_1 = nil,
		mi_b_2 = nil,
		mi_b_3 = nil,
		ms_p_1 = nil,
		ms_p_2 = nil,
		ms_p_3 = nil,
		mi_l_1 = nil,
		mi_l_2 = nil,
		mi_l_3 = nil,
		pick1 = nil,
		gap_i = 0,
		pick2 = nil,
		gap_j = "",
		pick3 = nil,
		ph_1 = {},
		ph_2 = {},
		gap_k = "",
		ph_3 = {},
		leaf_1 = {},
		leaf_2 = {},
		leaf_3 = {},
		node_1 = {},
		node_2 = {},
		node_3 = {},
		wrap_1 = {},
		wrap_2 = {},
		wrap_3 = {},
	}, tRepeatAll_mt)
end

function tRepeatAll_mt:encode(byteArray)
	byteArray.write_bool(self.b1)
	byteArray.write_bool(self.b2)
	byteArray.write_bool(self.b3)
	byteArray.write_int32(self.i32_1)
	byteArray.write_int32(self.i32_2)
	byteArray.write_int32(self.i32_3)
	byteArray.write_string(self.gap_a)
	byteArray.write_int32(self.i32_4)
	byteArray.write_int8(self.i8_1)
	byteArray.write_int8(self.i8_2)
	byteArray.write_uint8(self.u8_1)
	byteArray.write_int8(self.i8_3)
	byteArray.write_uint8(self.u8_2)
	byteArray.write_uint8(self.u8_3)
	byteArray.write_int16(self.i16_1)
	byteArray.write_int16(self.i16_2)
	byteArray.write_int16(self.i16_3)
	byteArray.write_uint16(self.u16_1)
	byteArray.write_string(self.gap_b)
	byteArray.write_uint16(self.u16_2)
	byteArray.write_uint16(self.u16_3)
	byteArray.write_int32(self.i32_5)
	byteArray.write_int32(self.i32_6)
	byteArray.write_int32(self.i32_7)
	byteArray.write_uint32(self.u32_1)
	byteArray.write_uint32(self.u32_2)
	byteArray.write_uint32(self.u32_3)
	byteArray.write_int64(self.i64_1)
	byteArray.write_int64(self.i64_2)
	byteArray.write_string(self.gap_c)
	byteArray.write_int64(self.i64_3)
	byteArray.write_uint64(self.u64_1)
	byteArray.write_uint64(self.u64_2)
	byteArray.write_uint64(self.u64_3)
	byteArray.write_integer(self.inte_1)
	byteArray.write_integer(self.inte_2)
	byteArray.write_integer(self.inte_3)
	byteArray.write_number(self.num_1)
	byteArray.write_string(self.gap_d)
	byteArray.write_number(self.num_2)
	byteArray.write_number(self.num_3)
	byteArray.write_float(self.flt_1)
	byteArray.write_float(self.flt_2)
	byteArray.write_float(self.flt_3)
	byteArray.write_double(self.dbl_1)
	byteArray.write_double(self.dbl_2)
	byteArray.write_double(self.dbl_3)
	byteArray.write_string(self.str_1)
	byteArray.write_string(self.str_2)
	byteArray.write_string(self.str_3)
	byteArray.write_int32(self.gap_e)
	byteArray.write_string(self.str_4)
	byteArray.write_binary(self.bin_1)
	byteArray.write_binary(self.bin_2)
	byteArray.write_binary(self.bin_3)
	if self.uni_1 and next(self.uni_1) then
		byteArray.write_uint8(1)
		byteArray = self.uni_1:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.uni_2 and next(self.uni_2) then
		byteArray.write_uint8(1)
		byteArray = self.uni_2:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.uni_3 and next(self.uni_3) then
		byteArray.write_uint8(1)
		byteArray = self.uni_3:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	byteArray.write_uint16(#(self.li32_1))
	byteArray.write_int32_list(self.li32_1)
	byteArray.write_uint16(#(self.li32_2))
	byteArray.write_int32_list(self.li32_2)
	byteArray.write_uint16(#(self.li32_3))
	byteArray.write_int32_list(self.li32_3)
	byteArray.write_uint16(#(self.lstr_1))
	for _, v in ipairs(self.lstr_1) do
		byteArray.write_string(v)
	end
	byteArray.write_int32(self.gap_f)
	byteArray.write_uint16(#(self.lstr_2))
	for _, v in ipairs(self.lstr_2) do
		byteArray.write_string(v)
	end
	byteArray.write_uint16(#(self.lstr_3))
	for _, v in ipairs(self.lstr_3) do
		byteArray.write_string(v)
	end
	byteArray.write_uint16(#(self.lbin_1))
	for _, v in ipairs(self.lbin_1) do
		byteArray.write_binary(v)
	end
	byteArray.write_uint16(#(self.lbin_2))
	for _, v in ipairs(self.lbin_2) do
		byteArray.write_binary(v)
	end
	byteArray.write_uint16(#(self.lbin_3))
	for _, v in ipairs(self.lbin_3) do
		byteArray.write_binary(v)
	end
	byteArray.write_uint16(#(self.lbool_1))
	byteArray.write_bool_list(self.lbool_1)
	byteArray.write_uint16(#(self.lbool_2))
	byteArray.write_bool_list(self.lbool_2)
	byteArray.write_uint16(#(self.lbool_3))
	byteArray.write_bool_list(self.lbool_3)
	byteArray.write_uint16(#(self.lint_1))
	for _, v in ipairs(self.lint_1) do
		byteArray.write_integer(v)
	end
	byteArray.write_uint16(#(self.lint_2))
	for _, v in ipairs(self.lint_2) do
		byteArray.write_integer(v)
	end
	byteArray.write_uint16(#(self.lint_3))
	for _, v in ipairs(self.lint_3) do
		byteArray.write_integer(v)
	end
	byteArray.write_uint16(#(self.lnum_1))
	for _, v in ipairs(self.lnum_1) do
		byteArray.write_number(v)
	end
	byteArray.write_uint16(#(self.lnum_2))
	for _, v in ipairs(self.lnum_2) do
		byteArray.write_number(v)
	end
	byteArray.write_uint16(#(self.lnum_3))
	for _, v in ipairs(self.lnum_3) do
		byteArray.write_number(v)
	end
	byteArray.write_uint16(#(self.lph_1))
	for _, v in ipairs(self.lph_1) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.lph_2))
	for _, v in ipairs(self.lph_2) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_string(self.gap_g)
	byteArray.write_uint16(#(self.lph_3))
	for _, v in ipairs(self.lph_3) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.lleaf_1))
	for _, v in ipairs(self.lleaf_1) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.lleaf_2))
	for _, v in ipairs(self.lleaf_2) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.lleaf_3))
	for _, v in ipairs(self.lleaf_3) do
		byteArray = v:encode(byteArray)
	end
	do
		local __map = self.ms_i_1 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.ms_i_1[__k])
		end
	end
	do
		local __map = self.ms_i_2 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.ms_i_2[__k])
		end
	end
	do
		local __map = self.ms_i_3 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.ms_i_3[__k])
		end
	end
	do
		local __map = self.mi_s_1 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_string(self.mi_s_1[__k])
		end
	end
	byteArray.write_int32(self.gap_h)
	do
		local __map = self.mi_s_2 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_string(self.mi_s_2[__k])
		end
	end
	do
		local __map = self.mi_s_3 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_string(self.mi_s_3[__k])
		end
	end
	do
		local __map = self.ms_s_1 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_string(self.ms_s_1[__k])
		end
	end
	do
		local __map = self.ms_s_2 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_string(self.ms_s_2[__k])
		end
	end
	do
		local __map = self.ms_s_3 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_string(self.ms_s_3[__k])
		end
	end
	do
		local __map = self.mi_i_1 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_int32(self.mi_i_1[__k])
		end
	end
	do
		local __map = self.mi_i_2 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_int32(self.mi_i_2[__k])
		end
	end
	do
		local __map = self.mi_i_3 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_int32(self.mi_i_3[__k])
		end
	end
	do
		local __map = self.ms_b_1 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_binary(self.ms_b_1[__k])
		end
	end
	do
		local __map = self.ms_b_2 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_binary(self.ms_b_2[__k])
		end
	end
	do
		local __map = self.ms_b_3 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_binary(self.ms_b_3[__k])
		end
	end
	do
		local __map = self.mi_b_1 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_binary(self.mi_b_1[__k])
		end
	end
	do
		local __map = self.mi_b_2 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_binary(self.mi_b_2[__k])
		end
	end
	do
		local __map = self.mi_b_3 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_binary(self.mi_b_3[__k])
		end
	end
	do
		local __map = self.ms_p_1 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray = self.ms_p_1[__k]:encode(byteArray)
		end
	end
	do
		local __map = self.ms_p_2 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray = self.ms_p_2[__k]:encode(byteArray)
		end
	end
	do
		local __map = self.ms_p_3 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray = self.ms_p_3[__k]:encode(byteArray)
		end
	end
	do
		local __map = self.mi_l_1 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray = self.mi_l_1[__k]:encode(byteArray)
		end
	end
	do
		local __map = self.mi_l_2 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray = self.mi_l_2[__k]:encode(byteArray)
		end
	end
	do
		local __map = self.mi_l_3 or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray = self.mi_l_3[__k]:encode(byteArray)
		end
	end
	if not self.pick1 then
		byteArray.write_uint8(0)
	else
		if self.pick1[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.pick1[2])
		elseif self.pick1[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.pick1[2])
		elseif self.pick1[1] == 3 then
			byteArray.write_uint8(3)
			byteArray.write_binary(self.pick1[2])
		else
			error("invalid oneof")
		end
	end
	byteArray.write_int32(self.gap_i)
	if not self.pick2 then
		byteArray.write_uint8(0)
	else
		if self.pick2[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.pick2[2])
		elseif self.pick2[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.pick2[2])
		elseif self.pick2[1] == 3 then
			byteArray.write_uint8(3)
			byteArray = self.pick2[2]:encode(byteArray)
		else
			error("invalid oneof")
		end
	end
	byteArray.write_string(self.gap_j)
	if not self.pick3 then
		byteArray.write_uint8(0)
	else
		if self.pick3[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.pick3[2])
		elseif self.pick3[1] == 2 then
			byteArray.write_uint8(2)
			byteArray = self.pick3[2]:encode(byteArray)
		elseif self.pick3[1] == 3 then
			byteArray.write_uint8(3)
			byteArray.write_uint16(#(self.pick3[2]))
			byteArray.write_int32_list(self.pick3[2])
		else
			error("invalid oneof")
		end
	end
	if self.ph_1 and next(self.ph_1) then
		byteArray.write_uint8(1)
		byteArray = self.ph_1:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.ph_2 and next(self.ph_2) then
		byteArray.write_uint8(1)
		byteArray = self.ph_2:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	byteArray.write_string(self.gap_k)
	if self.ph_3 and next(self.ph_3) then
		byteArray.write_uint8(1)
		byteArray = self.ph_3:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.leaf_1 and next(self.leaf_1) then
		byteArray.write_uint8(1)
		byteArray = self.leaf_1:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.leaf_2 and next(self.leaf_2) then
		byteArray.write_uint8(1)
		byteArray = self.leaf_2:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.leaf_3 and next(self.leaf_3) then
		byteArray.write_uint8(1)
		byteArray = self.leaf_3:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.node_1 and next(self.node_1) then
		byteArray.write_uint8(1)
		byteArray = self.node_1:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.node_2 and next(self.node_2) then
		byteArray.write_uint8(1)
		byteArray = self.node_2:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.node_3 and next(self.node_3) then
		byteArray.write_uint8(1)
		byteArray = self.node_3:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.wrap_1 and next(self.wrap_1) then
		byteArray.write_uint8(1)
		byteArray = self.wrap_1:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.wrap_2 and next(self.wrap_2) then
		byteArray.write_uint8(1)
		byteArray = self.wrap_2:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.wrap_3 and next(self.wrap_3) then
		byteArray.write_uint8(1)
		byteArray = self.wrap_3:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	return byteArray
end

function tRepeatAll_mt:decode(byteArray)
	self.b1 = byteArray.read_bool()
	self.b2 = byteArray.read_bool()
	self.b3 = byteArray.read_bool()
	self.i32_1 = byteArray.read_int32()
	self.i32_2 = byteArray.read_int32()
	self.i32_3 = byteArray.read_int32()
	self.gap_a = byteArray.read_string()
	self.i32_4 = byteArray.read_int32()
	self.i8_1 = byteArray.read_int8()
	self.i8_2 = byteArray.read_int8()
	self.u8_1 = byteArray.read_uint8()
	self.i8_3 = byteArray.read_int8()
	self.u8_2 = byteArray.read_uint8()
	self.u8_3 = byteArray.read_uint8()
	self.i16_1 = byteArray.read_int16()
	self.i16_2 = byteArray.read_int16()
	self.i16_3 = byteArray.read_int16()
	self.u16_1 = byteArray.read_uint16()
	self.gap_b = byteArray.read_string()
	self.u16_2 = byteArray.read_uint16()
	self.u16_3 = byteArray.read_uint16()
	self.i32_5 = byteArray.read_int32()
	self.i32_6 = byteArray.read_int32()
	self.i32_7 = byteArray.read_int32()
	self.u32_1 = byteArray.read_uint32()
	self.u32_2 = byteArray.read_uint32()
	self.u32_3 = byteArray.read_uint32()
	self.i64_1 = byteArray.read_int64()
	self.i64_2 = byteArray.read_int64()
	self.gap_c = byteArray.read_string()
	self.i64_3 = byteArray.read_int64()
	self.u64_1 = byteArray.read_uint64()
	self.u64_2 = byteArray.read_uint64()
	self.u64_3 = byteArray.read_uint64()
	self.inte_1 = byteArray.read_integer()
	self.inte_2 = byteArray.read_integer()
	self.inte_3 = byteArray.read_integer()
	self.num_1 = byteArray.read_number()
	self.gap_d = byteArray.read_string()
	self.num_2 = byteArray.read_number()
	self.num_3 = byteArray.read_number()
	self.flt_1 = byteArray.read_float()
	self.flt_2 = byteArray.read_float()
	self.flt_3 = byteArray.read_float()
	self.dbl_1 = byteArray.read_double()
	self.dbl_2 = byteArray.read_double()
	self.dbl_3 = byteArray.read_double()
	self.str_1 = byteArray.read_string()
	self.str_2 = byteArray.read_string()
	self.str_3 = byteArray.read_string()
	self.gap_e = byteArray.read_int32()
	self.str_4 = byteArray.read_string()
	self.bin_1 = byteArray.read_binary()
	self.bin_2 = byteArray.read_binary()
	self.bin_3 = byteArray.read_binary()
	local isNiluni_1 = byteArray.read_uint8()
	if isNiluni_1 > 0 then
		self.uni_1 = union()
		self.uni_1:decode(byteArray)
	else
		self.uni_1 = {}
	end
	local isNiluni_2 = byteArray.read_uint8()
	if isNiluni_2 > 0 then
		self.uni_2 = union()
		self.uni_2:decode(byteArray)
	else
		self.uni_2 = {}
	end
	local isNiluni_3 = byteArray.read_uint8()
	if isNiluni_3 > 0 then
		self.uni_3 = union()
		self.uni_3:decode(byteArray)
	else
		self.uni_3 = {}
	end
	local cntOfli32_1 = byteArray.read_uint16()
	self.li32_1 = byteArray.read_int32_list(cntOfli32_1)
	local cntOfli32_2 = byteArray.read_uint16()
	self.li32_2 = byteArray.read_int32_list(cntOfli32_2)
	local cntOfli32_3 = byteArray.read_uint16()
	self.li32_3 = byteArray.read_int32_list(cntOfli32_3)
	local cntOflstr_1 = byteArray.read_uint16()
	self.lstr_1 = {}
	for i = 1, cntOflstr_1 do
		self.lstr_1[i] = byteArray.read_string()
	end
	self.gap_f = byteArray.read_int32()
	local cntOflstr_2 = byteArray.read_uint16()
	self.lstr_2 = {}
	for i = 1, cntOflstr_2 do
		self.lstr_2[i] = byteArray.read_string()
	end
	local cntOflstr_3 = byteArray.read_uint16()
	self.lstr_3 = {}
	for i = 1, cntOflstr_3 do
		self.lstr_3[i] = byteArray.read_string()
	end
	local cntOflbin_1 = byteArray.read_uint16()
	self.lbin_1 = {}
	for i = 1, cntOflbin_1 do
		self.lbin_1[i] = byteArray.read_binary()
	end
	local cntOflbin_2 = byteArray.read_uint16()
	self.lbin_2 = {}
	for i = 1, cntOflbin_2 do
		self.lbin_2[i] = byteArray.read_binary()
	end
	local cntOflbin_3 = byteArray.read_uint16()
	self.lbin_3 = {}
	for i = 1, cntOflbin_3 do
		self.lbin_3[i] = byteArray.read_binary()
	end
	local cntOflbool_1 = byteArray.read_uint16()
	self.lbool_1 = byteArray.read_bool_list(cntOflbool_1)
	local cntOflbool_2 = byteArray.read_uint16()
	self.lbool_2 = byteArray.read_bool_list(cntOflbool_2)
	local cntOflbool_3 = byteArray.read_uint16()
	self.lbool_3 = byteArray.read_bool_list(cntOflbool_3)
	local cntOflint_1 = byteArray.read_uint16()
	self.lint_1 = {}
	for i = 1, cntOflint_1 do
		self.lint_1[i] = byteArray.read_integer()
	end
	local cntOflint_2 = byteArray.read_uint16()
	self.lint_2 = {}
	for i = 1, cntOflint_2 do
		self.lint_2[i] = byteArray.read_integer()
	end
	local cntOflint_3 = byteArray.read_uint16()
	self.lint_3 = {}
	for i = 1, cntOflint_3 do
		self.lint_3[i] = byteArray.read_integer()
	end
	local cntOflnum_1 = byteArray.read_uint16()
	self.lnum_1 = {}
	for i = 1, cntOflnum_1 do
		self.lnum_1[i] = byteArray.read_number()
	end
	local cntOflnum_2 = byteArray.read_uint16()
	self.lnum_2 = {}
	for i = 1, cntOflnum_2 do
		self.lnum_2[i] = byteArray.read_number()
	end
	local cntOflnum_3 = byteArray.read_uint16()
	self.lnum_3 = {}
	for i = 1, cntOflnum_3 do
		self.lnum_3[i] = byteArray.read_number()
	end
	local cntOflph_1 = byteArray.read_uint16()
	self.lph_1 = {}
	for i = 1, cntOflph_1 do
		local temp = phoneNumber()
			temp:decode(byteArray)
		self.lph_1[i] = temp
	end
	local cntOflph_2 = byteArray.read_uint16()
	self.lph_2 = {}
	for i = 1, cntOflph_2 do
		local temp = phoneNumber()
			temp:decode(byteArray)
		self.lph_2[i] = temp
	end
	self.gap_g = byteArray.read_string()
	local cntOflph_3 = byteArray.read_uint16()
	self.lph_3 = {}
	for i = 1, cntOflph_3 do
		local temp = phoneNumber()
			temp:decode(byteArray)
		self.lph_3[i] = temp
	end
	local cntOflleaf_1 = byteArray.read_uint16()
	self.lleaf_1 = {}
	for i = 1, cntOflleaf_1 do
		local temp = comboLeaf()
			temp:decode(byteArray)
		self.lleaf_1[i] = temp
	end
	local cntOflleaf_2 = byteArray.read_uint16()
	self.lleaf_2 = {}
	for i = 1, cntOflleaf_2 do
		local temp = comboLeaf()
			temp:decode(byteArray)
		self.lleaf_2[i] = temp
	end
	local cntOflleaf_3 = byteArray.read_uint16()
	self.lleaf_3 = {}
	for i = 1, cntOflleaf_3 do
		local temp = comboLeaf()
			temp:decode(byteArray)
		self.lleaf_3[i] = temp
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.ms_i_1 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.ms_i_2 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.ms_i_3 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.mi_s_1 = __map
	end
	self.gap_h = byteArray.read_int32()
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.mi_s_2 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.mi_s_3 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.ms_s_1 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.ms_s_2 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.ms_s_3 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.mi_i_1 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.mi_i_2 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.mi_i_3 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_binary()
			__map[__k] = __v
		end
		self.ms_b_1 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_binary()
			__map[__k] = __v
		end
		self.ms_b_2 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_binary()
			__map[__k] = __v
		end
		self.ms_b_3 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_binary()
			__map[__k] = __v
		end
		self.mi_b_1 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_binary()
			__map[__k] = __v
		end
		self.mi_b_2 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_binary()
			__map[__k] = __v
		end
		self.mi_b_3 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = phoneNumber()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.ms_p_1 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = phoneNumber()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.ms_p_2 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = phoneNumber()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.ms_p_3 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = comboLeaf()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.mi_l_1 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = comboLeaf()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.mi_l_2 = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = comboLeaf()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.mi_l_3 = __map
	end
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.pick1 = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.pick1 = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.pick1 = {2, __ov}
		elseif __tag == 3 then
			__ov = byteArray.read_binary()
			self.pick1 = {3, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.gap_i = byteArray.read_int32()
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.pick2 = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.pick2 = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.pick2 = {2, __ov}
		elseif __tag == 3 then
			__ov = phoneNumber()
			__ov:decode(byteArray)
			self.pick2 = {3, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.gap_j = byteArray.read_string()
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.pick3 = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.pick3 = {1, __ov}
		elseif __tag == 2 then
			__ov = comboLeaf()
			__ov:decode(byteArray)
			self.pick3 = {2, __ov}
		elseif __tag == 3 then
			do
				local __cnt = byteArray.read_uint16()
				__ov = byteArray.read_int32_list(__cnt)
			end
			self.pick3 = {3, __ov}
		else
			error("invalid oneof tag")
		end
	end
	local isNilph_1 = byteArray.read_uint8()
	if isNilph_1 > 0 then
		self.ph_1 = phoneNumber()
		self.ph_1:decode(byteArray)
	else
		self.ph_1 = {}
	end
	local isNilph_2 = byteArray.read_uint8()
	if isNilph_2 > 0 then
		self.ph_2 = phoneNumber()
		self.ph_2:decode(byteArray)
	else
		self.ph_2 = {}
	end
	self.gap_k = byteArray.read_string()
	local isNilph_3 = byteArray.read_uint8()
	if isNilph_3 > 0 then
		self.ph_3 = phoneNumber()
		self.ph_3:decode(byteArray)
	else
		self.ph_3 = {}
	end
	local isNilleaf_1 = byteArray.read_uint8()
	if isNilleaf_1 > 0 then
		self.leaf_1 = comboLeaf()
		self.leaf_1:decode(byteArray)
	else
		self.leaf_1 = {}
	end
	local isNilleaf_2 = byteArray.read_uint8()
	if isNilleaf_2 > 0 then
		self.leaf_2 = comboLeaf()
		self.leaf_2:decode(byteArray)
	else
		self.leaf_2 = {}
	end
	local isNilleaf_3 = byteArray.read_uint8()
	if isNilleaf_3 > 0 then
		self.leaf_3 = comboLeaf()
		self.leaf_3:decode(byteArray)
	else
		self.leaf_3 = {}
	end
	local isNilnode_1 = byteArray.read_uint8()
	if isNilnode_1 > 0 then
		self.node_1 = comboNode()
		self.node_1:decode(byteArray)
	else
		self.node_1 = {}
	end
	local isNilnode_2 = byteArray.read_uint8()
	if isNilnode_2 > 0 then
		self.node_2 = comboNode()
		self.node_2:decode(byteArray)
	else
		self.node_2 = {}
	end
	local isNilnode_3 = byteArray.read_uint8()
	if isNilnode_3 > 0 then
		self.node_3 = comboNode()
		self.node_3:decode(byteArray)
	else
		self.node_3 = {}
	end
	local isNilwrap_1 = byteArray.read_uint8()
	if isNilwrap_1 > 0 then
		self.wrap_1 = comboWrap()
		self.wrap_1:decode(byteArray)
	else
		self.wrap_1 = {}
	end
	local isNilwrap_2 = byteArray.read_uint8()
	if isNilwrap_2 > 0 then
		self.wrap_2 = comboWrap()
		self.wrap_2:decode(byteArray)
	else
		self.wrap_2 = {}
	end
	local isNilwrap_3 = byteArray.read_uint8()
	if isNilwrap_3 > 0 then
		self.wrap_3 = comboWrap()
		self.wrap_3:decode(byteArray)
	else
		self.wrap_3 = {}
	end
end

function tRepeatAll_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(2012)
	self:encode(byteArray)
	return byteArray.getBytes()
end

benchLeaf_mt = {}
benchLeaf_mt.__index = benchLeaf_mt

function benchLeaf()
	return setmetatable({
		msgId = 3001,
		id = 0,
		tag = "",
		blob = "",
	}, benchLeaf_mt)
end

function benchLeaf_mt:encode(byteArray)
	byteArray.write_int32(self.id)
	byteArray.write_string(self.tag)
	byteArray.write_binary(self.blob)
	return byteArray
end

function benchLeaf_mt:decode(byteArray)
	self.id = byteArray.read_int32()
	self.tag = byteArray.read_string()
	self.blob = byteArray.read_binary()
end

function benchLeaf_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(3001)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tBenchScalar_mt = {}
tBenchScalar_mt.__index = tBenchScalar_mt

function tBenchScalar()
	return setmetatable({
		msgId = 3002,
		a1 = 0,
		a2 = 0,
		a3 = 0,
		a4 = 0,
		a5 = 0,
		a6 = 0,
		a7 = 0,
		a8 = 0,
		b1 = 0,
		b2 = 0,
		b3 = 0,
		b4 = 0,
		s1 = "",
		s2 = "",
		s3 = "",
		s4 = "",
		p1 = "",
		p2 = "",
		p3 = "",
		p4 = "",
		f1 = false,
		f2 = false,
		f3 = 0,
		f4 = 0,
	}, tBenchScalar_mt)
end

function tBenchScalar_mt:encode(byteArray)
	byteArray.write_int32(self.a1)
	byteArray.write_int32(self.a2)
	byteArray.write_int32(self.a3)
	byteArray.write_int32(self.a4)
	byteArray.write_int32(self.a5)
	byteArray.write_int32(self.a6)
	byteArray.write_int32(self.a7)
	byteArray.write_int32(self.a8)
	byteArray.write_int64(self.b1)
	byteArray.write_int64(self.b2)
	byteArray.write_int64(self.b3)
	byteArray.write_int64(self.b4)
	byteArray.write_string(self.s1)
	byteArray.write_string(self.s2)
	byteArray.write_string(self.s3)
	byteArray.write_string(self.s4)
	byteArray.write_binary(self.p1)
	byteArray.write_binary(self.p2)
	byteArray.write_binary(self.p3)
	byteArray.write_binary(self.p4)
	byteArray.write_bool(self.f1)
	byteArray.write_bool(self.f2)
	byteArray.write_float(self.f3)
	byteArray.write_double(self.f4)
	return byteArray
end

function tBenchScalar_mt:decode(byteArray)
	self.a1 = byteArray.read_int32()
	self.a2 = byteArray.read_int32()
	self.a3 = byteArray.read_int32()
	self.a4 = byteArray.read_int32()
	self.a5 = byteArray.read_int32()
	self.a6 = byteArray.read_int32()
	self.a7 = byteArray.read_int32()
	self.a8 = byteArray.read_int32()
	self.b1 = byteArray.read_int64()
	self.b2 = byteArray.read_int64()
	self.b3 = byteArray.read_int64()
	self.b4 = byteArray.read_int64()
	self.s1 = byteArray.read_string()
	self.s2 = byteArray.read_string()
	self.s3 = byteArray.read_string()
	self.s4 = byteArray.read_string()
	self.p1 = byteArray.read_binary()
	self.p2 = byteArray.read_binary()
	self.p3 = byteArray.read_binary()
	self.p4 = byteArray.read_binary()
	self.f1 = byteArray.read_bool()
	self.f2 = byteArray.read_bool()
	self.f3 = byteArray.read_float()
	self.f4 = byteArray.read_double()
end

function tBenchScalar_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(3002)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tBenchList_mt = {}
tBenchList_mt.__index = tBenchList_mt

function tBenchList()
	return setmetatable({
		msgId = 3003,
		scores = {},
		labels = {},
		chunks = {},
		items = {},
		extras = {},
	}, tBenchList_mt)
end

function tBenchList_mt:encode(byteArray)
	byteArray.write_uint16(#(self.scores))
	byteArray.write_int32_list(self.scores)
	byteArray.write_uint16(#(self.labels))
	for _, v in ipairs(self.labels) do
		byteArray.write_string(v)
	end
	byteArray.write_uint16(#(self.chunks))
	for _, v in ipairs(self.chunks) do
		byteArray.write_binary(v)
	end
	byteArray.write_uint16(#(self.items))
	for _, v in ipairs(self.items) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.extras))
	for _, v in ipairs(self.extras) do
		byteArray = v:encode(byteArray)
	end
	return byteArray
end

function tBenchList_mt:decode(byteArray)
	local cntOfscores = byteArray.read_uint16()
	self.scores = byteArray.read_int32_list(cntOfscores)
	local cntOflabels = byteArray.read_uint16()
	self.labels = {}
	for i = 1, cntOflabels do
		self.labels[i] = byteArray.read_string()
	end
	local cntOfchunks = byteArray.read_uint16()
	self.chunks = {}
	for i = 1, cntOfchunks do
		self.chunks[i] = byteArray.read_binary()
	end
	local cntOfitems = byteArray.read_uint16()
	self.items = {}
	for i = 1, cntOfitems do
		local temp = benchLeaf()
			temp:decode(byteArray)
		self.items[i] = temp
	end
	local cntOfextras = byteArray.read_uint16()
	self.extras = {}
	for i = 1, cntOfextras do
		local temp = comboLeaf()
			temp:decode(byteArray)
		self.extras[i] = temp
	end
end

function tBenchList_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(3003)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tBenchMap_mt = {}
tBenchMap_mt.__index = tBenchMap_mt

function tBenchMap()
	return setmetatable({
		msgId = 3004,
		attrs = nil,
		rev = nil,
		lookup = nil,
		phones = nil,
		tags = nil,
	}, tBenchMap_mt)
end

function tBenchMap_mt:encode(byteArray)
	do
		local __map = self.attrs or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.attrs[__k])
		end
	end
	do
		local __map = self.rev or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_string(self.rev[__k])
		end
	end
	do
		local __map = self.lookup or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray = self.lookup[__k]:encode(byteArray)
		end
	end
	do
		local __map = self.phones or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray = self.phones[__k]:encode(byteArray)
		end
	end
	do
		local __map = self.tags or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_string(self.tags[__k])
		end
	end
	return byteArray
end

function tBenchMap_mt:decode(byteArray)
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.attrs = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.rev = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = benchLeaf()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.lookup = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = phoneNumber()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.phones = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.tags = __map
	end
end

function tBenchMap_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(3004)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tBenchOneof_mt = {}
tBenchOneof_mt.__index = tBenchOneof_mt

function tBenchOneof()
	return setmetatable({
		msgId = 3005,
		seq = 0,
		reward = nil,
		meta = nil,
		payload = nil,
		tip = "",
	}, tBenchOneof_mt)
end

function tBenchOneof_mt:encode(byteArray)
	byteArray.write_int32(self.seq)
	if not self.reward then
		byteArray.write_uint8(0)
	else
		if self.reward[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.reward[2])
		elseif self.reward[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.reward[2])
		elseif self.reward[1] == 3 then
			byteArray.write_uint8(3)
			byteArray = self.reward[2]:encode(byteArray)
		elseif self.reward[1] == 4 then
			byteArray.write_uint8(4)
			byteArray = self.reward[2]:encode(byteArray)
		else
			error("invalid oneof")
		end
	end
	if not self.meta then
		byteArray.write_uint8(0)
	else
		if self.meta[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.meta[2])
		elseif self.meta[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.meta[2])
		else
			error("invalid oneof")
		end
	end
	if not self.payload then
		byteArray.write_uint8(0)
	else
		if self.payload[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_binary(self.payload[2])
		elseif self.payload[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.payload[2])
		elseif self.payload[1] == 3 then
			byteArray.write_uint8(3)
			byteArray = self.payload[2]:encode(byteArray)
		else
			error("invalid oneof")
		end
	end
	byteArray.write_string(self.tip)
	return byteArray
end

function tBenchOneof_mt:decode(byteArray)
	self.seq = byteArray.read_int32()
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.reward = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.reward = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.reward = {2, __ov}
		elseif __tag == 3 then
			__ov = benchLeaf()
			__ov:decode(byteArray)
			self.reward = {3, __ov}
		elseif __tag == 4 then
			__ov = phoneNumber()
			__ov:decode(byteArray)
			self.reward = {4, __ov}
		else
			error("invalid oneof tag")
		end
	end
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.meta = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.meta = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.meta = {2, __ov}
		else
			error("invalid oneof tag")
		end
	end
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.payload = nil
		elseif __tag == 1 then
			__ov = byteArray.read_binary()
			self.payload = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.payload = {2, __ov}
		elseif __tag == 3 then
			__ov = benchLeaf()
			__ov:decode(byteArray)
			self.payload = {3, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.tip = byteArray.read_string()
end

function tBenchOneof_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(3005)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tBenchNested_mt = {}
tBenchNested_mt.__index = tBenchNested_mt

function tBenchNested()
	return setmetatable({
		msgId = 3006,
		root = {},
		contact = {},
		leaf = {},
		children = {},
		contacts = {},
	}, tBenchNested_mt)
end

function tBenchNested_mt:encode(byteArray)
	if self.root and next(self.root) then
		byteArray.write_uint8(1)
		byteArray = self.root:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.contact and next(self.contact) then
		byteArray.write_uint8(1)
		byteArray = self.contact:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	if self.leaf and next(self.leaf) then
		byteArray.write_uint8(1)
		byteArray = self.leaf:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	byteArray.write_uint16(#(self.children))
	for _, v in ipairs(self.children) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.contacts))
	for _, v in ipairs(self.contacts) do
		byteArray = v:encode(byteArray)
	end
	return byteArray
end

function tBenchNested_mt:decode(byteArray)
	local isNilroot = byteArray.read_uint8()
	if isNilroot > 0 then
		self.root = benchLeaf()
		self.root:decode(byteArray)
	else
		self.root = {}
	end
	local isNilcontact = byteArray.read_uint8()
	if isNilcontact > 0 then
		self.contact = phoneNumber()
		self.contact:decode(byteArray)
	else
		self.contact = {}
	end
	local isNilleaf = byteArray.read_uint8()
	if isNilleaf > 0 then
		self.leaf = comboLeaf()
		self.leaf:decode(byteArray)
	else
		self.leaf = {}
	end
	local cntOfchildren = byteArray.read_uint16()
	self.children = {}
	for i = 1, cntOfchildren do
		local temp = benchLeaf()
			temp:decode(byteArray)
		self.children[i] = temp
	end
	local cntOfcontacts = byteArray.read_uint16()
	self.contacts = {}
	for i = 1, cntOfcontacts do
		local temp = phoneNumber()
			temp:decode(byteArray)
		self.contacts[i] = temp
	end
end

function tBenchNested_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(3006)
	self:encode(byteArray)
	return byteArray.getBytes()
end

tBenchHeavy_mt = {}
tBenchHeavy_mt.__index = tBenchHeavy_mt

function tBenchHeavy()
	return setmetatable({
		msgId = 3007,
		seq = 0,
		title = "",
		root = {},
		items = {},
		scores = {},
		labels = {},
		chunks = {},
		attrs = nil,
		rev = nil,
		lookup = nil,
		phones = nil,
		reward = nil,
		meta = nil,
		payload = "",
		extras = {},
	}, tBenchHeavy_mt)
end

function tBenchHeavy_mt:encode(byteArray)
	byteArray.write_int32(self.seq)
	byteArray.write_string(self.title)
	if self.root and next(self.root) then
		byteArray.write_uint8(1)
		byteArray = self.root:encode(byteArray)
	else
		byteArray.write_uint8(0)
	end
	byteArray.write_uint16(#(self.items))
	for _, v in ipairs(self.items) do
		byteArray = v:encode(byteArray)
	end
	byteArray.write_uint16(#(self.scores))
	byteArray.write_int32_list(self.scores)
	byteArray.write_uint16(#(self.labels))
	for _, v in ipairs(self.labels) do
		byteArray.write_string(v)
	end
	byteArray.write_uint16(#(self.chunks))
	for _, v in ipairs(self.chunks) do
		byteArray.write_binary(v)
	end
	do
		local __map = self.attrs or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray.write_int32(self.attrs[__k])
		end
	end
	do
		local __map = self.rev or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_int32(__k)
			byteArray.write_string(self.rev[__k])
		end
	end
	do
		local __map = self.lookup or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray = self.lookup[__k]:encode(byteArray)
		end
	end
	do
		local __map = self.phones or {}
		local __keys = {}
		local __n = 0
		for __k in pairs(__map) do __n = __n + 1; __keys[__n] = __k end
		byteArray.write_uint16(__n)
		for __i = 1, __n do
			local __k = __keys[__i]
			byteArray.write_string(__k)
			byteArray = self.phones[__k]:encode(byteArray)
		end
	end
	if not self.reward then
		byteArray.write_uint8(0)
	else
		if self.reward[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.reward[2])
		elseif self.reward[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.reward[2])
		elseif self.reward[1] == 3 then
			byteArray.write_uint8(3)
			byteArray = self.reward[2]:encode(byteArray)
		elseif self.reward[1] == 4 then
			byteArray.write_uint8(4)
			byteArray = self.reward[2]:encode(byteArray)
		else
			error("invalid oneof")
		end
	end
	if not self.meta then
		byteArray.write_uint8(0)
	else
		if self.meta[1] == 1 then
			byteArray.write_uint8(1)
			byteArray.write_int32(self.meta[2])
		elseif self.meta[1] == 2 then
			byteArray.write_uint8(2)
			byteArray.write_string(self.meta[2])
		else
			error("invalid oneof")
		end
	end
	byteArray.write_binary(self.payload)
	byteArray.write_uint16(#(self.extras))
	for _, v in ipairs(self.extras) do
		byteArray = v:encode(byteArray)
	end
	return byteArray
end

function tBenchHeavy_mt:decode(byteArray)
	self.seq = byteArray.read_int32()
	self.title = byteArray.read_string()
	local isNilroot = byteArray.read_uint8()
	if isNilroot > 0 then
		self.root = benchLeaf()
		self.root:decode(byteArray)
	else
		self.root = {}
	end
	local cntOfitems = byteArray.read_uint16()
	self.items = {}
	for i = 1, cntOfitems do
		local temp = benchLeaf()
			temp:decode(byteArray)
		self.items[i] = temp
	end
	local cntOfscores = byteArray.read_uint16()
	self.scores = byteArray.read_int32_list(cntOfscores)
	local cntOflabels = byteArray.read_uint16()
	self.labels = {}
	for i = 1, cntOflabels do
		self.labels[i] = byteArray.read_string()
	end
	local cntOfchunks = byteArray.read_uint16()
	self.chunks = {}
	for i = 1, cntOfchunks do
		self.chunks[i] = byteArray.read_binary()
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = byteArray.read_int32()
			__map[__k] = __v
		end
		self.attrs = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_int32()
			__v = byteArray.read_string()
			__map[__k] = __v
		end
		self.rev = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = benchLeaf()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.lookup = __map
	end
	do
		local __cnt = byteArray.read_uint16()
		local __map = {}
		for __i = 1, __cnt do
			__k = byteArray.read_string()
			__v = phoneNumber()
			__v:decode(byteArray)
			__map[__k] = __v
		end
		self.phones = __map
	end
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.reward = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.reward = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.reward = {2, __ov}
		elseif __tag == 3 then
			__ov = benchLeaf()
			__ov:decode(byteArray)
			self.reward = {3, __ov}
		elseif __tag == 4 then
			__ov = phoneNumber()
			__ov:decode(byteArray)
			self.reward = {4, __ov}
		else
			error("invalid oneof tag")
		end
	end
	do
		local __tag = byteArray.read_uint8()
		if __tag == 0 then
			self.meta = nil
		elseif __tag == 1 then
			__ov = byteArray.read_int32()
			self.meta = {1, __ov}
		elseif __tag == 2 then
			__ov = byteArray.read_string()
			self.meta = {2, __ov}
		else
			error("invalid oneof tag")
		end
	end
	self.payload = byteArray.read_binary()
	local cntOfextras = byteArray.read_uint16()
	self.extras = {}
	for i = 1, cntOfextras do
		local temp = comboLeaf()
			temp:decode(byteArray)
		self.extras[i] = temp
	end
end

function tBenchHeavy_mt:build(byteArray)
	byteArray.reset()
	byteArray.write_uint16(3007)
	self:encode(byteArray)
	return byteArray.getBytes()
end

