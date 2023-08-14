function test()
	local tb = {}
	tb.msgId =1
	t.aa = ""

	tb.encode = function(byteArray)
		byteArray.write_string(tb.aa)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.aa = byteArray.read_string()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(1)
		return tb.encode(byteArray)
	end

	return tb
end

function phoneNumber()
	local tb = {}
	tb.msgId =2
	t.number = {}
	t.type = 0

	tb.encode = function(byteArray)
		if tb.number and next(tb.number) then
			byteArray.write_uint8(1)
			tb.number.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		byteArray.write_int32(tb.type)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local isNilnumber = byteArray.read_uint8()
		if isNilnumber > 0 then
			tb.number = test()
			tb.number.decode(byteArray)
		else
			tb.number = {}
		end
		tb.type = byteArray.read_int32()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(2)
		return tb.encode(byteArray)
	end

	return tb
end

function person()
	local tb = {}
	tb.msgId =3
	t.name = ""
	t.id = 0
	t.email = ""
	t.phone = {}

	tb.encode = function(byteArray)
		byteArray.write_string(tb.name)
		byteArray.write_int32(tb.id)
		byteArray.write_string(tb.email)
		byteArray.write_uint16(#(tb.phone))
		for k, v in pairs(tb.phone) do
			byteArray = v.encode(byteArray)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.name = byteArray.read_string()
		tb.id = byteArray.read_int32()
		tb.email = byteArray.read_string()
		local cntOfphone = byteArray.read_uint16()
		tb.phone = {}
		for i = 1, cntOfphone do
			local temp = phoneNumber()
			temp.decode(byteArray)
			table.insert(tb.phone, temp)
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(3)
		return tb.encode(byteArray)
	end

	return tb
end

function addressBook()
	local tb = {}
	tb.msgId =4
	t.person = {}
	t.other = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.person))
		for k, v in pairs(tb.person) do
			byteArray = v.encode(byteArray)
		end
		byteArray.write_uint16(#(tb.other))
		for k, v in pairs(tb.other) do
			byteArray = v.encode(byteArray)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfperson = byteArray.read_uint16()
		tb.person = {}
		for i = 1, cntOfperson do
			local temp = person()
			temp.decode(byteArray)
			table.insert(tb.person, temp)
		end
		local cntOfother = byteArray.read_uint16()
		tb.other = {}
		for i = 1, cntOfother do
			local temp = person()
			temp.decode(byteArray)
			table.insert(tb.other, temp)
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(4)
		return tb.encode(byteArray)
	end

	return tb
end

function union()
	local tb = {}
	tb.msgId =5
	t.test = ""
	t.type = 0

	tb.encode = function(byteArray)
		byteArray.write_string(tb.test)
		byteArray.write_int32(tb.type)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.test = byteArray.read_string()
		tb.type = byteArray.read_int32()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(5)
		return tb.encode(byteArray)
	end

	return tb
end

function tbool()
	local tb = {}
	tb.msgId =6
	t.bool = false

	tb.encode = function(byteArray)
		byteArray.write_bool(tb.bool)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.bool = byteArray.read_bool()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(6)
		return tb.encode(byteArray)
	end

	return tb
end

function tint8()
	local tb = {}
	tb.msgId =7
	t.int1 = 0
	t.int2 = 0

	tb.encode = function(byteArray)
		byteArray.write_int8(tb.int1)
		byteArray.write_int8(tb.int2)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_int8()
		tb.int2 = byteArray.read_int8()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(7)
		return tb.encode(byteArray)
	end

	return tb
end

function tuint8()
	local tb = {}
	tb.msgId =8
	t.int1 = 0
	t.int2 = 0

	tb.encode = function(byteArray)
		byteArray.write_uint8(tb.int1)
		byteArray.write_uint8(tb.int2)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_uint8()
		tb.int2 = byteArray.read_uint8()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(8)
		return tb.encode(byteArray)
	end

	return tb
end

function tint16()
	local tb = {}
	tb.msgId =9
	t.int1 = 0
	t.int2 = 0

	tb.encode = function(byteArray)
		byteArray.write_int16(tb.int1)
		byteArray.write_int16(tb.int2)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_int16()
		tb.int2 = byteArray.read_int16()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(9)
		return tb.encode(byteArray)
	end

	return tb
end

function tuint16()
	local tb = {}
	tb.msgId =10
	t.int1 = 0
	t.int2 = 0

	tb.encode = function(byteArray)
		byteArray.write_uint16(tb.int1)
		byteArray.write_uint16(tb.int2)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_uint16()
		tb.int2 = byteArray.read_uint16()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(10)
		return tb.encode(byteArray)
	end

	return tb
end

function tint32()
	local tb = {}
	tb.msgId =11
	t.int1 = 0
	t.int2 = 0
	t.int3 = 0
	t.int4 = 0
	t.int5 = 0
	t.int6 = 0
	t.int7 = 0
	t.int8 = 0
	t.int9 = 0
	t.int10 = 0

	tb.encode = function(byteArray)
		byteArray.write_int32(tb.int1)
		byteArray.write_int32(tb.int2)
		byteArray.write_int32(tb.int3)
		byteArray.write_int32(tb.int4)
		byteArray.write_int32(tb.int5)
		byteArray.write_int32(tb.int6)
		byteArray.write_int32(tb.int7)
		byteArray.write_int32(tb.int8)
		byteArray.write_int32(tb.int9)
		byteArray.write_int32(tb.int10)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_int32()
		tb.int2 = byteArray.read_int32()
		tb.int3 = byteArray.read_int32()
		tb.int4 = byteArray.read_int32()
		tb.int5 = byteArray.read_int32()
		tb.int6 = byteArray.read_int32()
		tb.int7 = byteArray.read_int32()
		tb.int8 = byteArray.read_int32()
		tb.int9 = byteArray.read_int32()
		tb.int10 = byteArray.read_int32()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(11)
		return tb.encode(byteArray)
	end

	return tb
end

function tuint32()
	local tb = {}
	tb.msgId =12
	t.int1 = 0
	t.int2 = 0

	tb.encode = function(byteArray)
		byteArray.write_uint32(tb.int1)
		byteArray.write_uint32(tb.int2)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_uint32()
		tb.int2 = byteArray.read_uint32()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(12)
		return tb.encode(byteArray)
	end

	return tb
end

function tint64()
	local tb = {}
	tb.msgId =13
	t.int1 = 0
	t.int2 = 0

	tb.encode = function(byteArray)
		byteArray.write_int64(tb.int1)
		byteArray.write_int64(tb.int2)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_int64()
		tb.int2 = byteArray.read_int64()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(13)
		return tb.encode(byteArray)
	end

	return tb
end

function tuint64()
	local tb = {}
	tb.msgId =14
	t.int1 = 0
	t.int2 = 0

	tb.encode = function(byteArray)
		byteArray.write_uint64(tb.int1)
		byteArray.write_uint64(tb.int2)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_uint64()
		tb.int2 = byteArray.read_uint64()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(14)
		return tb.encode(byteArray)
	end

	return tb
end

function tinteger()
	local tb = {}
	tb.msgId =15
	t.int1 = 0
	t.int2 = 0
	t.int3 = 0
	t.int4 = 0
	t.int5 = 0
	t.int6 = 0
	t.int7 = 0
	t.int8 = 0

	tb.encode = function(byteArray)
		byteArray.write_integer(tb.int1)
		byteArray.write_integer(tb.int2)
		byteArray.write_integer(tb.int3)
		byteArray.write_integer(tb.int4)
		byteArray.write_integer(tb.int5)
		byteArray.write_integer(tb.int6)
		byteArray.write_integer(tb.int7)
		byteArray.write_integer(tb.int8)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_integer()
		tb.int2 = byteArray.read_integer()
		tb.int3 = byteArray.read_integer()
		tb.int4 = byteArray.read_integer()
		tb.int5 = byteArray.read_integer()
		tb.int6 = byteArray.read_integer()
		tb.int7 = byteArray.read_integer()
		tb.int8 = byteArray.read_integer()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(15)
		return tb.encode(byteArray)
	end

	return tb
end

function tnumber()
	local tb = {}
	tb.msgId =16
	t.int1 = 0
	t.int2 = 0
	t.int3 = 0
	t.int4 = 0
	t.int5 = 0
	t.int6 = 0
	t.int7 = 0
	t.int8 = 0
	t.float1 = 0
	t.float2 = 0

	tb.encode = function(byteArray)
		byteArray.write_number(tb.int1)
		byteArray.write_number(tb.int2)
		byteArray.write_number(tb.int3)
		byteArray.write_number(tb.int4)
		byteArray.write_number(tb.int5)
		byteArray.write_number(tb.int6)
		byteArray.write_number(tb.int7)
		byteArray.write_number(tb.int8)
		byteArray.write_number(tb.float1)
		byteArray.write_number(tb.float2)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_number()
		tb.int2 = byteArray.read_number()
		tb.int3 = byteArray.read_number()
		tb.int4 = byteArray.read_number()
		tb.int5 = byteArray.read_number()
		tb.int6 = byteArray.read_number()
		tb.int7 = byteArray.read_number()
		tb.int8 = byteArray.read_number()
		tb.float1 = byteArray.read_number()
		tb.float2 = byteArray.read_number()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(16)
		return tb.encode(byteArray)
	end

	return tb
end

function tfloat()
	local tb = {}
	tb.msgId =17
	t.int1 = 0
	t.int2 = 0

	tb.encode = function(byteArray)
		byteArray.write_float(tb.int1)
		byteArray.write_float(tb.int2)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_float()
		tb.int2 = byteArray.read_float()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(17)
		return tb.encode(byteArray)
	end

	return tb
end

function tdouble()
	local tb = {}
	tb.msgId =18
	t.int1 = 0
	t.int2 = 0

	tb.encode = function(byteArray)
		byteArray.write_double(tb.int1)
		byteArray.write_double(tb.int2)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_double()
		tb.int2 = byteArray.read_double()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(18)
		return tb.encode(byteArray)
	end

	return tb
end

function tstring()
	local tb = {}
	tb.msgId =19
	t.int1 = ""
	t.int2 = ""

	tb.encode = function(byteArray)
		byteArray.write_string(tb.int1)
		byteArray.write_string(tb.int2)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_string()
		tb.int2 = byteArray.read_string()
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(19)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistbool()
	local tb = {}
	tb.msgId =20
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_bool(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_bool())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(20)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistint8()
	local tb = {}
	tb.msgId =21
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_int8(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_int8())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(21)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistuint8()
	local tb = {}
	tb.msgId =22
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_uint8(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_uint8())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(22)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistint16()
	local tb = {}
	tb.msgId =23
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_int16(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_int16())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(23)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistuint16()
	local tb = {}
	tb.msgId =24
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_uint16(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_uint16())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(24)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistint32()
	local tb = {}
	tb.msgId =25
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_int32(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_int32())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(25)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistuint32()
	local tb = {}
	tb.msgId =26
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_uint32(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_uint32())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(26)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistint64()
	local tb = {}
	tb.msgId =27
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_int64(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_int64())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(27)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistuint64()
	local tb = {}
	tb.msgId =28
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_uint64(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_uint64())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(28)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistinteger()
	local tb = {}
	tb.msgId =29
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_integer(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_integer())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(29)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistnumber()
	local tb = {}
	tb.msgId =30
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_number(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_number())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(30)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistfloat()
	local tb = {}
	tb.msgId =31
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_float(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_float())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(31)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistdouble()
	local tb = {}
	tb.msgId =32
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_double(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_double())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(32)
		return tb.encode(byteArray)
	end

	return tb
end

function tliststring()
	local tb = {}
	tb.msgId =33
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs (tb.int1) do
			byteArray.write_string(v)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_string())
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(33)
		return tb.encode(byteArray)
	end

	return tb
end

function tlistunion()
	local tb = {}
	tb.msgId =34
	t.int1 = {}

	tb.encode = function(byteArray)
		byteArray.write_uint16(#(tb.int1))
		for k, v in pairs(tb.int1) do
			byteArray = v.encode(byteArray)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			local temp = union()
			temp.decode(byteArray)
			table.insert(tb.int1, temp)
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(34)
		return tb.encode(byteArray)
	end

	return tb
end

function allType()
	local tb = {}
	tb.msgId =35
	t.bool = false
	t.int8 = 0
	t.uint8 = 0
	t.int16 = 0
	t.uint16 = 0
	t.int32 = 0
	t.uint32 = 0
	t.int64 = 0
	t.uint64 = 0
	t.inte8 = 0
	t.uinte8 = 0
	t.inte16 = 0
	t.uinte16 = 0
	t.inte32 = 0
	t.uinte32 = 0
	t.inte64 = 0
	t.uinte64 = 0
	t.num8 = 0
	t.unum8 = 0
	t.num16 = 0
	t.unum16 = 0
	t.num32 = 0
	t.unum32 = 0
	t.num64 = 0
	t.unum64 = 0
	t.numfloat = 0
	t.numdouble = 0
	t.float = 0
	t.double = 0
	t.string1 = ""
	t.string2 = ""
	t.union = {}
	t.lbool = {}
	t.lint8 = {}
	t.luint8 = {}
	t.lint16 = {}
	t.luint16 = {}
	t.lint32 = {}
	t.luint32 = {}
	t.lint64 = {}
	t.luint64 = {}
	t.linte8 = {}
	t.linte16 = {}
	t.linte32 = {}
	t.linte64 = {}
	t.lnum8 = {}
	t.lnum16 = {}
	t.lnum32 = {}
	t.lnum64 = {}
	t.lnfloat32 = {}
	t.lnfloat64 = {}
	t.lfloat = {}
	t.ldouble = {}
	t.lstring = {}
	t.lunion = {}

	tb.encode = function(byteArray)
		byteArray.write_bool(tb.bool)
		byteArray.write_int8(tb.int8)
		byteArray.write_uint8(tb.uint8)
		byteArray.write_int16(tb.int16)
		byteArray.write_uint16(tb.uint16)
		byteArray.write_int32(tb.int32)
		byteArray.write_uint32(tb.uint32)
		byteArray.write_int64(tb.int64)
		byteArray.write_uint64(tb.uint64)
		byteArray.write_integer(tb.inte8)
		byteArray.write_integer(tb.uinte8)
		byteArray.write_integer(tb.inte16)
		byteArray.write_integer(tb.uinte16)
		byteArray.write_integer(tb.inte32)
		byteArray.write_integer(tb.uinte32)
		byteArray.write_integer(tb.inte64)
		byteArray.write_integer(tb.uinte64)
		byteArray.write_number(tb.num8)
		byteArray.write_number(tb.unum8)
		byteArray.write_number(tb.num16)
		byteArray.write_number(tb.unum16)
		byteArray.write_number(tb.num32)
		byteArray.write_number(tb.unum32)
		byteArray.write_number(tb.num64)
		byteArray.write_number(tb.unum64)
		byteArray.write_number(tb.numfloat)
		byteArray.write_number(tb.numdouble)
		byteArray.write_float(tb.float)
		byteArray.write_double(tb.double)
		byteArray.write_string(tb.string1)
		byteArray.write_string(tb.string2)
		if tb.union and next(tb.union) then
			byteArray.write_uint8(1)
			tb.union.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		byteArray.write_uint16(#(tb.lbool))
		for k, v in pairs (tb.lbool) do
			byteArray.write_bool(v)
		end
		byteArray.write_uint16(#(tb.lint8))
		for k, v in pairs (tb.lint8) do
			byteArray.write_int8(v)
		end
		byteArray.write_uint16(#(tb.luint8))
		for k, v in pairs (tb.luint8) do
			byteArray.write_uint8(v)
		end
		byteArray.write_uint16(#(tb.lint16))
		for k, v in pairs (tb.lint16) do
			byteArray.write_int16(v)
		end
		byteArray.write_uint16(#(tb.luint16))
		for k, v in pairs (tb.luint16) do
			byteArray.write_uint16(v)
		end
		byteArray.write_uint16(#(tb.lint32))
		for k, v in pairs (tb.lint32) do
			byteArray.write_int32(v)
		end
		byteArray.write_uint16(#(tb.luint32))
		for k, v in pairs (tb.luint32) do
			byteArray.write_uint32(v)
		end
		byteArray.write_uint16(#(tb.lint64))
		for k, v in pairs (tb.lint64) do
			byteArray.write_int64(v)
		end
		byteArray.write_uint16(#(tb.luint64))
		for k, v in pairs (tb.luint64) do
			byteArray.write_uint64(v)
		end
		byteArray.write_uint16(#(tb.linte8))
		for k, v in pairs (tb.linte8) do
			byteArray.write_integer(v)
		end
		byteArray.write_uint16(#(tb.linte16))
		for k, v in pairs (tb.linte16) do
			byteArray.write_integer(v)
		end
		byteArray.write_uint16(#(tb.linte32))
		for k, v in pairs (tb.linte32) do
			byteArray.write_integer(v)
		end
		byteArray.write_uint16(#(tb.linte64))
		for k, v in pairs (tb.linte64) do
			byteArray.write_integer(v)
		end
		byteArray.write_uint16(#(tb.lnum8))
		for k, v in pairs (tb.lnum8) do
			byteArray.write_number(v)
		end
		byteArray.write_uint16(#(tb.lnum16))
		for k, v in pairs (tb.lnum16) do
			byteArray.write_number(v)
		end
		byteArray.write_uint16(#(tb.lnum32))
		for k, v in pairs (tb.lnum32) do
			byteArray.write_number(v)
		end
		byteArray.write_uint16(#(tb.lnum64))
		for k, v in pairs (tb.lnum64) do
			byteArray.write_number(v)
		end
		byteArray.write_uint16(#(tb.lnfloat32))
		for k, v in pairs (tb.lnfloat32) do
			byteArray.write_number(v)
		end
		byteArray.write_uint16(#(tb.lnfloat64))
		for k, v in pairs (tb.lnfloat64) do
			byteArray.write_number(v)
		end
		byteArray.write_uint16(#(tb.lfloat))
		for k, v in pairs (tb.lfloat) do
			byteArray.write_float(v)
		end
		byteArray.write_uint16(#(tb.ldouble))
		for k, v in pairs (tb.ldouble) do
			byteArray.write_double(v)
		end
		byteArray.write_uint16(#(tb.lstring))
		for k, v in pairs (tb.lstring) do
			byteArray.write_string(v)
		end
		byteArray.write_uint16(#(tb.lunion))
		for k, v in pairs(tb.lunion) do
			byteArray = v.encode(byteArray)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.bool = byteArray.read_bool()
		tb.int8 = byteArray.read_int8()
		tb.uint8 = byteArray.read_uint8()
		tb.int16 = byteArray.read_int16()
		tb.uint16 = byteArray.read_uint16()
		tb.int32 = byteArray.read_int32()
		tb.uint32 = byteArray.read_uint32()
		tb.int64 = byteArray.read_int64()
		tb.uint64 = byteArray.read_uint64()
		tb.inte8 = byteArray.read_integer()
		tb.uinte8 = byteArray.read_integer()
		tb.inte16 = byteArray.read_integer()
		tb.uinte16 = byteArray.read_integer()
		tb.inte32 = byteArray.read_integer()
		tb.uinte32 = byteArray.read_integer()
		tb.inte64 = byteArray.read_integer()
		tb.uinte64 = byteArray.read_integer()
		tb.num8 = byteArray.read_number()
		tb.unum8 = byteArray.read_number()
		tb.num16 = byteArray.read_number()
		tb.unum16 = byteArray.read_number()
		tb.num32 = byteArray.read_number()
		tb.unum32 = byteArray.read_number()
		tb.num64 = byteArray.read_number()
		tb.unum64 = byteArray.read_number()
		tb.numfloat = byteArray.read_number()
		tb.numdouble = byteArray.read_number()
		tb.float = byteArray.read_float()
		tb.double = byteArray.read_double()
		tb.string1 = byteArray.read_string()
		tb.string2 = byteArray.read_string()
		local isNilunion = byteArray.read_uint8()
		if isNilunion > 0 then
			tb.union = union()
			tb.union.decode(byteArray)
		else
			tb.union = {}
		end
		local cntOflbool = byteArray.read_uint16()
		tb.lbool = {}
		for i = 1, cntOflbool do
			table.insert(tb.lbool, byteArray.read_bool())
		end
		local cntOflint8 = byteArray.read_uint16()
		tb.lint8 = {}
		for i = 1, cntOflint8 do
			table.insert(tb.lint8, byteArray.read_int8())
		end
		local cntOfluint8 = byteArray.read_uint16()
		tb.luint8 = {}
		for i = 1, cntOfluint8 do
			table.insert(tb.luint8, byteArray.read_uint8())
		end
		local cntOflint16 = byteArray.read_uint16()
		tb.lint16 = {}
		for i = 1, cntOflint16 do
			table.insert(tb.lint16, byteArray.read_int16())
		end
		local cntOfluint16 = byteArray.read_uint16()
		tb.luint16 = {}
		for i = 1, cntOfluint16 do
			table.insert(tb.luint16, byteArray.read_uint16())
		end
		local cntOflint32 = byteArray.read_uint16()
		tb.lint32 = {}
		for i = 1, cntOflint32 do
			table.insert(tb.lint32, byteArray.read_int32())
		end
		local cntOfluint32 = byteArray.read_uint16()
		tb.luint32 = {}
		for i = 1, cntOfluint32 do
			table.insert(tb.luint32, byteArray.read_uint32())
		end
		local cntOflint64 = byteArray.read_uint16()
		tb.lint64 = {}
		for i = 1, cntOflint64 do
			table.insert(tb.lint64, byteArray.read_int64())
		end
		local cntOfluint64 = byteArray.read_uint16()
		tb.luint64 = {}
		for i = 1, cntOfluint64 do
			table.insert(tb.luint64, byteArray.read_uint64())
		end
		local cntOflinte8 = byteArray.read_uint16()
		tb.linte8 = {}
		for i = 1, cntOflinte8 do
			table.insert(tb.linte8, byteArray.read_integer())
		end
		local cntOflinte16 = byteArray.read_uint16()
		tb.linte16 = {}
		for i = 1, cntOflinte16 do
			table.insert(tb.linte16, byteArray.read_integer())
		end
		local cntOflinte32 = byteArray.read_uint16()
		tb.linte32 = {}
		for i = 1, cntOflinte32 do
			table.insert(tb.linte32, byteArray.read_integer())
		end
		local cntOflinte64 = byteArray.read_uint16()
		tb.linte64 = {}
		for i = 1, cntOflinte64 do
			table.insert(tb.linte64, byteArray.read_integer())
		end
		local cntOflnum8 = byteArray.read_uint16()
		tb.lnum8 = {}
		for i = 1, cntOflnum8 do
			table.insert(tb.lnum8, byteArray.read_number())
		end
		local cntOflnum16 = byteArray.read_uint16()
		tb.lnum16 = {}
		for i = 1, cntOflnum16 do
			table.insert(tb.lnum16, byteArray.read_number())
		end
		local cntOflnum32 = byteArray.read_uint16()
		tb.lnum32 = {}
		for i = 1, cntOflnum32 do
			table.insert(tb.lnum32, byteArray.read_number())
		end
		local cntOflnum64 = byteArray.read_uint16()
		tb.lnum64 = {}
		for i = 1, cntOflnum64 do
			table.insert(tb.lnum64, byteArray.read_number())
		end
		local cntOflnfloat32 = byteArray.read_uint16()
		tb.lnfloat32 = {}
		for i = 1, cntOflnfloat32 do
			table.insert(tb.lnfloat32, byteArray.read_number())
		end
		local cntOflnfloat64 = byteArray.read_uint16()
		tb.lnfloat64 = {}
		for i = 1, cntOflnfloat64 do
			table.insert(tb.lnfloat64, byteArray.read_number())
		end
		local cntOflfloat = byteArray.read_uint16()
		tb.lfloat = {}
		for i = 1, cntOflfloat do
			table.insert(tb.lfloat, byteArray.read_float())
		end
		local cntOfldouble = byteArray.read_uint16()
		tb.ldouble = {}
		for i = 1, cntOfldouble do
			table.insert(tb.ldouble, byteArray.read_double())
		end
		local cntOflstring = byteArray.read_uint16()
		tb.lstring = {}
		for i = 1, cntOflstring do
			table.insert(tb.lstring, byteArray.read_string())
		end
		local cntOflunion = byteArray.read_uint16()
		tb.lunion = {}
		for i = 1, cntOflunion do
			local temp = union()
			temp.decode(byteArray)
			table.insert(tb.lunion, temp)
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(35)
		return tb.encode(byteArray)
	end

	return tb
end

function testnull()
	local tb = {}
	tb.msgId =36

	tb.encode = function(byteArray)
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(36)
		return tb.encode(byteArray)
	end

	return tb
end

function person1()
	local tb = {}
	tb.msgId =1001
	t.name = ""
	t.id = 0
	t.email = ""
	t.phone = {}

	tb.encode = function(byteArray)
		byteArray.write_string(tb.name)
		byteArray.write_int32(tb.id)
		byteArray.write_string(tb.email)
		byteArray.write_uint16(#(tb.phone))
		for k, v in pairs(tb.phone) do
			byteArray = v.encode(byteArray)
		end
		return byteArray.getBytes()
	end

	tb.decode = function(byteArray)
		tb.name = byteArray.read_string()
		tb.id = byteArray.read_int32()
		tb.email = byteArray.read_string()
		local cntOfphone = byteArray.read_uint16()
		tb.phone = {}
		for i = 1, cntOfphone do
			local temp = phoneNumber()
			temp.decode(byteArray)
			table.insert(tb.phone, temp)
		end
	end

	tb.build = function(byteArray)
		byteArray.setBytes({})
		byteArray.write_uint16(1001)
		return tb.encode(byteArray)
	end

	return tb
end

