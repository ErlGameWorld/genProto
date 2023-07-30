function test()
	local tb = {}
	tb.msgId =1
	t.aa = ""

	tb.encode = function(byteArray)
		byteArray.write_string(tb.aa)
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.aa = byteArray.read_string()
	end

	tb.build = function(byteArray)
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
		return byteArray
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
		return byteArray
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
		return byteArray
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.test = byteArray.read_string()
		tb.type = byteArray.read_int32()
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.bool = byteArray.read_bool()
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_int8()
		tb.int2 = byteArray.read_int8()
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_uint8()
		tb.int2 = byteArray.read_uint8()
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_int16()
		tb.int2 = byteArray.read_int16()
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_uint16()
		tb.int2 = byteArray.read_uint16()
	end

	tb.build = function(byteArray)
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
		return byteArray
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_uint32()
		tb.int2 = byteArray.read_uint32()
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_int64()
		tb.int2 = byteArray.read_int64()
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_uint64()
		tb.int2 = byteArray.read_uint64()
	end

	tb.build = function(byteArray)
		byteArray.write_uint16(14)
		return tb.encode(byteArray)
	end

	return tb
end

function tinteger()
	local tb = {}
	tb.msgId =15
	t.int1 = {}
	t.int2 = {}
	t.int3 = {}
	t.int4 = {}
	t.int5 = {}
	t.int6 = {}
	t.int7 = {}
	t.int8 = {}

	tb.encode = function(byteArray)
		if tb.int1 and next(tb.int1) then
			byteArray.write_uint8(1)
			tb.int1.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int2 and next(tb.int2) then
			byteArray.write_uint8(1)
			tb.int2.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int3 and next(tb.int3) then
			byteArray.write_uint8(1)
			tb.int3.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int4 and next(tb.int4) then
			byteArray.write_uint8(1)
			tb.int4.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int5 and next(tb.int5) then
			byteArray.write_uint8(1)
			tb.int5.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int6 and next(tb.int6) then
			byteArray.write_uint8(1)
			tb.int6.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int7 and next(tb.int7) then
			byteArray.write_uint8(1)
			tb.int7.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int8 and next(tb.int8) then
			byteArray.write_uint8(1)
			tb.int8.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		return byteArray
	end

	tb.decode = function(byteArray)
		local isNilint1 = byteArray.read_uint8()
		if isNilint1 > 0 then
			tb.int1 = integer()
			tb.int1.decode(byteArray)
		else
			tb.int1 = {}
		end
		local isNilint2 = byteArray.read_uint8()
		if isNilint2 > 0 then
			tb.int2 = integer()
			tb.int2.decode(byteArray)
		else
			tb.int2 = {}
		end
		local isNilint3 = byteArray.read_uint8()
		if isNilint3 > 0 then
			tb.int3 = integer()
			tb.int3.decode(byteArray)
		else
			tb.int3 = {}
		end
		local isNilint4 = byteArray.read_uint8()
		if isNilint4 > 0 then
			tb.int4 = integer()
			tb.int4.decode(byteArray)
		else
			tb.int4 = {}
		end
		local isNilint5 = byteArray.read_uint8()
		if isNilint5 > 0 then
			tb.int5 = integer()
			tb.int5.decode(byteArray)
		else
			tb.int5 = {}
		end
		local isNilint6 = byteArray.read_uint8()
		if isNilint6 > 0 then
			tb.int6 = integer()
			tb.int6.decode(byteArray)
		else
			tb.int6 = {}
		end
		local isNilint7 = byteArray.read_uint8()
		if isNilint7 > 0 then
			tb.int7 = integer()
			tb.int7.decode(byteArray)
		else
			tb.int7 = {}
		end
		local isNilint8 = byteArray.read_uint8()
		if isNilint8 > 0 then
			tb.int8 = integer()
			tb.int8.decode(byteArray)
		else
			tb.int8 = {}
		end
	end

	tb.build = function(byteArray)
		byteArray.write_uint16(15)
		return tb.encode(byteArray)
	end

	return tb
end

function tnumber()
	local tb = {}
	tb.msgId =16
	t.int1 = {}
	t.int2 = {}
	t.int3 = {}
	t.int4 = {}
	t.int5 = {}
	t.int6 = {}
	t.int7 = {}
	t.int8 = {}
	t.float1 = {}
	t.float2 = {}

	tb.encode = function(byteArray)
		if tb.int1 and next(tb.int1) then
			byteArray.write_uint8(1)
			tb.int1.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int2 and next(tb.int2) then
			byteArray.write_uint8(1)
			tb.int2.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int3 and next(tb.int3) then
			byteArray.write_uint8(1)
			tb.int3.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int4 and next(tb.int4) then
			byteArray.write_uint8(1)
			tb.int4.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int5 and next(tb.int5) then
			byteArray.write_uint8(1)
			tb.int5.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int6 and next(tb.int6) then
			byteArray.write_uint8(1)
			tb.int6.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int7 and next(tb.int7) then
			byteArray.write_uint8(1)
			tb.int7.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.int8 and next(tb.int8) then
			byteArray.write_uint8(1)
			tb.int8.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.float1 and next(tb.float1) then
			byteArray.write_uint8(1)
			tb.float1.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.float2 and next(tb.float2) then
			byteArray.write_uint8(1)
			tb.float2.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		return byteArray
	end

	tb.decode = function(byteArray)
		local isNilint1 = byteArray.read_uint8()
		if isNilint1 > 0 then
			tb.int1 = number()
			tb.int1.decode(byteArray)
		else
			tb.int1 = {}
		end
		local isNilint2 = byteArray.read_uint8()
		if isNilint2 > 0 then
			tb.int2 = number()
			tb.int2.decode(byteArray)
		else
			tb.int2 = {}
		end
		local isNilint3 = byteArray.read_uint8()
		if isNilint3 > 0 then
			tb.int3 = number()
			tb.int3.decode(byteArray)
		else
			tb.int3 = {}
		end
		local isNilint4 = byteArray.read_uint8()
		if isNilint4 > 0 then
			tb.int4 = number()
			tb.int4.decode(byteArray)
		else
			tb.int4 = {}
		end
		local isNilint5 = byteArray.read_uint8()
		if isNilint5 > 0 then
			tb.int5 = number()
			tb.int5.decode(byteArray)
		else
			tb.int5 = {}
		end
		local isNilint6 = byteArray.read_uint8()
		if isNilint6 > 0 then
			tb.int6 = number()
			tb.int6.decode(byteArray)
		else
			tb.int6 = {}
		end
		local isNilint7 = byteArray.read_uint8()
		if isNilint7 > 0 then
			tb.int7 = number()
			tb.int7.decode(byteArray)
		else
			tb.int7 = {}
		end
		local isNilint8 = byteArray.read_uint8()
		if isNilint8 > 0 then
			tb.int8 = number()
			tb.int8.decode(byteArray)
		else
			tb.int8 = {}
		end
		local isNilfloat1 = byteArray.read_uint8()
		if isNilfloat1 > 0 then
			tb.float1 = number()
			tb.float1.decode(byteArray)
		else
			tb.float1 = {}
		end
		local isNilfloat2 = byteArray.read_uint8()
		if isNilfloat2 > 0 then
			tb.float2 = number()
			tb.float2.decode(byteArray)
		else
			tb.float2 = {}
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_float()
		tb.int2 = byteArray.read_float()
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_double()
		tb.int2 = byteArray.read_double()
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		tb.int1 = byteArray.read_string()
		tb.int2 = byteArray.read_string()
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_bool())
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_int8())
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_uint8())
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_int16())
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_uint16())
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_int32())
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_uint32())
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_int64())
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_uint64())
		end
	end

	tb.build = function(byteArray)
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
		for k, v in pairs(tb.int1) do
			byteArray = v.encode(byteArray)
		end
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			local temp = integer()
			temp.decode(byteArray)
			table.insert(tb.int1, temp)
		end
	end

	tb.build = function(byteArray)
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
		for k, v in pairs(tb.int1) do
			byteArray = v.encode(byteArray)
		end
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			local temp = number()
			temp.decode(byteArray)
			table.insert(tb.int1, temp)
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_float())
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_double())
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
	end

	tb.decode = function(byteArray)
		local cntOfint1 = byteArray.read_uint16()
		tb.int1 = {}
		for i = 1, cntOfint1 do
			table.insert(tb.int1, byteArray.read_string())
		end
	end

	tb.build = function(byteArray)
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
		return byteArray
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
	t.inte8 = {}
	t.uinte8 = {}
	t.inte16 = {}
	t.uinte16 = {}
	t.inte32 = {}
	t.uinte32 = {}
	t.inte64 = {}
	t.uinte64 = {}
	t.num8 = {}
	t.unum8 = {}
	t.num16 = {}
	t.unum16 = {}
	t.num32 = {}
	t.unum32 = {}
	t.num64 = {}
	t.unum64 = {}
	t.numfloat = {}
	t.numdouble = {}
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
		if tb.inte8 and next(tb.inte8) then
			byteArray.write_uint8(1)
			tb.inte8.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.uinte8 and next(tb.uinte8) then
			byteArray.write_uint8(1)
			tb.uinte8.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.inte16 and next(tb.inte16) then
			byteArray.write_uint8(1)
			tb.inte16.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.uinte16 and next(tb.uinte16) then
			byteArray.write_uint8(1)
			tb.uinte16.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.inte32 and next(tb.inte32) then
			byteArray.write_uint8(1)
			tb.inte32.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.uinte32 and next(tb.uinte32) then
			byteArray.write_uint8(1)
			tb.uinte32.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.inte64 and next(tb.inte64) then
			byteArray.write_uint8(1)
			tb.inte64.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.uinte64 and next(tb.uinte64) then
			byteArray.write_uint8(1)
			tb.uinte64.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.num8 and next(tb.num8) then
			byteArray.write_uint8(1)
			tb.num8.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.unum8 and next(tb.unum8) then
			byteArray.write_uint8(1)
			tb.unum8.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.num16 and next(tb.num16) then
			byteArray.write_uint8(1)
			tb.num16.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.unum16 and next(tb.unum16) then
			byteArray.write_uint8(1)
			tb.unum16.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.num32 and next(tb.num32) then
			byteArray.write_uint8(1)
			tb.num32.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.unum32 and next(tb.unum32) then
			byteArray.write_uint8(1)
			tb.unum32.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.num64 and next(tb.num64) then
			byteArray.write_uint8(1)
			tb.num64.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.unum64 and next(tb.unum64) then
			byteArray.write_uint8(1)
			tb.unum64.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.numfloat and next(tb.numfloat) then
			byteArray.write_uint8(1)
			tb.numfloat.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
		if tb.numdouble and next(tb.numdouble) then
			byteArray.write_uint8(1)
			tb.numdouble.encode(byteArray)
		else
			byteArray.write_uint8(0)
		end
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
		for k, v in pairs(tb.linte8) do
			byteArray = v.encode(byteArray)
		end
		byteArray.write_uint16(#(tb.linte16))
		for k, v in pairs(tb.linte16) do
			byteArray = v.encode(byteArray)
		end
		byteArray.write_uint16(#(tb.linte32))
		for k, v in pairs(tb.linte32) do
			byteArray = v.encode(byteArray)
		end
		byteArray.write_uint16(#(tb.linte64))
		for k, v in pairs(tb.linte64) do
			byteArray = v.encode(byteArray)
		end
		byteArray.write_uint16(#(tb.lnum8))
		for k, v in pairs(tb.lnum8) do
			byteArray = v.encode(byteArray)
		end
		byteArray.write_uint16(#(tb.lnum16))
		for k, v in pairs(tb.lnum16) do
			byteArray = v.encode(byteArray)
		end
		byteArray.write_uint16(#(tb.lnum32))
		for k, v in pairs(tb.lnum32) do
			byteArray = v.encode(byteArray)
		end
		byteArray.write_uint16(#(tb.lnum64))
		for k, v in pairs(tb.lnum64) do
			byteArray = v.encode(byteArray)
		end
		byteArray.write_uint16(#(tb.lnfloat32))
		for k, v in pairs(tb.lnfloat32) do
			byteArray = v.encode(byteArray)
		end
		byteArray.write_uint16(#(tb.lnfloat64))
		for k, v in pairs(tb.lnfloat64) do
			byteArray = v.encode(byteArray)
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
		return byteArray
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
		local isNilinte8 = byteArray.read_uint8()
		if isNilinte8 > 0 then
			tb.inte8 = integer()
			tb.inte8.decode(byteArray)
		else
			tb.inte8 = {}
		end
		local isNiluinte8 = byteArray.read_uint8()
		if isNiluinte8 > 0 then
			tb.uinte8 = integer()
			tb.uinte8.decode(byteArray)
		else
			tb.uinte8 = {}
		end
		local isNilinte16 = byteArray.read_uint8()
		if isNilinte16 > 0 then
			tb.inte16 = integer()
			tb.inte16.decode(byteArray)
		else
			tb.inte16 = {}
		end
		local isNiluinte16 = byteArray.read_uint8()
		if isNiluinte16 > 0 then
			tb.uinte16 = integer()
			tb.uinte16.decode(byteArray)
		else
			tb.uinte16 = {}
		end
		local isNilinte32 = byteArray.read_uint8()
		if isNilinte32 > 0 then
			tb.inte32 = integer()
			tb.inte32.decode(byteArray)
		else
			tb.inte32 = {}
		end
		local isNiluinte32 = byteArray.read_uint8()
		if isNiluinte32 > 0 then
			tb.uinte32 = integer()
			tb.uinte32.decode(byteArray)
		else
			tb.uinte32 = {}
		end
		local isNilinte64 = byteArray.read_uint8()
		if isNilinte64 > 0 then
			tb.inte64 = integer()
			tb.inte64.decode(byteArray)
		else
			tb.inte64 = {}
		end
		local isNiluinte64 = byteArray.read_uint8()
		if isNiluinte64 > 0 then
			tb.uinte64 = integer()
			tb.uinte64.decode(byteArray)
		else
			tb.uinte64 = {}
		end
		local isNilnum8 = byteArray.read_uint8()
		if isNilnum8 > 0 then
			tb.num8 = number()
			tb.num8.decode(byteArray)
		else
			tb.num8 = {}
		end
		local isNilunum8 = byteArray.read_uint8()
		if isNilunum8 > 0 then
			tb.unum8 = number()
			tb.unum8.decode(byteArray)
		else
			tb.unum8 = {}
		end
		local isNilnum16 = byteArray.read_uint8()
		if isNilnum16 > 0 then
			tb.num16 = number()
			tb.num16.decode(byteArray)
		else
			tb.num16 = {}
		end
		local isNilunum16 = byteArray.read_uint8()
		if isNilunum16 > 0 then
			tb.unum16 = number()
			tb.unum16.decode(byteArray)
		else
			tb.unum16 = {}
		end
		local isNilnum32 = byteArray.read_uint8()
		if isNilnum32 > 0 then
			tb.num32 = number()
			tb.num32.decode(byteArray)
		else
			tb.num32 = {}
		end
		local isNilunum32 = byteArray.read_uint8()
		if isNilunum32 > 0 then
			tb.unum32 = number()
			tb.unum32.decode(byteArray)
		else
			tb.unum32 = {}
		end
		local isNilnum64 = byteArray.read_uint8()
		if isNilnum64 > 0 then
			tb.num64 = number()
			tb.num64.decode(byteArray)
		else
			tb.num64 = {}
		end
		local isNilunum64 = byteArray.read_uint8()
		if isNilunum64 > 0 then
			tb.unum64 = number()
			tb.unum64.decode(byteArray)
		else
			tb.unum64 = {}
		end
		local isNilnumfloat = byteArray.read_uint8()
		if isNilnumfloat > 0 then
			tb.numfloat = number()
			tb.numfloat.decode(byteArray)
		else
			tb.numfloat = {}
		end
		local isNilnumdouble = byteArray.read_uint8()
		if isNilnumdouble > 0 then
			tb.numdouble = number()
			tb.numdouble.decode(byteArray)
		else
			tb.numdouble = {}
		end
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
			local temp = integer()
			temp.decode(byteArray)
			table.insert(tb.linte8, temp)
		end
		local cntOflinte16 = byteArray.read_uint16()
		tb.linte16 = {}
		for i = 1, cntOflinte16 do
			local temp = integer()
			temp.decode(byteArray)
			table.insert(tb.linte16, temp)
		end
		local cntOflinte32 = byteArray.read_uint16()
		tb.linte32 = {}
		for i = 1, cntOflinte32 do
			local temp = integer()
			temp.decode(byteArray)
			table.insert(tb.linte32, temp)
		end
		local cntOflinte64 = byteArray.read_uint16()
		tb.linte64 = {}
		for i = 1, cntOflinte64 do
			local temp = integer()
			temp.decode(byteArray)
			table.insert(tb.linte64, temp)
		end
		local cntOflnum8 = byteArray.read_uint16()
		tb.lnum8 = {}
		for i = 1, cntOflnum8 do
			local temp = number()
			temp.decode(byteArray)
			table.insert(tb.lnum8, temp)
		end
		local cntOflnum16 = byteArray.read_uint16()
		tb.lnum16 = {}
		for i = 1, cntOflnum16 do
			local temp = number()
			temp.decode(byteArray)
			table.insert(tb.lnum16, temp)
		end
		local cntOflnum32 = byteArray.read_uint16()
		tb.lnum32 = {}
		for i = 1, cntOflnum32 do
			local temp = number()
			temp.decode(byteArray)
			table.insert(tb.lnum32, temp)
		end
		local cntOflnum64 = byteArray.read_uint16()
		tb.lnum64 = {}
		for i = 1, cntOflnum64 do
			local temp = number()
			temp.decode(byteArray)
			table.insert(tb.lnum64, temp)
		end
		local cntOflnfloat32 = byteArray.read_uint16()
		tb.lnfloat32 = {}
		for i = 1, cntOflnfloat32 do
			local temp = number()
			temp.decode(byteArray)
			table.insert(tb.lnfloat32, temp)
		end
		local cntOflnfloat64 = byteArray.read_uint16()
		tb.lnfloat64 = {}
		for i = 1, cntOflnfloat64 do
			local temp = number()
			temp.decode(byteArray)
			table.insert(tb.lnfloat64, temp)
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
		byteArray.write_uint16(35)
		return tb.encode(byteArray)
	end

	return tb
end

function testnull()
	local tb = {}
	tb.msgId =36

	tb.encode = function(byteArray)
		return byteArray
	end

	tb.decode = function(byteArray)
	end

	tb.build = function(byteArray)
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
		return byteArray
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
		byteArray.write_uint16(1001)
		return tb.encode(byteArray)
	end

	return tb
end

