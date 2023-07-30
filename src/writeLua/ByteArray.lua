BA_ENDIAN_BIG = "ENDIAN_BIG"						-- 大端
BA_ENDIAN_LITTLE = "ENDIAN_LITTLE"					-- 小端

function ByteArray(endian)
	----------------------------------------------------------------------
	-- private member variable
	----------------------------------------------------------------------
	local mRadix = {[8]="%03o", [10]="%03u", [16]="%02X"}		-- 进制
	local mEndian = endian or ""								-- 大小端标识
	local mBuf = {}												-- 二进制字节流
	local mPos = 1												-- 读写位置
	----------------------------------------------------------------------
	-- private method
	----------------------------------------------------------------------
	-- 验证读写位置
	local function checkAvailable()
		assert(#mBuf >= mPos, string.format("End of file was encountered. pos: %d, length: %d.", mPos, #mBuf))
	end
	-- 获取字符码
	local function getLetterCode(fmt)
		fmt = fmt or ""
		if BA_ENDIAN_LITTLE == mEndian then
			return "<"..fmt
		elseif BA_ENDIAN_BIG == mEndianthen then
			return ">"..fmt
		else
			return "="..fmt
		end
	end
	-- 读单个字节
	local function readRawByte()
		checkAvailable()
		local rawByte = mBuf[mPos]
		mPos = mPos + 1
		return rawByte
	end
	-- 写单个字节
	local function writeRawByte(rawByte)
		if mPos > #mBuf + 1 then
			for i=#mBuf + 1, mPos - 1 do
				mBuf[i] = string.char(0)
			end
		end
		mBuf[mPos] = rawByte
		mPos = mPos + 1
	end
	-- 读字节流
	local function readBuf(length)
		checkAvailable()
		local buf = table.concat(mBuf, "", mPos, mPos + length - 1)
		mPos = mPos + length
		return buf
	end
	-- 写字节流
	local function writeBuf(buf)
		for i=1, #buf do
			writeRawByte(buf:sub(i, i))
		end
	end
	-- 读字符串
	local function read_string_bytes(length)
		if 0 == length then
			return ""
		end
		local tmp, value = string.unpack(readBuf(length), getLetterCode("A"..length))
		return value
	end
	-- 写字符串
	local function write_string_bytes(value)
		local buf = string.pack(getLetterCode("A"), value)
		writeBuf(buf)
	end
	----------------------------------------------------------------------
	-- public method
	----------------------------------------------------------------------
	local ba = {}
	-- 设置大小端
	ba.setEndian = function(endian)
		mEndian = endian or ""
	end
	-- 设置字节流
	ba.setBytes = function(buf)
		if #mBuf > 0 then
			return
		end
		writeBuf(buf)
		mPos = 1		-- 这里必须重置读写位置为1,方能保证接下去的读操作正确
	end
	-- 获取字节流
	ba.getBytes = function()
		local bytes = {}
		for i=1, #mBuf do
			bytes[#bytes+1] = string.byte(mBuf[i])
		end
		local packRes = string.pack(getLetterCode("b"..#bytes), unpack(bytes))
		return packRes
	end
	-- 获取字节流长度
	ba.getLength = function()
		return #mBuf
	end
	-- 字节流转为字符串,radix-8,10,16
	ba.toString = function(radix, separator)
		radix = radix or 16
		radix = mRadix[radix] or "%02X"
		separator = separator or " "
		local bytes = {}
		for i=1, #mBuf do
			bytes[i] = string.format(radix..separator, string.byte(mBuf[i]))
		end
		return table.concat(bytes)
	end
	----------------------------------------------------------------------
	-- 读16位整型
	ba.read_int16 = function()
		local tmp, value = string.unpack(readBuf(2), getLetterCode("h"))
		return value
	end
	-- 写16位整型
	ba.write_int16 = function(value)
		local buf = string.pack(getLetterCode("h"), value)
		writeBuf(buf)
	end
	-- 读16位无符号整型
	ba.read_uint16 = function()
		local tmp, value = string.unpack(readBuf(2), getLetterCode("H"))
		return value
	end
	-- 写16位无符号整型
	ba.write_uint16 = function(value)
        local sstr = getLetterCode("H")
		local buf = string.pack(sstr, value)
		writeBuf(buf)
	end
	-- 读32位整型
	ba.read_int32 = function()
		local tmp, value = string.unpack(readBuf(4), getLetterCode("i"))
		return value
	end
	-- 写32位整型
	ba.write_int32 = function(value)
		local buf = string.pack(getLetterCode("i"), value)
		writeBuf(buf)
	end
	-- 读32位无符号整型
	ba.read_uint32 = function()
		local tmp, value = string.unpack(readBuf(4), getLetterCode("I"))
		return value
	end
	-- 写32位无符号整型
	ba.write_uint32 = function(value)
		local buf = string.pack(getLetterCode("I"), value)
		writeBuf(buf)
	end
	-- 读长整型
	ba.read_long = function()
		local tmp, value = string.unpack(readBuf(4), getLetterCode("l"))
		return value
	end
	-- 写长整型
	ba.write_long = function(value)
		local buf = string.pack(getLetterCode("l"), value)
		writeBuf(buf)
	end
	-- 读无符号长整型
	ba.read_ulong = function()
		local tmp, value = string.unpack(readBuf(4), getLetterCode("L"))
		return value
	end
	-- 写无符号长整型
	ba.write_ulong = function(value)
		local buf = string.pack(getLetterCode("L"), value)
		writeBuf(buf)
	end
	-- 读64位整型
	ba.read_int64 = function()
		-- local tmp, value = string.unpack(readBuf(8), getLetterCode("m"))
		-- return value
		return read_string_bytes(8)
	end
	-- 写64位整型
	ba.write_int64 = function(value)
		-- local buf = string.pack(getLetterCode("m"), value)
		-- writeBuf(buf)
		local buf = string.pack(getLetterCode("A"), value)
		writeBuf(buf)
	end
	-- 读64位无符号整型
	ba.read_uint64 = function()
		-- local tmp, value = string.unpack(readBuf(8), getLetterCode("M"))
		-- return value
		return read_string_bytes(8)
	end
	-- 写64位无符号整型
	ba.write_uint64 = function(value)
		-- local buf = string.pack(getLetterCode("M"), value)
		-- writeBuf(buf)
		local buf = string.pack(getLetterCode("A"), value)
		writeBuf(buf)
	end
	-- 读单精度浮点型
	ba.read_float = function()
		local tmp, value = string.unpack(readBuf(4), getLetterCode("f"))
		return value
	end
	-- 写单精度浮点型
	ba.write_float = function(value)
		local buf = string.pack(getLetterCode("f"), value)
		writeBuf(buf)
	end
	-- 读双精度浮点型
	ba.read_double = function()
		local tmp, value = string.unpack(readBuf(8), getLetterCode("d"))
		return value
	end
	-- 写双精度浮点型
	ba.write_double = function(value)
		local buf = string.pack(getLetterCode("d"), value)
		writeBuf(buf)
	end
	-- 读布尔型
	ba.read_bool = function()
		return 1 == read_char()
	end
	-- 写布尔型
	ba.write_bool = function(value)
		if value then
			ba.write_char(1)
		else
			ba.write_char(0)
		end
	end
	-- 读字符型
	ba.read_int8 = function()
		local tmp, value = string.unpack(readRawByte(), "c")
		return value
	end
	-- 写字符型
	ba.write_int8 = function(value)
		writeRawByte(string.pack("c", value))
	end
	-- 读单字节
	ba.read_uint8 = function()
		-- 方法1
		-- return string.byte(readRawByte())
		-- 方法2
		local tmp, value = string.unpack(readRawByte(), "b")
		return value
	end
	-- 写单字节
	ba.write_uint8 = function(value)
		-- 方法1
		-- writeRawByte(string.char(value))
		-- 方法2
		writeRawByte(string.pack("b", value))
	end
	-- 读字符串
	ba.read_string = function()
		local length = ba.read_uint16()
		return read_string_bytes(length)
	end
	-- 写字符串
	ba.write_string = function(value)
		local buf = string.pack(getLetterCode("A"), value)
		ba.write_uint16(#buf)
		writeBuf(buf)
	end
	----------------------------------------------------------------------
	return ba
end

