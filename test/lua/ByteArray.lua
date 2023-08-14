function ByteArray()
    local mBuf = {}                                                           -- 二进制字节流
    local mPos = 1                                                            -- 读写位置

    local ba = {}
    -- 设置字节流  解码先设置从tcp获取得来的字节数据 或者编码之前先重置 mBuf 和 mPos
    ba.setBytes = function(buf)
        mBuf = buf
        mPos = 1                                                            -- 这里必须重置读写位置为1,方能保证接下去的读操作正确
    end

    -- 获取字节流 编码完成之后 通过该接口获取编码之后的字节数据
    ba.getBytes = function()
        return table.concat(mBuf)
    end

    -- 字节流转为字符串,radix-8,10,16
    ba.toString = function(buff, radix, separator)
        radix = radix or 16
        if radix == 8 then
            radix = "%03o"
        elseif radix == 16 then
            radix = "%02X"
        else
            radix = "%03u"
        end
        separator = separator or " "
        local bytes = {}
        for i = 1, buff do
            bytes[i] = string.format(radix .. separator, string.byte(buff[i]))
        end
        return table.concat(bytes)
    end

    -- 读布尔型
    ba.read_bool = function()
        return 1 == read_int8()
    end

    -- 写布尔型
    ba.write_bool = function(value)
        if value then
            ba.write_int8(1)
        else
            ba.write_int8(0)
        end
    end

    -- 读字符型
    ba.read_int8 = function()
        local value, tPos = string.unpack(">i1", mBuf, mPos)
        mPos = tPos
        return value
    end

    -- 写字符型
    ba.write_int8 = function(value)
        local value = string.pack(">i1", value)
        mBuf[mPos] = value
        mPos = mPos + 1
    end

    -- 读单字节
    ba.read_uint8 = function()
        local value, tPos = string.unpack(">I1", mBuf, mPos)
        mPos = tPos
        return value
    end

    -- 写单字节
    ba.write_uint8 = function(value)
        local value = string.pack(">I1", value)
        mBuf[mPos] = value
        mPos = mPos + 1
    end

    -- 读16位整型
    ba.read_int16 = function()
        local value, tPos = string.unpack(">i2", mBuf, mPos)
        mPos = tPos
        return value
    end

    -- 写16位整型
    ba.write_int16 = function(value)
        local value = string.pack(">i2", value)
        mBuf[mPos] = value
        mPos = mPos + 1
    end

    -- 读16位无符号整型
    ba.read_uint16 = function()
        local value, tPos = string.unpack(">I2", mBuf, mPos)
        mPos = tPos
        return value
    end

    -- 写16位无符号整型
    ba.write_uint16 = function(value)
        local value = string.pack(">I2", value)
        mBuf[mPos] = value
        mPos = mPos + 1
    end

    -- 读32位整型
    ba.read_int32 = function()
        local value, tPos = string.unpack(">i4", mBuf, mPos)
        mPos = tPos
        return value
    end

    -- 写32位整型
    ba.write_int32 = function(value)
        local value = string.pack(">i4", value)
        mBuf[mPos] = value
        mPos = mPos + 1
    end

    -- 读32位无符号整型
    ba.read_uint32 = function()
        local value, tPos = string.unpack(">I4", mBuf, mPos)
        mPos = tPos
        return value
    end

    -- 写32位无符号整型
    ba.write_uint32 = function(value)
        local value = string.pack(">I4", value)
        mBuf[mPos] = value
        mPos = mPos + 1
    end

    -- 读64位符号整型
    ba.read_int64 = function()
        local value, tPos = string.unpack(">i8", mBuf, mPos)
        mPos = tPos
        return value
    end

    -- 写64位整型
    ba.write_int64 = function(value)
        local value = string.pack(">i8", value)
        mBuf[mPos] = value
        mPos = mPos + 1
    end

    -- 读64位无符号整型
    ba.read_uint64 = function()
        local value, tPos = string.unpack(">I8", mBuf, mPos)
        mPos = tPos
        return value
    end

    -- 写64位无符号整型
    ba.write_uint64 = function(value)
        local value = string.pack(">I8", value)
        mBuf[mPos] = value
        mPos = mPos + 1
    end

    -- 读单精度浮点型
    ba.read_float = function()
        local value, tPos = string.unpack(">f", mBuf, mPos)
        mPos = tPos
        return value
    end

    -- 写单精度浮点型
    ba.write_float = function(value)
        local value = string.pack(">f", value)
        mBuf[mPos] = value
        mPos = mPos + 1
    end

    -- 读双精度浮点型
    ba.read_double = function()
        local value, tPos = string.unpack(">d", mBuf, mPos)
        mPos = tPos
        return value
    end

    -- 写双精度浮点型
    ba.write_double = function(value)
        local value = string.pack(">d", value)
        mBuf[mPos] = value
        mPos = mPos + 1
    end

    -- 读字符串
    ba.read_string = function()
        local value, tPos = string.unpack(">s2", mBuf, mPos)
        mPos = tPos
        return value
    end

    -- 写字符串
    ba.write_string = function(value)
        local value = string.pack(">s2", value)
        mBuf[mPos] = value
        mPos = mPos + 1
    end

    -- 读字整数
    ba.read_integer = function()
        local tag = read_int8()
        if tag == 8 then
            return read_int8()
        elseif tag == 16 then
            return read_int16()
        elseif tag == 32 then
            return read_int32()
        elseif tag == 64 then
            return read_int64()
        end
    end

    -- 写整数
    ba.write_integer = function(value)
        if value >= -128 and value <= 127 then
            write_int8(8)
            write_int8(value)
        elseif value >= -32768 and value <= 32767 then
            write_int8(16)
            write_int16(value)
        elseif value >= -2147483648 and value <= 2147483647 then
            write_int8(32)
            write_int32(value)
        elseif value >= -9223372036854775808 and value <= 9223372036854775807 then
            write_int8(64)
            write_int64(value)
        end
    end

    -- 读字数字
    ba.read_integer = function()
        local tag = read_int8()
        if tag == 8 then
            return read_int8()
        elseif tag == 16 then
            return read_int16()
        elseif tag == 32 then
            return read_int32()
        elseif tag == 64 then
            return read_int64()
        elseif tag == 33 then
            return read_float()
        elseif tag == 65 then
            return read_double()
        end
    end

    -- 写数字
    ba.write_integer = function(value)
        local valueType = math.type(value)
        if valueType == 'integer' then
            if value >= -128 and value <= 127 then
                write_int8(8)
                write_int8(value)
            elseif value >= -32768 and value <= 32767 then
                write_int8(16)
                write_int16(value)
            elseif value >= -2147483648 and value <= 2147483647 then
                write_int8(32)
                write_int32(value)
            elseif value >= -9223372036854775808 and value <= 9223372036854775807 then
                write_int8(64)
                write_int64(value)
            end
        elseif valueType == 'float' then
            if value >= 1.175494351e-38 and value <= 3.402823466e+38 then
                write_int8(33)
                write_float(value)
            elseif value >= 2.2250738585072014e-308 and value <= 1.7976931348623158e+308 then
                write_int8(65)
                write_float(value)
            end
        end
    end

    ----------------------------------------------------------------------
    return ba
end

