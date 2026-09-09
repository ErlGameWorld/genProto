function ByteArray()
    local chunks = {}
    local nchunks = 0
    local rbuf = ""
    local mPos = 1

    local function wpack(fmt, ...)
        nchunks = nchunks + 1
        chunks[nchunks] = string.pack(fmt, ...)
    end

    local ba = {}

    ba.reset = function()
        chunks = {}
        nchunks = 0
        rbuf = ""
        mPos = 1
    end

    ba.setBytes = function(buf)
        if type(buf) == "string" then
            rbuf = buf
            mPos = 1
        else
            ba.reset()
        end
    end

    ba.getBytes = function()
        if nchunks == 0 then
            return rbuf
        end
        if nchunks == 1 then
            return chunks[1]
        end
        return table.concat(chunks)
    end

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
        for i = 1, #buff do
            bytes[i] = string.format(radix .. separator, string.byte(buff, i))
        end
        return table.concat(bytes)
    end

    ba.read_bool = function()
        return ba.read_int8() == 1
    end

    ba.write_bool = function(value)
        wpack(">b", value and 1 or 0)
    end

    ba.read_int8 = function()
        local value
        value, mPos = string.unpack(">i1", rbuf, mPos)
        return value
    end

    ba.write_int8 = function(value)
        wpack(">i1", value)
    end

    ba.read_uint8 = function()
        local value
        value, mPos = string.unpack(">I1", rbuf, mPos)
        return value
    end

    ba.write_uint8 = function(value)
        wpack(">I1", value)
    end

    ba.read_int16 = function()
        local value
        value, mPos = string.unpack(">i2", rbuf, mPos)
        return value
    end

    ba.write_int16 = function(value)
        wpack(">i2", value)
    end

    ba.read_uint16 = function()
        local value
        value, mPos = string.unpack(">I2", rbuf, mPos)
        return value
    end

    ba.write_uint16 = function(value)
        wpack(">I2", value)
    end

    ba.read_int32 = function()
        local value
        value, mPos = string.unpack(">i4", rbuf, mPos)
        return value
    end

    ba.write_int32 = function(value)
        wpack(">i4", value)
    end

    ba.read_uint32 = function()
        local value
        value, mPos = string.unpack(">I4", rbuf, mPos)
        return value
    end

    ba.write_uint32 = function(value)
        wpack(">I4", value)
    end

    ba.read_int64 = function()
        local value
        value, mPos = string.unpack(">i8", rbuf, mPos)
        return value
    end

    ba.write_int64 = function(value)
        wpack(">i8", value)
    end

    ba.read_uint64 = function()
        local value
        value, mPos = string.unpack(">I8", rbuf, mPos)
        return value
    end

    ba.write_uint64 = function(value)
        wpack(">I8", value)
    end

    ba.read_float = function()
        local value
        value, mPos = string.unpack(">f", rbuf, mPos)
        return value
    end

    ba.write_float = function(value)
        wpack(">f", value)
    end

    ba.read_double = function()
        local value
        value, mPos = string.unpack(">d", rbuf, mPos)
        return value
    end

    ba.write_double = function(value)
        wpack(">d", value)
    end

    ba.read_string = function()
        local value
        value, mPos = string.unpack(">s2", rbuf, mPos)
        return value
    end

    ba.write_string = function(value)
        wpack(">s2", value)
    end

    ba.read_binary = ba.read_string
    ba.write_binary = ba.write_string

    local function writeList(packChar, arr)
        local n = #arr
        for i = 1, n do
            wpack(">" .. packChar, arr[i])
        end
    end

    local function readList(packChar, count)
        if count == 0 then return {} end
        local fmt = ">" .. string.rep(packChar, count)
        local vals = { string.unpack(fmt, rbuf, mPos) }
        mPos = vals[count + 1]
        vals[count + 1] = nil
        return vals
    end

    ba.write_bool_list = function(arr) writeList("I1", arr) end
    ba.read_bool_list = function(count)
        local raw = readList("I1", count)
        for i = 1, count do raw[i] = raw[i] == 1 end
        return raw
    end
    ba.write_int8_list = function(arr) writeList("i1", arr) end
    ba.read_int8_list = function(count) return readList("i1", count) end
    ba.write_uint8_list = function(arr) writeList("I1", arr) end
    ba.read_uint8_list = function(count) return readList("I1", count) end
    ba.write_int16_list = function(arr) writeList("i2", arr) end
    ba.read_int16_list = function(count) return readList("i2", count) end
    ba.write_uint16_list = function(arr) writeList("I2", arr) end
    ba.read_uint16_list = function(count) return readList("I2", count) end
    ba.write_int32_list = function(arr) writeList("i4", arr) end
    ba.read_int32_list = function(count) return readList("i4", count) end
    ba.write_uint32_list = function(arr) writeList("I4", arr) end
    ba.read_uint32_list = function(count) return readList("I4", count) end
    ba.write_int64_list = function(arr) writeList("i8", arr) end
    ba.read_int64_list = function(count) return readList("i8", count) end
    ba.write_uint64_list = function(arr) writeList("I8", arr) end
    ba.read_uint64_list = function(count) return readList("I8", count) end
    ba.write_float_list = function(arr) writeList("f", arr) end
    ba.read_float_list = function(count) return readList("f", count) end
    ba.write_double_list = function(arr) writeList("d", arr) end
    ba.read_double_list = function(count) return readList("d", count) end

    ba.read_integer = function()
        local tag = ba.read_uint8()
        if tag == 8 then
            return ba.read_int8()
        elseif tag == 16 then
            return ba.read_int16()
        elseif tag == 32 then
            return ba.read_int32()
        elseif tag == 64 then
            return ba.read_int64()
        else
            error("Unknown integer tag: " .. tostring(tag))
        end
    end

    ba.write_integer = function(value)
        if value >= -128 and value <= 127 then
            wpack(">Bi1", 8, value)
        elseif value >= -32768 and value <= 32767 then
            wpack(">Bi2", 16, value)
        elseif value >= -2147483648 and value <= 2147483647 then
            wpack(">Bi4", 32, value)
        elseif value >= -9223372036854775808 and value <= 9223372036854775807 then
            wpack(">Bi8", 64, value)
        else
            error("Integer out of range: " .. tostring(value))
        end
    end

    ba.read_number = function()
        local tag = ba.read_uint8()
        if tag == 8 then
            return ba.read_int8()
        elseif tag == 16 then
            return ba.read_int16()
        elseif tag == 32 then
            return ba.read_int32()
        elseif tag == 64 then
            return ba.read_int64()
        elseif tag == 33 then
            return ba.read_float()
        elseif tag == 65 then
            return ba.read_double()
        else
            error("Invalid number tag: " .. tostring(tag))
        end
    end

    ba.write_number = function(value)
        local valueType = math.type(value)
        if valueType == "integer" then
            ba.write_integer(value)
        elseif valueType == "float" then
            if value >= 1.175494351e-38 and value <= 3.402823466e+38 then
                wpack(">Bf", 33, value)
            elseif value >= 2.2250738585072014e-308 and value <= 1.7976931348623158e+308 then
                wpack(">Bd", 65, value)
            else
                error("Float out of range: " .. tostring(value))
            end
        else
            error("Not a number: " .. tostring(value))
        end
    end

    return ba
end
