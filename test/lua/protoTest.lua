function test()
    -- 封包

    local msgTable = new phoneNumber()
    msgTable.number = new test()
    msgTable.type = 1
    local byteArray = ByteArray()
    byteArray = msgTable.build(byteArray)
    local body = byteArray.getBytes()
    -- 包头(大小端转换后的包体长度)
    local bodyLength = string.len(body)							-- 包体长度
    local head = string.pack(">I4", bodyLength)	-- 包头四个字节,这里要用"i"
    -- 发送
    local nSend = dosend(head..body)

    -- 解包 先读取消息id 根据消息id到protoNane.lua 获取函数名
    -- 再根据函数名到 _G表找到反序列化的函数

end