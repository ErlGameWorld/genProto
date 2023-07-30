genProto
=====

    用于根据自定义的协议文件生成 erl, c#, lua的序列化和反序列化代码

Build
-----

    rebar3 escriptize  

Use
-----

    rebar3 escriptize生成的genProto genPtoto.cmd 在_build/default/bin下面 将其复制到可以被搜索的执行路径或者工作目录
    genProto ProtoInputDir HrlOutDir ErlOutDir

# 简单描述

    可用于erlang游戏或者erlang其他网络应用的协议解析编码解码的项目,具有较高的编码解码性能，
    协议文件存放在proto目录 文件名为 Message protocol definition file的缩写 mpdf。

# 支持的数据类型

    erl: int8 uint8 int16 uint16 int32 uint32 int64 uint64 integer(整数 64位) number(整数或者浮点数 64位) string float(32位浮点数) double(64位浮点数) bool record(struct) 以及上面类型的列表
    lua(待修正): int8 uint8 int16 uint16 int32 uint32 int64 uint64 integer(整数 64位) number(整数或者浮点数 64位) string float(32位浮点数) double(64位浮点数) bool record 以及上面类型的列表
    c#(待修正): int8 uint8 int16 uint16 int32 uint32 int64 uint64 integer(整数 64位) number(整数或者浮点数 64位) string float(32位浮点数) double(64位浮点数) bool record 以及上面类型的列表

# 各种数据类型的编码格式(字节序大端存储， string用utf8编码)

    int8：               直接存8bit的值       
    uint8：              直接存8bit的值  
    int16：              直接存16bit的值
    uint16：             直接存16bit的值
    int32：              直接存32bit的值
    uint32：             直接存32bit的值
    int64：              直接存64bit的值
    uint64：             直接存64bit的值
    integer：            如果值在int8范围区间: 8bit的Tag 值为8 + 8bit的值; 如果值在int16范围区间: 8bit的Tag 值为16 + 16bit的值; 如果值在int32范围区间: 8bit的Tag 值为32 + 32bit的值; 如果值在int64范围区间: 8bit的Tag 值为64 + 64bit的值;
    number：             如果值是整数则：编码规则同 integer类型; 如果是浮点数: 如果值在float32范围区间  8bit的Tag 值为33 + 32bit的值; 如果值在float64范围区间  8bit的Tag 值为65 + 64bit的值;    
    string：             16bit的Tag 存放的字符串长度(长度不包含字符串结尾符) + 字符串的内容(内容不包含结字符串尾符号)         
    float:               直接存放32bit的值
    double：             直接存放64bit的值
    bool：               占位8bit 如果为true 则存放的值为1 否则存放的值为0
    record(struct)：     如果是undefined 或者是空指针 则 8bit Tag 值为0, 否则 8bit的tag 值为1 + record的二进制数据
    list_+上面的数据类型的时候： 16bit的tag 用于存放 数组的长度 + 按数组顺序序列化每个元素的值的二进制数据(注意：如果列表类型为record(struct) 每个值序列化的时候并不会加 8bit的tag, 直接存record的二进制数据)

### maybe TODO
    lua 支持 integer number

### 关于消息接收转发解码和发送

	erlang通常会将接收到的消息由网关进程转发给其他工作进程， 建议先匹配消息id, 然后转发二进制消息到工作进程，然后由工作进程解码再处理
	同时广播消息可先编码成二进制之后再广播， 避免重复编码

### erl部分简单性能测评

	主要和gpb做简单对比测试
    gpb测试相关文件在test/gpb目录下
    测试协议：
    gpb:
    message test {
     	required string aa = 1;
     }
    
    message phoneNumber {
       	required test number = 1;
       	required int32 type  = 2;
    }
    
    message person {
      	required string name = 1;
      	required int32 integer = 2;
      	optional string email = 3;
      	repeated phoneNumber phone = 4;
    }
    
    message addressBook {
    	repeated  person person1 = 1;
    	repeated  person others = 2;
    }
    
    message tint32 {
        required int32 int1 = 1;
        required int32 int2 = 2;
        required int32 int3 = 3;
        required int32 int4 = 4;
        required int32 int5 = 5;
        required int32 int6 = 6;
        required int32 int7 = 7;
        required int32 int8 = 8;
        required int32 int9 = 9;
        required int32 int10 = 10;
    }
    genProto用的协议:
    
    test {
    	string aa;
    }
    
    phoneNumber{
        test number;
        int32 type;
    }
    
    person{
    	string name;
    	int32 id;
    	string email;
    	list[phoneNumber] phone;
    }
    
    addressBook {
        list[person] person;
    	list[person] other;
    }
    
    tint32{
        int32 int1;
        int32 int2;
        int32 int3;
        int32 int4;
        int32 int5;
        int32 int6;
        int32 int7;
        int32 int8;
        int32 int9;
        int32 int10;
    }
    测试运行三次 每次100万次循环
    tint32 gpb-------->>
        tint32 encode:
            > timer:tc(mytest, encode_int32, [1000000]).
            {9625000,ok}
            > timer:tc(mytest, encode_int32, [1000000]).
            {9000000,ok}
            > timer:tc(mytest, encode_int32, [1000000]).
            {9969000,ok}
            
        tin32 decode:
            > timer:tc(mytest, decode_int32, [1000000]).
            {6217994,ok}
            > timer:tc(mytest, decode_int32, [1000000]).
            {6187993,ok}
            > timer:tc(mytest, decode_int32, [1000000]).
            {6265994,ok}
            
        size:
            > BTInt32 = mytest:decode_int32(1).
            <<8,1,16,255,255,255,255,255,255,255,255,255,1,24,128,1,
              32,128,255,255,255,255,255,255,255,255,1,40,128,...>>
            31> byte_size(BTInt32).              
            74
    tint32 genProto ------->>
        tint32 encode:
            > timer:tc(test, encode_int32, [1000000]).
            {328999,ok}
            > timer:tc(test, encode_int32, [1000000]).
            {328000,ok}
            > timer:tc(test, encode_int32, [1000000]).
            {344000,ok}
        
        tint32 decode:
            > timer:tc(test, decode_int32, [1000000]).
            {328000,ok}
            > timer:tc(test, decode_int32, [1000000]).
            {328000,ok}
            > timer:tc(test, decode_int32, [1000000]).
            {329000,ok}
            
        size:
            > BTInt32 = test:decode_int32(1).  
            <<0,11,0,0,0,1,255,255,255,255,0,0,0,128,255,255,255,128,
              0,1,0,0,255,255,0,0,125,43,117,...>>
            > byte_size(BTInt32).
            42
    ===============================================================================
    ===============================================================================        
    addressBook gpb-------->>
        addressBook encode:
            > timer:tc(mytest, encode_addressBook, [1000000]).
            {9108990,ok}
            > timer:tc(mytest, encode_addressBook, [1000000]).
            {8999991,ok}
            > timer:tc(mytest, encode_addressBook, [1000000]).
            {9031991,ok}
    
        addressBook decode:
            > timer:tc(mytest, decode_addressBook, [1000000]).
            {5702995,ok}
            > timer:tc(mytest, decode_addressBook, [1000000]).
            {5764994,ok}
            > timer:tc(mytest, decode_addressBook, [1000000]).
            {5718995,ok}
        size:
            > BAddr = mytest:decode_addressBook(1).
            <<10,43,10,5,65,108,105,99,101,16,144,78,34,15,10,11,10,9,
              49,50,51,52,53,54,55,56,57,16,1,...>>
            > byte_size(BAddr).                    
            75
    addressBook genProto -------->>
        addressBook encode:
            > timer:tc(test, encode_addressBook, [1000000]).
            {4186995,ok}
            > timer:tc(test, encode_addressBook, [1000000]).
            {4202996,ok}
            > timer:tc(test, encode_addressBook, [1000000]).
            {4202996,ok}
        addressBook decode:
            > timer:tc(test, decode_addressBook, [1000000]).
            {2749997,ok}
            > timer:tc(test, decode_addressBook, [1000000]).
            {2812997,ok}
            > timer:tc(test, decode_addressBook, [1000000]).
            {2812997,ok}
        size:
            BAddr = test:decode_addressBook(1).  
            <<0,4,0,2,0,5,65,108,105,99,101,0,0,39,16,0,0,0,2,1,0,9,
              49,50,51,52,53,54,55,...>>
            67> byte_size(BAddr). 
            83

    

    
