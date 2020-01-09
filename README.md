genProto
=====

An OTP library

Build
-----
    $ rebar compile

# 简单描述 
    之前一直想找个可用于erlang游戏或者erlang其他网络应用的协议解析编码解码的项目,且希望具有较高的编码解码性能，
    大概瞄过 flatc, ecapnp以及gpb(erlang protobuff)协议, 最后还是决定自己来写个，以后有机会
    也可以用上。由于时间关系，自己只简单评测, 更加详细的，感兴趣可自行评测。已经尽量
    优化了编码解码的代码使其编码解码速度更快
    开源此项目的目的是希望有人可以来review并提出优化的方法。
    协议文件存放在proto目录 文件名为 Message protocol definition file的缩写 mpdf
    源码目录为src，test目录主要用于生成协议和测试的脚本以及用于测试的代码，有建议提issue,谢谢！
    
## 二进制语法
	Bin = <<E1, E2, ... En>>
	<<E1, E2, ... En>> = Bin
	每个E 1..n 指定bitstring的Segment
	每个段具有以下一般语法：
	Value:Size/TypeSpecifierList
	可以省略Size或TypeSpecifier或两者。因此，允许以下变体：
	Ei =
		Value |
		Value:Size |
		Value/TypeSpecifierList |
		Value:Size/TypeSpecifierList
### Value	
	当在二进制构造中使用时，Value部分是任何表达式，用于求值为整数，浮点或位串，如果表达式不是单个文字或变量，则将其括在括号中。
	在二进制匹配中使用时，用于位串匹配，Value必须是变量，或整数，浮点或字符串，简单而言就是Value部分必须是文字或变量。
### Size
	在位串构造中使用，Size是要求求整数的表达式。
	用于位串匹配，Size必须是整数，或绑定到整数的变量。
	Size的值以Unit指定段的大小，默认值取决于类型
		• For integer it is 8.
		• For float it is 64.
		• For binary and bitstring it is the whole binary or bit string.
		在匹配中，此默认值仅对最后一个元素有效。匹配中的所有其他位串或二进制元素必须具有大小规范,段的大小Size部分乘以TypeSpecifierList中的unit(稍后描述)给出了段的位数.对于utf8，utf16和utf32类型，不得给出Size的大小。段的大小由类型和值本身隐式确定。
		
### TypeSpecifierList
	是一个类型说明符列表，按任何顺序，用连字符("-")分隔。默认值用于任何省略的类型说明符
#### Type
	Type= integer | float | binary | bytes | bitstring | bits | utf8 | utf16 | utf32	
	默认值为integer。bytes是二进制的简写，bits是bitstring的简写。有关utf类型的更多信息，请参见下文。
#### Signedness
	Signedness= signed | unsigned
	只有匹配和类型为整数时才有意义。默认值为无符号。
#### Endianness
	Endianness= big | little | native
	Native-endian意味着字节顺序在加载时被解析为big-endian或little-endian，具体取决于运行Erlang机器的CPU的本机内容。仅当Type为integer，utf16，utf32或float时，字节顺序才有意义。默认值为big。
#### Unit
	Unit= unit:IntegerLiteral
	允许的范围是1..256。对于integer，float和bitstring，默认值为1;对于binary，默认值为8。对于utf8，utf16和utf32类型，不能给出Unit说明符。
	它与Size说明符相乘，以给出段的有效大小。单位大小指定没有大小的二进制段的对齐方式，二进制类型的段必须具有可被8整除的大小
	
### 注意
	构造二进制文件时，如果整数段的大小N太小而不能包含给定的整数，则整数的最高有效位将被静默丢弃，并且只有N个最低有效位被放入二进制。	
#### 例子：
	X:4/little-signed-integer-unit:8
	该元素的总大小为4 * 8 = 32位，它包含一个小端序的有符号整数
	
### 关于 utf8 utf16 utf32
	构造utf类型的段时，Value必须是0..16＃D7FF或16＃E000 .... 16＃10FFFF范围内的整数。如果Value超出允许范围，则构造将失败并返回badarg异常。生成的二进制段的大小取决于类型或值，或两者：
	• For utf8, Value is encoded in 1-4 bytes.
	• For utf16, Value is encoded in 2 or 4 bytes.
	• For utf32, Value is always be encoded in 4 bytes.	
	构造时，可以给出一个文字字符串，后跟一个UTF类型，例如：<<“abc”/ utf8 >>，这是<< $ a / utf8，$ b / utf8，$ c / utf8的语法糖>>。
	成功匹配utf类型的段，得到0..16＃D7FF或16＃E000..16＃10FFFF范围内的整数。
    如果返回值超出这些范围，则匹配失败。


### 如何实现二进制文件
	在内部，二进制和位串以相同的方式实现。
	内部有四种类型的二进制对象：
	两个是二进制数据的容器，称为：
		• Refc binaries (short for reference-counted binaries)
		• Heap binaries
	两个仅仅是对二进制文件的一部分的引用，被称为：
		• sub binaries 
		• match contexts
	
### Refc Binaries
	Refc二进制文件由两部分组成：
	•存储在进程堆上的对象，称为ProcBin
	•二进制对象本身，存储在所有进程堆之外	
	任何数量的进程都可以通过任意数量的ProcBins引用二进制对象。该对象包含一个引用计数器，用于跟踪引用的数量，以便在最后一个引用消失时将其删除。
	进程中的所有ProcBin对象都是链表的一部分，因此当ProcBin消失时，垃圾收集器可以跟踪它们并减少二进制文件中的引用计数器。
	
### Heap Binaries
	堆二进制文件是小型二进制文件，最多64个字节，并直接存储在进程堆上。它们在进程被垃圾收集时以及作为消息发送时被复制。它们不需要垃圾收集器进行任何特殊处理。
### Sub Binaries	
	The reference objects sub binaries and match contexts can reference part of a refc binary or heap binary
	子二进制文件由split_binary / 2创建或者当二进制文件以二进制模式匹配时。子二进制是对另一个二进制文件（refc或堆二进制文件的一部分，但从不进入另一个子二进制文件）的引用。因此，匹配二进制文件相对便宜，因为实际的二进制数据永远不会被复制。
### Match Context
	匹配上下文类似于子二进制，但针对二进制匹配进行了优化
	
### 关于iolist
    定义(直接引用霸业的文章)
    1. [] 
    2. binary
    3. 列表, 每个元素是int(0-255)或者binary或者iolist.
    其中binary是指 bitsize % 8 == 0 .
    int 是0-255 
    Iolist的作用是用于往port送数据的时候.由于底层的系统调用如writev支持向量写, 就避免了无谓的iolist_to_binary这样的扁平话操作, 避免了内存拷贝,极大的提高了效率.
    另外额外补充：
    erlang中列表时在头部添加比较高效，但是binary是在尾部追加更高效

### 关于消息接收转发解码和发送
	erlang通常会将接收到的消息由网关进程转发给其他工作进程， 建议先匹配消息id, 然后转发二进制消息到工作进程，然后由工作进程解码再处理
	同时广播消息可先编码成二进制之后再广播， 避免重复编码
	
### 简单性能测评
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

### TODO
    生成的protoMsg.erl
    encode 函数参数列表太长时 换行显示
    encode返回的编码列表参数太多时 换行显示
    decodeBin simple类型解码列表过长时 换行显示
    decodeBin 返回元组元素太多时 换行显示

    

    
