genProto
=====

An OTP library

Build
-----
    $ rebar compile

简单描述 
    之前一直想找个可用于erlang游戏或者erlang其他网络应用的协议解析编码解码的项目,且希望具有较高的编码解码性能，
    大概瞄过 flatc, ecannp以及gpb(erlang protobuff)协议, 最后还是决定自己来写个，就当着写着玩吧，以后有机会
    也可以用上。由于时间关系，自己只简单评测，测试编码解码性能稍微比gpb好点, 更加详细的，感兴趣可自行评测。我已经尽量
    优化了编码解码的代码使其编码解码速度更快,但是实际上没达到我的预期。
    开源此项目的目的是希望有人可以来review并提出优化的方法。
    协议文件存放在proto目录 文件名为 Message protocol definition file的缩写 mpdf
    源码目录为src,如果想要浏览解码编码的代码主要看protoCode.erl即可， 其他文件都是辅助生成协议的代码,
    test目录主要用于生成协议和测试的脚本以及用于测试的代码，有建议提issue
    
     