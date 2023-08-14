using GenProto;

phoneNumber phoneNumber = new phoneNumber()
{
    number = new test { aa = "hello" },
    type = 100
};

var data =  phoneNumber.Serialize();

var ss = new phoneNumber();
ss.Deserialize(data);

Console.WriteLine("Hello, World!");