%% 测试协议 注释可以用 % 也可以用 //
test {
	string aa;
}

// phoneNumber info
phoneNumber{
    test number;   //电话号码
    int32 type;    //类型
}

%% person info
person{
	string name;    %% 名字
	int32 id;       %% id
	string email;   //邮箱
	list[phoneNumber] phone;   // list of phoneNumber
}

%% addressBook info
addressBook {
    list[person] person;
	list[person] other;
}
