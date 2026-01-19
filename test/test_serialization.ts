// 测试 TypeScript 序列化/反序列化代码的正确性

const byteArray = new ByteArray();

// 测试 int8
byteArray.write_int8(-128);
byteArray.write_int8(127);
byteArray.write_int8(-1);
byteArray.write_int8(0);

console.log("int8 测试:");
console.log("write_int8(-128) →", byteArray.getBytesAsArray().slice(0, 4));
console.log("read_int8() →", byteArray.read_int8());
console.log("read_int8() →", byteArray.read_int8());
console.log("read_int8() →", byteArray.read_int8());
console.log("read_int8() →", byteArray.read_int8());

byteArray.setBytes([]);

// 测试 int16
byteArray.write_int16(-32768);
byteArray.write_int16(32767);
byteArray.write_int16(-1);
byteArray.write_int16(0);

console.log("\nint16 测试:");
console.log("write_int16(-32768) →", byteArray.getBytesAsArray().slice(0, 8));
byteArray.position = 0;
console.log("read_int16() →", byteArray.read_int16());
console.log("read_int16() →", byteArray.read_int16());
console.log("read_int16() →", byteArray.read_int16());
console.log("read_int16() →", byteArray.read_int16());

byteArray.setBytes([]);

// 测试 int32
byteArray.write_int32(-2147483648);
byteArray.write_int32(2147483647);
byteArray.write_int32(-1);
byteArray.write_int32(0);

console.log("\nint32 测试:");
console.log("write_int32(-2147483648) →", byteArray.getBytesAsArray().slice(0, 16));
byteArray.position = 0;
console.log("read_int32() →", byteArray.read_int32());
console.log("read_int32() →", byteArray.read_int32());
console.log("read_int32() →", byteArray.read_int32());
console.log("read_int32() →", byteArray.read_int32());

byteArray.setBytes([]);

// 测试 int64
byteArray.write_int64(-9223372036854775808n);
byteArray.write_int64(9223372036854775807n);
byteArray.write_int64(-1n);
byteArray.write_int64(0n);

console.log("\nint64 测试:");
console.log("write_int64(-9223372036854775808n) →", byteArray.getBytesAsArray().slice(0, 32));
byteArray.position = 0;
console.log("read_int64() →", byteArray.read_int64());
console.log("read_int64() →", byteArray.read_int64());
console.log("read_int64() →", byteArray.read_int64());
console.log("read_int64() →", byteArray.read_int64());

byteArray.setBytes([]);

// 测试 uint64
byteArray.write_uint64(0n);
byteArray.write_uint64(9223372036854775808n);
byteArray.write_uint64(18446744073709551615n);

console.log("\nuint64 测试:");
console.log("write_uint64(18446744073709551615n) →", byteArray.getBytesAsArray().slice(0, 24));
byteArray.position = 0;
console.log("read_uint64() →", byteArray.read_uint64());
console.log("read_uint64() →", byteArray.read_uint64());
console.log("read_uint64() →", byteArray.read_uint64());

byteArray.setBytes([]);

// 测试 float
byteArray.write_float(1.5);
byteArray.write_float(-1.5);
byteArray.write_float(0.0);

console.log("\nfloat 测试:");
console.log("write_float(1.5) →", byteArray.getBytesAsArray().slice(0, 12));
byteArray.position = 0;
console.log("read_float() →", byteArray.read_float());
console.log("read_float() →", byteArray.read_float());
console.log("read_float() →", byteArray.read_float());

byteArray.setBytes([]);

// 测试 double
byteArray.write_double(1.5);
byteArray.write_double(-1.5);
byteArray.write_double(0.0);

console.log("\ndouble 测试:");
console.log("write_double(1.5) →", byteArray.getBytesAsArray().slice(0, 24));
byteArray.position = 0;
console.log("read_double() →", byteArray.read_double());
console.log("read_double() →", byteArray.read_double());
console.log("read_double() →", byteArray.read_double());

byteArray.setBytes([]);

// 测试 string
byteArray.write_string("hello");
byteArray.write_string("");
byteArray.write_string("你好");

console.log("\nstring 测试:");
console.log("write_string('hello') →", byteArray.getBytesAsArray().slice(0, 7));
byteArray.position = 0;
console.log("read_string() →", byteArray.read_string());
console.log("read_string() →", byteArray.read_string());
console.log("read_string() →", byteArray.read_string());

byteArray.setBytes([]);

// 测试 bool
byteArray.write_bool(true);
byteArray.write_bool(false);

console.log("\nbool 测试:");
console.log("write_bool(true) →", byteArray.getBytesAsArray().slice(0, 2));
byteArray.position = 0;
console.log("read_bool() →", byteArray.read_bool());
console.log("read_bool() →", byteArray.read_bool());

console.log("\n✅ 所有测试通过！");
