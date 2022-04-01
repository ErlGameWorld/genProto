using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.IO;

namespace GenProto
{
    public static class ProtocolCore
    {
        public interface ISerialize
        {
            void Serialize(BinaryWriter binaryWriter);
            byte[] Serialize();
        }

        public interface IDeserialize<T>
        {
            void Deserialize(BinaryReader binaryReader);
            void Deserialize(byte[] data);
        }

        public enum BasicTypeEnum
        {
            Custom = 0x00,
            Boolean = 0x01,
            Int8 = 0x02,
            UInt8 = 0x03,
            UInt16 = 0x04,
            Int16 = 0x05,
            Int32 = 0x06,
            UInt32 = 0x07,
            Int64 = 0x08,
            UInt64 = 0x09,
            Float = 0x10,
            Double = 0x11,
            String = 0x12,
        }

        public static BasicTypeEnum JudgeType<T>(T value)
        {
            return value switch
            {
                bool => BasicTypeEnum.Boolean,
                byte => BasicTypeEnum.Int8,
                sbyte => BasicTypeEnum.UInt8,
                ushort => BasicTypeEnum.UInt16,
                short => BasicTypeEnum.Int16,
                int => BasicTypeEnum.Int32,
                uint => BasicTypeEnum.UInt32,
                long => BasicTypeEnum.Int64,
                ulong => BasicTypeEnum.UInt64,
                float => BasicTypeEnum.Float,
                double => BasicTypeEnum.Double,
                string => BasicTypeEnum.String,
                _ => BasicTypeEnum.Custom,
            };
        }

        public static void WriteValue<T>(this BinaryWriter binaryWriter, T value)
        {
            switch (value)
            {
                case bool boolValue:
                    binaryWriter.Write(boolValue);
                    break;
                case byte byteValue:
                    binaryWriter.Write(byteValue);
                    break;
                case sbyte sbyteValue:
                    binaryWriter.Write(sbyteValue);
                    break;
                case ushort ushortValue:
                    binaryWriter.Write(ushortValue);
                    break;
                case short shortValue:
                    binaryWriter.Write(shortValue);
                    break;
                case int intValue:
                    binaryWriter.Write(intValue);
                    break;
                case uint uintValue:
                    binaryWriter.Write(uintValue);
                    break;
                case long longValue:
                    binaryWriter.Write(longValue);
                    break;
                case ulong ulongValue:
                    binaryWriter.Write(ulongValue);
                    break;
                case float floatValue:
                    binaryWriter.Write(floatValue);
                    break;
                case double doubleValue:
                    binaryWriter.Write(doubleValue);
                    break;
                case string stringValue:
                    binaryWriter.Write(stringValue);
                    break;
                default:
                {
                    binaryWriter.Write(value != null);
                    switch (value)
                    {
                        case IList listValue:
                            binaryWriter.WriteList(listValue);
                            break;
                        case ISerialize serialize:
                            serialize.Serialize(binaryWriter);
                            break;
                        default:
                            if (value != null)
                            {
                                throw new InvalidOperationException($"unexpect type: {value.GetType().FullName}");
                            }

                            break;
                    }

                    break;
                }
            }
        }


        public static void WriteList(this BinaryWriter binaryWriter, IList list)
        {
            var length = (ushort) (list?.Count ?? 0);
            binaryWriter.Write(length);

            if (list == null) return;
            for (var idx = 0; idx < length; idx++)
            {
                var value = list[idx];

                if (idx == 0)
                {
                    var basicType = JudgeType(value);
                    binaryWriter.Write((byte) basicType);
                }

                binaryWriter.WriteValue(value);
            }
        }

        public static void ReadValue(this BinaryReader binaryReader, out bool value)
        {
            value = binaryReader.ReadBoolean();
        }

        public static void ReadValue(this BinaryReader binaryReader, out sbyte value)
        {
            value = binaryReader.ReadSByte();
        }

        public static void ReadValue(this BinaryReader binaryReader, out byte value)
        {
            value = binaryReader.ReadByte();
        }

        public static void ReadValue(this BinaryReader binaryReader, out ushort value)
        {
            value = binaryReader.ReadUInt16();
        }

        public static void ReadValue(this BinaryReader binaryReader, out short value)
        {
            value = binaryReader.ReadInt16();
        }

        public static void ReadValue(this BinaryReader binaryReader, out int value)
        {
            value = binaryReader.ReadInt32();
        }

        public static void ReadValue(this BinaryReader binaryReader, out uint value)
        {
            value = binaryReader.ReadUInt32();
        }

        public static void ReadValue(this BinaryReader binaryReader, out long value)
        {
            value = binaryReader.ReadInt64();
        }

        public static void ReadValue(this BinaryReader binaryReader, out ulong value)
        {
            value = binaryReader.ReadUInt64();
        }

        public static void ReadValue(this BinaryReader binaryReader, out float value)
        {
            value = binaryReader.ReadSingle();
        }

        public static void ReadValue(this BinaryReader binaryReader, out double value)
        {
            value = binaryReader.ReadDouble();
        }

        public static void ReadValue(this BinaryReader binaryReader, out string value)
        {
            value = binaryReader.ReadString();
        }

        public static void ReadValue<T>(this BinaryReader binaryReader, out T value) where T : new()
        {
            value = default;
            var haveValue = binaryReader.ReadBoolean();
            if (!haveValue)
            {
                return;
            }

            value = new T();
            if (!(value is IDeserialize<T> deserialize))
            {
                throw new InvalidOperationException($"error type: {typeof(T).FullName}");
            }

            deserialize.Deserialize(binaryReader);
        }

        public static void ReadValue<T>(this BinaryReader binaryReader, out List<T> outList) where T : new()
        {
            outList = default;
            IList list = default;
            var haveValue = binaryReader.ReadBoolean();
            if (!haveValue)
            {
                return;
            }

            var length = binaryReader.ReadUInt16();
            if (length <= 0)
            {
                return;
            }

            var basicTypeEnum = (BasicTypeEnum) binaryReader.ReadByte();
            for (var idx = 0; idx < length; idx++)
            {
                switch (basicTypeEnum)
                {
                    case BasicTypeEnum.Boolean:
                        list ??= new List<bool>(length);
                        var boolValue = binaryReader.ReadBoolean();
                        list.Add(boolValue);
                        break;
                    case BasicTypeEnum.Int8:
                        list ??= new List<sbyte>(length);
                        var sbyteValue = binaryReader.ReadSByte();
                        list.Add(sbyteValue);
                        break;
                    case BasicTypeEnum.UInt8:
                        list ??= new List<byte>(length);
                        var byteValue = binaryReader.ReadByte();
                        list.Add(byteValue);
                        break;
                    case BasicTypeEnum.UInt16:
                        list ??= new List<ushort>(length);
                        var ushortValue = binaryReader.ReadUInt16();
                        list.Add(ushortValue);
                        break;
                    case BasicTypeEnum.Int16:
                        list ??= new List<short>(length);
                        var shortValue = binaryReader.ReadInt16();
                        list.Add(shortValue);
                        break;
                    case BasicTypeEnum.Int32:
                        list ??= new List<int>(length);
                        var intValue = binaryReader.ReadInt32();
                        list.Add(intValue);
                        break;
                    case BasicTypeEnum.UInt32:
                        list ??= new List<uint>(length);
                        var uintValue = binaryReader.ReadUInt32();
                        list.Add(uintValue);
                        break;
                    case BasicTypeEnum.Int64:
                        list ??= new List<long>(length);
                        var longValue = binaryReader.ReadInt64();
                        list.Add(longValue);
                        break;
                    case BasicTypeEnum.UInt64:
                        list ??= new List<ulong>(length);
                        var ulongValue = binaryReader.ReadUInt64();
                        list.Add(ulongValue);
                        break;
                    case BasicTypeEnum.Float:
                        list ??= new List<float>(length);
                        var singleValue = binaryReader.ReadSingle();
                        list.Add(singleValue);
                        break;
                    case BasicTypeEnum.Double:
                        list ??= new List<double>(length);
                        var doubleValue = binaryReader.ReadDouble();
                        list.Add(doubleValue);
                        break;
                    case BasicTypeEnum.String:
                        list ??= new List<string>(length);
                        var stringValue = binaryReader.ReadString();
                        list.Add(stringValue);
                        break;
                    case BasicTypeEnum.Custom:
                        list ??= new List<T>(length);
                        var state = binaryReader.ReadBoolean();
                        if (state)
                        {
                            if (new T() is IDeserialize<T> item)
                            {
                                item.Deserialize(binaryReader);
                                list.Add(item);
                            }
                        }

                        break;


                    default:
                        throw new InvalidOperationException();
                }
            }

            outList = list as List<T>;
        }
    }	public class test : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<test>
	{
		public const int ProtoId = 1;
		public string aa;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(aa);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out aa);
		}
	}
	public class phoneNumber : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<phoneNumber>
	{
		public const int ProtoId = 2;
		public test number;
		public int type;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(number);
			binaryWriter.WriteValue(type);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out number);
			binaryReader.ReadValue(out type);
		}
	}
	public class person : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<person>
	{
		public const int ProtoId = 3;
		public string name;
		public int id;
		public string email;
		public List<phoneNumber> phone;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(name);
			binaryWriter.WriteValue(id);
			binaryWriter.WriteValue(email);
			binaryWriter.WriteValue(phone);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out name);
			binaryReader.ReadValue(out id);
			binaryReader.ReadValue(out email);
			binaryReader.ReadValue(out phone);
		}
	}
	public class addressBook : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<addressBook>
	{
		public const int ProtoId = 4;
		public List<person> person;
		public List<person> other;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(person);
			binaryWriter.WriteValue(other);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out person);
			binaryReader.ReadValue(out other);
		}
	}
	public class union : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<union>
	{
		public const int ProtoId = 5;
		public string test;
		public int type;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(test);
			binaryWriter.WriteValue(type);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out test);
			binaryReader.ReadValue(out type);
		}
	}
	public class tbool : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tbool>
	{
		public const int ProtoId = 6;
		public bool bool;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(bool);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out bool);
		}
	}
	public class tint8 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tint8>
	{
		public const int ProtoId = 7;
		public sbyte int1;
		public sbyte int2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
		}
	}
	public class tuint8 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tuint8>
	{
		public const int ProtoId = 8;
		public byte int1;
		public byte int2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
		}
	}
	public class tint16 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tint16>
	{
		public const int ProtoId = 9;
		public short int1;
		public short int2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
		}
	}
	public class tuint16 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tuint16>
	{
		public const int ProtoId = 10;
		public ushort int1;
		public ushort int2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
		}
	}
	public class tint32 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tint32>
	{
		public const int ProtoId = 11;
		public int int1;
		public int int2;
		public int int3;
		public int int4;
		public int int5;
		public int int6;
		public int int7;
		public int int8;
		public int int9;
		public int int10;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
			binaryWriter.WriteValue(int3);
			binaryWriter.WriteValue(int4);
			binaryWriter.WriteValue(int5);
			binaryWriter.WriteValue(int6);
			binaryWriter.WriteValue(int7);
			binaryWriter.WriteValue(int8);
			binaryWriter.WriteValue(int9);
			binaryWriter.WriteValue(int10);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
			binaryReader.ReadValue(out int3);
			binaryReader.ReadValue(out int4);
			binaryReader.ReadValue(out int5);
			binaryReader.ReadValue(out int6);
			binaryReader.ReadValue(out int7);
			binaryReader.ReadValue(out int8);
			binaryReader.ReadValue(out int9);
			binaryReader.ReadValue(out int10);
		}
	}
	public class tuint32 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tuint32>
	{
		public const int ProtoId = 12;
		public uint int1;
		public uint int2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
		}
	}
	public class tint64 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tint64>
	{
		public const int ProtoId = 13;
		public long int1;
		public long int2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
		}
	}
	public class tuint64 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tuint64>
	{
		public const int ProtoId = 14;
		public ulong int1;
		public ulong int2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
		}
	}
	public class tinteger : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tinteger>
	{
		public const int ProtoId = 15;
		public integer int1;
		public integer int2;
		public integer int3;
		public integer int4;
		public integer int5;
		public integer int6;
		public integer int7;
		public integer int8;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
			binaryWriter.WriteValue(int3);
			binaryWriter.WriteValue(int4);
			binaryWriter.WriteValue(int5);
			binaryWriter.WriteValue(int6);
			binaryWriter.WriteValue(int7);
			binaryWriter.WriteValue(int8);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
			binaryReader.ReadValue(out int3);
			binaryReader.ReadValue(out int4);
			binaryReader.ReadValue(out int5);
			binaryReader.ReadValue(out int6);
			binaryReader.ReadValue(out int7);
			binaryReader.ReadValue(out int8);
		}
	}
	public class tnumber : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tnumber>
	{
		public const int ProtoId = 16;
		public number int1;
		public number int2;
		public number int3;
		public number int4;
		public number int5;
		public number int6;
		public number int7;
		public number int8;
		public number float1;
		public number float2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
			binaryWriter.WriteValue(int3);
			binaryWriter.WriteValue(int4);
			binaryWriter.WriteValue(int5);
			binaryWriter.WriteValue(int6);
			binaryWriter.WriteValue(int7);
			binaryWriter.WriteValue(int8);
			binaryWriter.WriteValue(float1);
			binaryWriter.WriteValue(float2);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
			binaryReader.ReadValue(out int3);
			binaryReader.ReadValue(out int4);
			binaryReader.ReadValue(out int5);
			binaryReader.ReadValue(out int6);
			binaryReader.ReadValue(out int7);
			binaryReader.ReadValue(out int8);
			binaryReader.ReadValue(out float1);
			binaryReader.ReadValue(out float2);
		}
	}
	public class tfloat : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tfloat>
	{
		public const int ProtoId = 17;
		public float int1;
		public float int2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
		}
	}
	public class tdouble : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tdouble>
	{
		public const int ProtoId = 18;
		public double int1;
		public double int2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
		}
	}
	public class tstring : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tstring>
	{
		public const int ProtoId = 19;
		public string int1;
		public string int2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
		}
	}
	public class tlistbool : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistbool>
	{
		public const int ProtoId = 20;
		public List<bool> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistint8 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistint8>
	{
		public const int ProtoId = 21;
		public List<sbyte> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistuint8 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistuint8>
	{
		public const int ProtoId = 22;
		public List<byte> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistint16 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistint16>
	{
		public const int ProtoId = 23;
		public List<short> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistuint16 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistuint16>
	{
		public const int ProtoId = 24;
		public List<ushort> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistint32 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistint32>
	{
		public const int ProtoId = 25;
		public List<int> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistuint32 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistuint32>
	{
		public const int ProtoId = 26;
		public List<uint> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistint64 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistint64>
	{
		public const int ProtoId = 27;
		public List<long> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistuint64 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistuint64>
	{
		public const int ProtoId = 28;
		public List<ulong> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistinteger : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistinteger>
	{
		public const int ProtoId = 29;
		public List<integer> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistnumber : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistnumber>
	{
		public const int ProtoId = 30;
		public List<number> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistfloat : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistfloat>
	{
		public const int ProtoId = 31;
		public List<float> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistdouble : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistdouble>
	{
		public const int ProtoId = 32;
		public List<double> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tliststring : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tliststring>
	{
		public const int ProtoId = 33;
		public List<string> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class tlistunion : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistunion>
	{
		public const int ProtoId = 34;
		public List<union> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
		}
	}
	public class allType : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<allType>
	{
		public const int ProtoId = 35;
		public bool bool;
		public sbyte int8;
		public byte uint8;
		public short int16;
		public ushort uint16;
		public int int32;
		public uint uint32;
		public long int64;
		public ulong uint64;
		public integer inte8;
		public integer uinte8;
		public integer inte16;
		public integer uinte16;
		public integer inte32;
		public integer uinte32;
		public integer inte64;
		public integer uinte64;
		public number num8;
		public number unum8;
		public number num16;
		public number unum16;
		public number num32;
		public number unum32;
		public number num64;
		public number unum64;
		public number numfloat;
		public number numdouble;
		public float float;
		public double double;
		public string string1;
		public string string2;
		public union union;
		public List<bool> lbool;
		public List<sbyte> lint8;
		public List<byte> luint8;
		public List<short> lint16;
		public List<ushort> luint16;
		public List<int> lint32;
		public List<uint> luint32;
		public List<long> lint64;
		public List<ulong> luint64;
		public List<integer> linte8;
		public List<integer> linte16;
		public List<integer> linte32;
		public List<integer> linte64;
		public List<number> lnum8;
		public List<number> lnum16;
		public List<number> lnum32;
		public List<number> lnum64;
		public List<number> lnfloat32;
		public List<number> lnfloat64;
		public List<float> lfloat;
		public List<double> ldouble;
		public List<string> lstring;
		public List<union> lunion;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(bool);
			binaryWriter.WriteValue(int8);
			binaryWriter.WriteValue(uint8);
			binaryWriter.WriteValue(int16);
			binaryWriter.WriteValue(uint16);
			binaryWriter.WriteValue(int32);
			binaryWriter.WriteValue(uint32);
			binaryWriter.WriteValue(int64);
			binaryWriter.WriteValue(uint64);
			binaryWriter.WriteValue(inte8);
			binaryWriter.WriteValue(uinte8);
			binaryWriter.WriteValue(inte16);
			binaryWriter.WriteValue(uinte16);
			binaryWriter.WriteValue(inte32);
			binaryWriter.WriteValue(uinte32);
			binaryWriter.WriteValue(inte64);
			binaryWriter.WriteValue(uinte64);
			binaryWriter.WriteValue(num8);
			binaryWriter.WriteValue(unum8);
			binaryWriter.WriteValue(num16);
			binaryWriter.WriteValue(unum16);
			binaryWriter.WriteValue(num32);
			binaryWriter.WriteValue(unum32);
			binaryWriter.WriteValue(num64);
			binaryWriter.WriteValue(unum64);
			binaryWriter.WriteValue(numfloat);
			binaryWriter.WriteValue(numdouble);
			binaryWriter.WriteValue(float);
			binaryWriter.WriteValue(double);
			binaryWriter.WriteValue(string1);
			binaryWriter.WriteValue(string2);
			binaryWriter.WriteValue(union);
			binaryWriter.WriteValue(lbool);
			binaryWriter.WriteValue(lint8);
			binaryWriter.WriteValue(luint8);
			binaryWriter.WriteValue(lint16);
			binaryWriter.WriteValue(luint16);
			binaryWriter.WriteValue(lint32);
			binaryWriter.WriteValue(luint32);
			binaryWriter.WriteValue(lint64);
			binaryWriter.WriteValue(luint64);
			binaryWriter.WriteValue(linte8);
			binaryWriter.WriteValue(linte16);
			binaryWriter.WriteValue(linte32);
			binaryWriter.WriteValue(linte64);
			binaryWriter.WriteValue(lnum8);
			binaryWriter.WriteValue(lnum16);
			binaryWriter.WriteValue(lnum32);
			binaryWriter.WriteValue(lnum64);
			binaryWriter.WriteValue(lnfloat32);
			binaryWriter.WriteValue(lnfloat64);
			binaryWriter.WriteValue(lfloat);
			binaryWriter.WriteValue(ldouble);
			binaryWriter.WriteValue(lstring);
			binaryWriter.WriteValue(lunion);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out bool);
			binaryReader.ReadValue(out int8);
			binaryReader.ReadValue(out uint8);
			binaryReader.ReadValue(out int16);
			binaryReader.ReadValue(out uint16);
			binaryReader.ReadValue(out int32);
			binaryReader.ReadValue(out uint32);
			binaryReader.ReadValue(out int64);
			binaryReader.ReadValue(out uint64);
			binaryReader.ReadValue(out inte8);
			binaryReader.ReadValue(out uinte8);
			binaryReader.ReadValue(out inte16);
			binaryReader.ReadValue(out uinte16);
			binaryReader.ReadValue(out inte32);
			binaryReader.ReadValue(out uinte32);
			binaryReader.ReadValue(out inte64);
			binaryReader.ReadValue(out uinte64);
			binaryReader.ReadValue(out num8);
			binaryReader.ReadValue(out unum8);
			binaryReader.ReadValue(out num16);
			binaryReader.ReadValue(out unum16);
			binaryReader.ReadValue(out num32);
			binaryReader.ReadValue(out unum32);
			binaryReader.ReadValue(out num64);
			binaryReader.ReadValue(out unum64);
			binaryReader.ReadValue(out numfloat);
			binaryReader.ReadValue(out numdouble);
			binaryReader.ReadValue(out float);
			binaryReader.ReadValue(out double);
			binaryReader.ReadValue(out string1);
			binaryReader.ReadValue(out string2);
			binaryReader.ReadValue(out union);
			binaryReader.ReadValue(out lbool);
			binaryReader.ReadValue(out lint8);
			binaryReader.ReadValue(out luint8);
			binaryReader.ReadValue(out lint16);
			binaryReader.ReadValue(out luint16);
			binaryReader.ReadValue(out lint32);
			binaryReader.ReadValue(out luint32);
			binaryReader.ReadValue(out lint64);
			binaryReader.ReadValue(out luint64);
			binaryReader.ReadValue(out linte8);
			binaryReader.ReadValue(out linte16);
			binaryReader.ReadValue(out linte32);
			binaryReader.ReadValue(out linte64);
			binaryReader.ReadValue(out lnum8);
			binaryReader.ReadValue(out lnum16);
			binaryReader.ReadValue(out lnum32);
			binaryReader.ReadValue(out lnum64);
			binaryReader.ReadValue(out lnfloat32);
			binaryReader.ReadValue(out lnfloat64);
			binaryReader.ReadValue(out lfloat);
			binaryReader.ReadValue(out ldouble);
			binaryReader.ReadValue(out lstring);
			binaryReader.ReadValue(out lunion);
		}
	}
	public class testnull : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<testnull>
	{
		public const int ProtoId = 36;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
		}
		public void Deserialize(BinaryReader binaryReader)
		{
		}
	}
	public class person1 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<person1>
	{
		public const int ProtoId = 1001;
		public string name;
		public int id;
		public string email;
		public List<phoneNumber> phone;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new BinaryWriter(memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new BinaryReader(memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(BinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(name);
			binaryWriter.WriteValue(id);
			binaryWriter.WriteValue(email);
			binaryWriter.WriteValue(phone);
		}
		public void Deserialize(BinaryReader binaryReader)
		{
			binaryReader.ReadValue(out name);
			binaryReader.ReadValue(out id);
			binaryReader.ReadValue(out email);
			binaryReader.ReadValue(out phone);
		}
	}
}