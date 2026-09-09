#pragma warning disable CS8981 // mpdf message names are lowercase by convention
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.IO;
using MiscUtil.Conversion;
using MiscUtil.IO;

namespace GenProto
{
    public static class ProtocolCore
    {
        public interface ISerialize
        {
            void Serialize(EndianBinaryWriter binaryWriter);
            byte[] Serialize();
        }

        public interface IDeserialize<T>
        {
            void Deserialize(EndianBinaryReader binaryReader);
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
            Binary = 0x13,
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

        public static void WriteValue<T>(this EndianBinaryWriter binaryWriter, T value)
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
                    var bytesLength = (ushort)binaryWriter.Encoding.GetByteCount(stringValue);
                    binaryWriter.Write(bytesLength);
                    var bytes = binaryWriter.Encoding.GetBytes(stringValue);
                    binaryWriter.Write(bytes);
                    break;
                case byte[] binaryValue:
                    var binLen = (ushort)(binaryValue?.Length ?? 0);
                    binaryWriter.Write(binLen);
                    if (binaryValue != null && binLen > 0)
                    {
                        binaryWriter.Write(binaryValue);
                    }
                    break;
                default:
                {
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


        public static void WriteList(this EndianBinaryWriter binaryWriter, IList list)
        {
            var length = (ushort)(list?.Count ?? 0);
            binaryWriter.Write(length);

            if (list == null) return;
            for (var idx = 0; idx < length; idx++)
            {
                var value = list[idx];
                binaryWriter.WriteValue(value);
            }
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out bool value)
        {
            value = binaryReader.ReadBoolean();
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out sbyte value)
        {
            value = binaryReader.ReadSByte();
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out byte value)
        {
            value = binaryReader.ReadByte();
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out ushort value)
        {
            value = binaryReader.ReadUInt16();
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out short value)
        {
            value = binaryReader.ReadInt16();
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out int value)
        {
            value = binaryReader.ReadInt32();
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out uint value)
        {
            value = binaryReader.ReadUInt32();
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out long value)
        {
            value = binaryReader.ReadInt64();
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out ulong value)
        {
            value = binaryReader.ReadUInt64();
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out float value)
        {
            value = binaryReader.ReadSingle();
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out double value)
        {
            value = binaryReader.ReadDouble();
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out string value)
        {
            var bytesLength = binaryReader.ReadUInt16();
            var bytes = binaryReader.ReadBytes(bytesLength);
            value = binaryReader.Encoding.GetString(bytes, 0, bytes.Length);
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out byte[] value)
        {
            var bytesLength = binaryReader.ReadUInt16();
            value = binaryReader.ReadBytes(bytesLength);
        }

        public static void ReadValue<T>(this EndianBinaryReader binaryReader, out T value) where T : new()
        {
            value = default;
            value = new T();
            if (value is not IDeserialize<T> deserialize)
            {
                throw new InvalidOperationException($"error type: {typeof(T).FullName}");
            }

            deserialize.Deserialize(binaryReader);
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out List<string> outList, BasicTypeEnum basicTypeEnum)
        {
            outList = null;
            var length = binaryReader.ReadUInt16();
            if (length <= 0)
            {
                return;
            }

            outList = new List<string>(length);
            for (var idx = 0; idx < length; idx++)
            {
                binaryReader.ReadValue(out string item);
                outList.Add(item);
            }
        }

        public static void ReadValue(this EndianBinaryReader binaryReader, out List<byte[]> outList, BasicTypeEnum basicTypeEnum)
        {
            outList = null;
            var length = binaryReader.ReadUInt16();
            if (length <= 0)
            {
                return;
            }

            outList = new List<byte[]>(length);
            for (var idx = 0; idx < length; idx++)
            {
                binaryReader.ReadValue(out byte[] item);
                outList.Add(item);
            }
        }

        public static void ReadValue<T>(this EndianBinaryReader binaryReader, out List<T> outList, BasicTypeEnum basicTypeEnum) where T : new()
        {
            outList = default;
            IList list = default;

            var length = binaryReader.ReadUInt16();
            if (length <= 0)
            {
                return;
            }

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
                    case BasicTypeEnum.Binary:
                        list ??= new List<byte[]>(length);
                        var binLen = binaryReader.ReadUInt16();
                        list.Add(binaryReader.ReadBytes(binLen));
                        break;
                    case BasicTypeEnum.Custom:
                        list ??= new List<T>(length);
                        if (new T() is IDeserialize<T> item)
                            {
                                item.Deserialize(binaryReader);
                                list.Add(item);
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(aa);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			if (number != null)
			{
				binaryWriter.Write((byte)1);
				number.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			binaryWriter.WriteValue(type);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			if (binaryReader.ReadByte() != 0)
			{
				number = new test();
				number.Deserialize(binaryReader);
			}
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(name);
			binaryWriter.WriteValue(id);
			binaryWriter.WriteValue(email);
			binaryWriter.WriteValue(phone);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out name);
			binaryReader.ReadValue(out id);
			binaryReader.ReadValue(out email);
			binaryReader.ReadValue(out phone, ProtocolCore.BasicTypeEnum.Custom);
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(person);
			binaryWriter.WriteValue(other);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out person, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out other, ProtocolCore.BasicTypeEnum.Custom);
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(test);
			binaryWriter.WriteValue(type);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out test);
			binaryReader.ReadValue(out type);
		}
	}
	public class tbool : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tbool>
	{
		public const int ProtoId = 6;
		public bool flag;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(flag);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out flag);
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
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
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1);
			binaryReader.ReadValue(out int2);
		}
	}
	public class tinteger : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tinteger>
	{
		public const int ProtoId = 15;
		public long int1;
		public long int2;
		public long int3;
		public long int4;
		public long int5;
		public long int6;
		public long int7;
		public long int8;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
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
		public void Deserialize(EndianBinaryReader binaryReader)
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
		public double int1;
		public double int2;
		public double int3;
		public double int4;
		public double int5;
		public double int6;
		public double int7;
		public double int8;
		public double float1;
		public double float2;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
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
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
			binaryWriter.WriteValue(int2);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.Boolean);
		}
	}
	public class tlistint8 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistint8>
	{
		public const int ProtoId = 21;
		public List<sbyte> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.Int8);
		}
	}
	public class tlistuint8 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistuint8>
	{
		public const int ProtoId = 22;
		public List<byte> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.UInt8);
		}
	}
	public class tlistint16 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistint16>
	{
		public const int ProtoId = 23;
		public List<short> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.Int16);
		}
	}
	public class tlistuint16 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistuint16>
	{
		public const int ProtoId = 24;
		public List<ushort> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.UInt16);
		}
	}
	public class tlistint32 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistint32>
	{
		public const int ProtoId = 25;
		public List<int> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.Int32);
		}
	}
	public class tlistuint32 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistuint32>
	{
		public const int ProtoId = 26;
		public List<uint> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.UInt32);
		}
	}
	public class tlistint64 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistint64>
	{
		public const int ProtoId = 27;
		public List<long> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.Int64);
		}
	}
	public class tlistuint64 : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistuint64>
	{
		public const int ProtoId = 28;
		public List<ulong> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.UInt64);
		}
	}
	public class tlistinteger : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistinteger>
	{
		public const int ProtoId = 29;
		public List<long> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.Int64);
		}
	}
	public class tlistnumber : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistnumber>
	{
		public const int ProtoId = 30;
		public List<double> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.Double);
		}
	}
	public class tlistfloat : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistfloat>
	{
		public const int ProtoId = 31;
		public List<float> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.Float);
		}
	}
	public class tlistdouble : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistdouble>
	{
		public const int ProtoId = 32;
		public List<double> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.Double);
		}
	}
	public class tliststring : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tliststring>
	{
		public const int ProtoId = 33;
		public List<string> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.String);
		}
	}
	public class tlistunion : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tlistunion>
	{
		public const int ProtoId = 34;
		public List<union> int1;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(int1);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out int1, ProtocolCore.BasicTypeEnum.Custom);
		}
	}
	public class allType : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<allType>
	{
		public const int ProtoId = 35;
		public bool flag;
		public sbyte v8;
		public byte uv8;
		public short v16;
		public ushort uv16;
		public int v32;
		public uint uv32;
		public long v64;
		public ulong uv64;
		public long inte8;
		public long uinte8;
		public long inte16;
		public long uinte16;
		public long inte32;
		public long uinte32;
		public long inte64;
		public long uinte64;
		public double num8;
		public double unum8;
		public double num16;
		public double unum16;
		public double num32;
		public double unum32;
		public double num64;
		public double unum64;
		public double numfloat;
		public double numdouble;
		public float f32;
		public double f64;
		public string string1;
		public string string2;
		public union uval;
		public List<bool> lbool;
		public List<sbyte> lint8;
		public List<byte> luint8;
		public List<short> lint16;
		public List<ushort> luint16;
		public List<int> lint32;
		public List<uint> luint32;
		public List<long> lint64;
		public List<ulong> luint64;
		public List<long> linte8;
		public List<long> linte16;
		public List<long> linte32;
		public List<long> linte64;
		public List<double> lnum8;
		public List<double> lnum16;
		public List<double> lnum32;
		public List<double> lnum64;
		public List<double> lnfloat32;
		public List<double> lnfloat64;
		public List<float> lfloat;
		public List<double> ldouble;
		public List<string> lstring;
		public List<union> lunion;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(flag);
			binaryWriter.WriteValue(v8);
			binaryWriter.WriteValue(uv8);
			binaryWriter.WriteValue(v16);
			binaryWriter.WriteValue(uv16);
			binaryWriter.WriteValue(v32);
			binaryWriter.WriteValue(uv32);
			binaryWriter.WriteValue(v64);
			binaryWriter.WriteValue(uv64);
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
			binaryWriter.WriteValue(f32);
			binaryWriter.WriteValue(f64);
			binaryWriter.WriteValue(string1);
			binaryWriter.WriteValue(string2);
			if (uval != null)
			{
				binaryWriter.Write((byte)1);
				uval.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
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
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out flag);
			binaryReader.ReadValue(out v8);
			binaryReader.ReadValue(out uv8);
			binaryReader.ReadValue(out v16);
			binaryReader.ReadValue(out uv16);
			binaryReader.ReadValue(out v32);
			binaryReader.ReadValue(out uv32);
			binaryReader.ReadValue(out v64);
			binaryReader.ReadValue(out uv64);
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
			binaryReader.ReadValue(out f32);
			binaryReader.ReadValue(out f64);
			binaryReader.ReadValue(out string1);
			binaryReader.ReadValue(out string2);
			if (binaryReader.ReadByte() != 0)
			{
				uval = new union();
				uval.Deserialize(binaryReader);
			}
			binaryReader.ReadValue(out lbool, ProtocolCore.BasicTypeEnum.Boolean);
			binaryReader.ReadValue(out lint8, ProtocolCore.BasicTypeEnum.Int8);
			binaryReader.ReadValue(out luint8, ProtocolCore.BasicTypeEnum.UInt8);
			binaryReader.ReadValue(out lint16, ProtocolCore.BasicTypeEnum.Int16);
			binaryReader.ReadValue(out luint16, ProtocolCore.BasicTypeEnum.UInt16);
			binaryReader.ReadValue(out lint32, ProtocolCore.BasicTypeEnum.Int32);
			binaryReader.ReadValue(out luint32, ProtocolCore.BasicTypeEnum.UInt32);
			binaryReader.ReadValue(out lint64, ProtocolCore.BasicTypeEnum.Int64);
			binaryReader.ReadValue(out luint64, ProtocolCore.BasicTypeEnum.UInt64);
			binaryReader.ReadValue(out linte8, ProtocolCore.BasicTypeEnum.Int64);
			binaryReader.ReadValue(out linte16, ProtocolCore.BasicTypeEnum.Int64);
			binaryReader.ReadValue(out linte32, ProtocolCore.BasicTypeEnum.Int64);
			binaryReader.ReadValue(out linte64, ProtocolCore.BasicTypeEnum.Int64);
			binaryReader.ReadValue(out lnum8, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lnum16, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lnum32, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lnum64, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lnfloat32, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lnfloat64, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lfloat, ProtocolCore.BasicTypeEnum.Float);
			binaryReader.ReadValue(out ldouble, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lstring, ProtocolCore.BasicTypeEnum.String);
			binaryReader.ReadValue(out lunion, ProtocolCore.BasicTypeEnum.Custom);
		}
	}
	public class testnull : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<testnull>
	{
		public const int ProtoId = 36;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
		}
	}
	public class tbinary : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tbinary>
	{
		public const int ProtoId = 37;
		public byte[] data;
		public List<byte[]> chunks;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(data);
			binaryWriter.WriteValue(chunks);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out data);
			binaryReader.ReadValue(out chunks, ProtocolCore.BasicTypeEnum.Binary);
		}
	}
	public class tmap : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tmap>
	{
		public const int ProtoId = 38;
		public Dictionary<string, int> attrs;
		public Dictionary<int, string> names;
		public Dictionary<string, phoneNumber> phones;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.Write((ushort)(attrs?.Count ?? 0));
			if (attrs != null)
			{
				foreach (var __kv in attrs)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(names?.Count ?? 0));
			if (names != null)
			{
				foreach (var __kv in names)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(phones?.Count ?? 0));
			if (phones != null)
			{
				foreach (var __kv in phones)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			{
				var __cnt = binaryReader.ReadUInt16();
				attrs = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					attrs[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				names = new Dictionary<int, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					names[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				phones = new Dictionary<string, phoneNumber>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					var __v = new phoneNumber(); __v.Deserialize(binaryReader);
					phones[__k] = __v;
				}
			}
		}
	}
	public class toneof : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<toneof>
	{
		public const int ProtoId = 39;
		public int reason;
		public (byte tag, object value)? item;
		public (byte tag, object value)? item2;
		public string tip;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(reason);
			if (item == null || item.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (item.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)item.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)item.Value.value);
						break;
					case 3:
						binaryWriter.Write((byte)3);
						((phoneNumber)item.Value.value).Serialize(binaryWriter);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			if (item2 == null || item2.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (item2.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)item2.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)item2.Value.value);
						break;
					case 3:
						binaryWriter.Write((byte)3);
						((phoneNumber)item2.Value.value).Serialize(binaryWriter);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(tip);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out reason);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						item = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						item = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						item = ((byte)2, (object)__ov);
						break;
					}
					case 3:
					{
						var __ov = new phoneNumber(); __ov.Deserialize(binaryReader);
						item = ((byte)3, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						item2 = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						item2 = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						item2 = ((byte)2, (object)__ov);
						break;
					}
					case 3:
					{
						var __ov = new phoneNumber(); __ov.Deserialize(binaryReader);
						item2 = ((byte)3, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out tip);
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
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(name);
			binaryWriter.WriteValue(id);
			binaryWriter.WriteValue(email);
			binaryWriter.WriteValue(phone);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out name);
			binaryReader.ReadValue(out id);
			binaryReader.ReadValue(out email);
			binaryReader.ReadValue(out phone, ProtocolCore.BasicTypeEnum.Custom);
		}
	}
	public class comboLeaf : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<comboLeaf>
	{
		public const int ProtoId = 2001;
		public int id;
		public string tag;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(id);
			binaryWriter.WriteValue(tag);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out id);
			binaryReader.ReadValue(out tag);
		}
	}
	public class comboNode : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<comboNode>
	{
		public const int ProtoId = 2002;
		public comboLeaf head;
		public List<comboLeaf> leaves;
		public Dictionary<string, int> index;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			if (head != null)
			{
				binaryWriter.Write((byte)1);
				head.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			binaryWriter.WriteValue(leaves);
			binaryWriter.Write((ushort)(index?.Count ?? 0));
			if (index != null)
			{
				foreach (var __kv in index)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			if (binaryReader.ReadByte() != 0)
			{
				head = new comboLeaf();
				head.Deserialize(binaryReader);
			}
			binaryReader.ReadValue(out leaves, ProtocolCore.BasicTypeEnum.Custom);
			{
				var __cnt = binaryReader.ReadUInt16();
				index = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					index[__k] = __v;
				}
			}
		}
	}
	public class comboWrap : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<comboWrap>
	{
		public const int ProtoId = 2003;
		public string name;
		public comboNode node;
		public Dictionary<int, string> rev;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(name);
			if (node != null)
			{
				binaryWriter.Write((byte)1);
				node.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			binaryWriter.Write((ushort)(rev?.Count ?? 0));
			if (rev != null)
			{
				foreach (var __kv in rev)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out name);
			if (binaryReader.ReadByte() != 0)
			{
				node = new comboNode();
				node.Deserialize(binaryReader);
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				rev = new Dictionary<int, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					rev[__k] = __v;
				}
			}
		}
	}
	public class tMapMatrix : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tMapMatrix>
	{
		public const int ProtoId = 2004;
		public Dictionary<string, int> s_i;
		public Dictionary<int, string> i_s;
		public Dictionary<string, string> s_s;
		public Dictionary<int, int> i_i;
		public Dictionary<long, int> i64_i;
		public Dictionary<uint, string> u_s;
		public Dictionary<string, byte[]> s_b;
		public Dictionary<int, byte[]> i_b;
		public Dictionary<string, phoneNumber> s_rec;
		public Dictionary<int, comboLeaf> i_leaf;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.Write((ushort)(s_i?.Count ?? 0));
			if (s_i != null)
			{
				foreach (var __kv in s_i)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(i_s?.Count ?? 0));
			if (i_s != null)
			{
				foreach (var __kv in i_s)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(s_s?.Count ?? 0));
			if (s_s != null)
			{
				foreach (var __kv in s_s)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(i_i?.Count ?? 0));
			if (i_i != null)
			{
				foreach (var __kv in i_i)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(i64_i?.Count ?? 0));
			if (i64_i != null)
			{
				foreach (var __kv in i64_i)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(u_s?.Count ?? 0));
			if (u_s != null)
			{
				foreach (var __kv in u_s)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(s_b?.Count ?? 0));
			if (s_b != null)
			{
				foreach (var __kv in s_b)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(i_b?.Count ?? 0));
			if (i_b != null)
			{
				foreach (var __kv in i_b)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(s_rec?.Count ?? 0));
			if (s_rec != null)
			{
				foreach (var __kv in s_rec)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			binaryWriter.Write((ushort)(i_leaf?.Count ?? 0));
			if (i_leaf != null)
			{
				foreach (var __kv in i_leaf)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			{
				var __cnt = binaryReader.ReadUInt16();
				s_i = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					s_i[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				i_s = new Dictionary<int, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					i_s[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				s_s = new Dictionary<string, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					s_s[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				i_i = new Dictionary<int, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					i_i[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				i64_i = new Dictionary<long, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					long __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					i64_i[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				u_s = new Dictionary<uint, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					uint __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					u_s[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				s_b = new Dictionary<string, byte[]>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					byte[] __v; binaryReader.ReadValue(out __v);
					s_b[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				i_b = new Dictionary<int, byte[]>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					byte[] __v; binaryReader.ReadValue(out __v);
					i_b[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				s_rec = new Dictionary<string, phoneNumber>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					var __v = new phoneNumber(); __v.Deserialize(binaryReader);
					s_rec[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				i_leaf = new Dictionary<int, comboLeaf>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					var __v = new comboLeaf(); __v.Deserialize(binaryReader);
					i_leaf[__k] = __v;
				}
			}
		}
	}
	public class tListMatrix : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tListMatrix>
	{
		public const int ProtoId = 2005;
		public List<bool> lb;
		public List<sbyte> li8;
		public List<byte> lu8;
		public List<short> li16;
		public List<ushort> lu16;
		public List<int> li32;
		public List<uint> lu32;
		public List<long> li64;
		public List<ulong> lu64;
		public List<long> lint;
		public List<double> lnum;
		public List<float> lflt;
		public List<double> ldbl;
		public List<string> lstr;
		public List<byte[]> lbin;
		public List<union> luni;
		public List<phoneNumber> lph;
		public List<comboLeaf> lleaf;
		public List<comboNode> lnode;
		public List<int> lli32_1;
		public List<int> lli32_2;
		public List<int> lli32_3;
		public Dictionary<string, int> lmap_1;
		public Dictionary<string, int> lmap_2;
		public Dictionary<string, int> lmap_3;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(lb);
			binaryWriter.WriteValue(li8);
			binaryWriter.WriteValue(lu8);
			binaryWriter.WriteValue(li16);
			binaryWriter.WriteValue(lu16);
			binaryWriter.WriteValue(li32);
			binaryWriter.WriteValue(lu32);
			binaryWriter.WriteValue(li64);
			binaryWriter.WriteValue(lu64);
			binaryWriter.WriteValue(lint);
			binaryWriter.WriteValue(lnum);
			binaryWriter.WriteValue(lflt);
			binaryWriter.WriteValue(ldbl);
			binaryWriter.WriteValue(lstr);
			binaryWriter.WriteValue(lbin);
			binaryWriter.WriteValue(luni);
			binaryWriter.WriteValue(lph);
			binaryWriter.WriteValue(lleaf);
			binaryWriter.WriteValue(lnode);
			binaryWriter.WriteValue(lli32_1);
			binaryWriter.WriteValue(lli32_2);
			binaryWriter.WriteValue(lli32_3);
			binaryWriter.Write((ushort)(lmap_1?.Count ?? 0));
			if (lmap_1 != null)
			{
				foreach (var __kv in lmap_1)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(lmap_2?.Count ?? 0));
			if (lmap_2 != null)
			{
				foreach (var __kv in lmap_2)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(lmap_3?.Count ?? 0));
			if (lmap_3 != null)
			{
				foreach (var __kv in lmap_3)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out lb, ProtocolCore.BasicTypeEnum.Boolean);
			binaryReader.ReadValue(out li8, ProtocolCore.BasicTypeEnum.Int8);
			binaryReader.ReadValue(out lu8, ProtocolCore.BasicTypeEnum.UInt8);
			binaryReader.ReadValue(out li16, ProtocolCore.BasicTypeEnum.Int16);
			binaryReader.ReadValue(out lu16, ProtocolCore.BasicTypeEnum.UInt16);
			binaryReader.ReadValue(out li32, ProtocolCore.BasicTypeEnum.Int32);
			binaryReader.ReadValue(out lu32, ProtocolCore.BasicTypeEnum.UInt32);
			binaryReader.ReadValue(out li64, ProtocolCore.BasicTypeEnum.Int64);
			binaryReader.ReadValue(out lu64, ProtocolCore.BasicTypeEnum.UInt64);
			binaryReader.ReadValue(out lint, ProtocolCore.BasicTypeEnum.Int64);
			binaryReader.ReadValue(out lnum, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lflt, ProtocolCore.BasicTypeEnum.Float);
			binaryReader.ReadValue(out ldbl, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lstr, ProtocolCore.BasicTypeEnum.String);
			binaryReader.ReadValue(out lbin, ProtocolCore.BasicTypeEnum.Binary);
			binaryReader.ReadValue(out luni, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out lph, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out lleaf, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out lnode, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out lli32_1, ProtocolCore.BasicTypeEnum.Int32);
			binaryReader.ReadValue(out lli32_2, ProtocolCore.BasicTypeEnum.Int32);
			binaryReader.ReadValue(out lli32_3, ProtocolCore.BasicTypeEnum.Int32);
			{
				var __cnt = binaryReader.ReadUInt16();
				lmap_1 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					lmap_1[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				lmap_2 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					lmap_2[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				lmap_3 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					lmap_3[__k] = __v;
				}
			}
		}
	}
	public class tOneofMatrix : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tOneofMatrix>
	{
		public const int ProtoId = 2006;
		public (byte tag, object value)? payload;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			if (payload == null || payload.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (payload.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((bool)payload.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((int)payload.Value.value);
						break;
					case 3:
						binaryWriter.Write((byte)3);
						binaryWriter.WriteValue((string)payload.Value.value);
						break;
					case 4:
						binaryWriter.Write((byte)4);
						binaryWriter.WriteValue((byte[])payload.Value.value);
						break;
					case 5:
						binaryWriter.Write((byte)5);
						((phoneNumber)payload.Value.value).Serialize(binaryWriter);
						break;
					case 6:
						binaryWriter.Write((byte)6);
						((comboLeaf)payload.Value.value).Serialize(binaryWriter);
						break;
					case 7:
						binaryWriter.Write((byte)7);
						((comboNode)payload.Value.value).Serialize(binaryWriter);
						break;
					case 8:
						binaryWriter.Write((byte)8);
						binaryWriter.WriteValue((List<int>)payload.Value.value);
						break;
					case 9:
						binaryWriter.Write((byte)9);
						binaryWriter.WriteValue((List<string>)payload.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						payload = null;
						break;
					case 1:
					{
						bool __ov; binaryReader.ReadValue(out __ov);
						payload = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						payload = ((byte)2, (object)__ov);
						break;
					}
					case 3:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						payload = ((byte)3, (object)__ov);
						break;
					}
					case 4:
					{
						byte[] __ov; binaryReader.ReadValue(out __ov);
						payload = ((byte)4, (object)__ov);
						break;
					}
					case 5:
					{
						var __ov = new phoneNumber(); __ov.Deserialize(binaryReader);
						payload = ((byte)5, (object)__ov);
						break;
					}
					case 6:
					{
						var __ov = new comboLeaf(); __ov.Deserialize(binaryReader);
						payload = ((byte)6, (object)__ov);
						break;
					}
					case 7:
					{
						var __ov = new comboNode(); __ov.Deserialize(binaryReader);
						payload = ((byte)7, (object)__ov);
						break;
					}
					case 8:
					{
						List<int> __ov; binaryReader.ReadValue(out __ov, ProtocolCore.BasicTypeEnum.Custom);
						payload = ((byte)8, (object)__ov);
						break;
					}
					case 9:
					{
						List<string> __ov; binaryReader.ReadValue(out __ov, ProtocolCore.BasicTypeEnum.Custom);
						payload = ((byte)9, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
		}
	}
	public class tNestDeep : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tNestDeep>
	{
		public const int ProtoId = 2007;
		public Dictionary<string, comboLeaf> ms_leaf;
		public comboWrap wrap;
		public (byte tag, object value)? nested;
		public List<comboWrap> lwrap_1;
		public List<comboWrap> lwrap_2;
		public List<comboWrap> lwrap_3;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.Write((ushort)(ms_leaf?.Count ?? 0));
			if (ms_leaf != null)
			{
				foreach (var __kv in ms_leaf)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			if (wrap != null)
			{
				binaryWriter.Write((byte)1);
				wrap.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (nested == null || nested.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (nested.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						((comboNode)nested.Value.value).Serialize(binaryWriter);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						((comboLeaf)nested.Value.value).Serialize(binaryWriter);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(lwrap_1);
			binaryWriter.WriteValue(lwrap_2);
			binaryWriter.WriteValue(lwrap_3);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_leaf = new Dictionary<string, comboLeaf>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					var __v = new comboLeaf(); __v.Deserialize(binaryReader);
					ms_leaf[__k] = __v;
				}
			}
			if (binaryReader.ReadByte() != 0)
			{
				wrap = new comboWrap();
				wrap.Deserialize(binaryReader);
			}
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						nested = null;
						break;
					case 1:
					{
						var __ov = new comboNode(); __ov.Deserialize(binaryReader);
						nested = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						var __ov = new comboLeaf(); __ov.Deserialize(binaryReader);
						nested = ((byte)2, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out lwrap_1, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out lwrap_2, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out lwrap_3, ProtocolCore.BasicTypeEnum.Custom);
		}
	}
	public class tInterleave : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tInterleave>
	{
		public const int ProtoId = 2008;
		public int n1;
		public string s1;
		public Dictionary<string, int> m1;
		public List<int> l1;
		public (byte tag, object value)? o1;
		public int n2;
		public string s2;
		public Dictionary<string, int> m2;
		public List<int> l2;
		public (byte tag, object value)? o2;
		public int n3;
		public string s3;
		public Dictionary<string, int> m3;
		public List<int> l3;
		public (byte tag, object value)? o3;
		public byte[] b1;
		public byte[] b2;
		public byte[] b3;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(n1);
			binaryWriter.WriteValue(s1);
			binaryWriter.Write((ushort)(m1?.Count ?? 0));
			if (m1 != null)
			{
				foreach (var __kv in m1)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.WriteValue(l1);
			if (o1 == null || o1.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (o1.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)o1.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)o1.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(n2);
			binaryWriter.WriteValue(s2);
			binaryWriter.Write((ushort)(m2?.Count ?? 0));
			if (m2 != null)
			{
				foreach (var __kv in m2)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.WriteValue(l2);
			if (o2 == null || o2.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (o2.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)o2.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)o2.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(n3);
			binaryWriter.WriteValue(s3);
			binaryWriter.Write((ushort)(m3?.Count ?? 0));
			if (m3 != null)
			{
				foreach (var __kv in m3)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.WriteValue(l3);
			if (o3 == null || o3.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (o3.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)o3.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)o3.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(b1);
			binaryWriter.WriteValue(b2);
			binaryWriter.WriteValue(b3);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out n1);
			binaryReader.ReadValue(out s1);
			{
				var __cnt = binaryReader.ReadUInt16();
				m1 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					m1[__k] = __v;
				}
			}
			binaryReader.ReadValue(out l1, ProtocolCore.BasicTypeEnum.Int32);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						o1 = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						o1 = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						o1 = ((byte)2, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out n2);
			binaryReader.ReadValue(out s2);
			{
				var __cnt = binaryReader.ReadUInt16();
				m2 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					m2[__k] = __v;
				}
			}
			binaryReader.ReadValue(out l2, ProtocolCore.BasicTypeEnum.Int32);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						o2 = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						o2 = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						o2 = ((byte)2, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out n3);
			binaryReader.ReadValue(out s3);
			{
				var __cnt = binaryReader.ReadUInt16();
				m3 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					m3[__k] = __v;
				}
			}
			binaryReader.ReadValue(out l3, ProtocolCore.BasicTypeEnum.Int32);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						o3 = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						o3 = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						o3 = ((byte)2, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out b1);
			binaryReader.ReadValue(out b2);
			binaryReader.ReadValue(out b3);
		}
	}
	public class tMultiOneof : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tMultiOneof>
	{
		public const int ProtoId = 2009;
		public (byte tag, object value)? a;
		public int g1;
		public (byte tag, object value)? b;
		public int g2;
		public (byte tag, object value)? c;
		public int g3;
		public (byte tag, object value)? d;
		public int g4;
		public (byte tag, object value)? e;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			if (a == null || a.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (a.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)a.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)a.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(g1);
			if (b == null || b.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (b.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)b.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)b.Value.value);
						break;
					case 3:
						binaryWriter.Write((byte)3);
						binaryWriter.WriteValue((byte[])b.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(g2);
			if (c == null || c.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (c.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)c.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						((phoneNumber)c.Value.value).Serialize(binaryWriter);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(g3);
			if (d == null || d.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (d.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						((comboLeaf)d.Value.value).Serialize(binaryWriter);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((List<int>)d.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(g4);
			if (e == null || e.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (e.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((string)e.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((int)e.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						a = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						a = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						a = ((byte)2, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out g1);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						b = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						b = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						b = ((byte)2, (object)__ov);
						break;
					}
					case 3:
					{
						byte[] __ov; binaryReader.ReadValue(out __ov);
						b = ((byte)3, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out g2);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						c = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						c = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						var __ov = new phoneNumber(); __ov.Deserialize(binaryReader);
						c = ((byte)2, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out g3);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						d = null;
						break;
					case 1:
					{
						var __ov = new comboLeaf(); __ov.Deserialize(binaryReader);
						d = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						List<int> __ov; binaryReader.ReadValue(out __ov, ProtocolCore.BasicTypeEnum.Custom);
						d = ((byte)2, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out g4);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						e = null;
						break;
					case 1:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						e = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						e = ((byte)2, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
		}
	}
	public class tEmptyEdge : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tEmptyEdge>
	{
		public const int ProtoId = 2010;
		public Dictionary<string, int> empty_map;
		public List<int> empty_list;
		public List<string> empty_strs;
		public byte[] empty_bin;
		public string empty_str;
		public (byte tag, object value)? none;
		public phoneNumber empty_phone;
		public comboLeaf empty_leaf;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.Write((ushort)(empty_map?.Count ?? 0));
			if (empty_map != null)
			{
				foreach (var __kv in empty_map)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.WriteValue(empty_list);
			binaryWriter.WriteValue(empty_strs);
			binaryWriter.WriteValue(empty_bin);
			binaryWriter.WriteValue(empty_str);
			if (none == null || none.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (none.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)none.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)none.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			if (empty_phone != null)
			{
				binaryWriter.Write((byte)1);
				empty_phone.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (empty_leaf != null)
			{
				binaryWriter.Write((byte)1);
				empty_leaf.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			{
				var __cnt = binaryReader.ReadUInt16();
				empty_map = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					empty_map[__k] = __v;
				}
			}
			binaryReader.ReadValue(out empty_list, ProtocolCore.BasicTypeEnum.Int32);
			binaryReader.ReadValue(out empty_strs, ProtocolCore.BasicTypeEnum.String);
			binaryReader.ReadValue(out empty_bin);
			binaryReader.ReadValue(out empty_str);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						none = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						none = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						none = ((byte)2, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			if (binaryReader.ReadByte() != 0)
			{
				empty_phone = new phoneNumber();
				empty_phone.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				empty_leaf = new comboLeaf();
				empty_leaf.Deserialize(binaryReader);
			}
		}
	}
	public class tScalarRun : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tScalarRun>
	{
		public const int ProtoId = 2011;
		public int a1;
		public int a2;
		public int a3;
		public string b1;
		public string b2;
		public string b3;
		public Dictionary<string, int> c1;
		public Dictionary<string, int> c2;
		public Dictionary<string, int> c3;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(a1);
			binaryWriter.WriteValue(a2);
			binaryWriter.WriteValue(a3);
			binaryWriter.WriteValue(b1);
			binaryWriter.WriteValue(b2);
			binaryWriter.WriteValue(b3);
			binaryWriter.Write((ushort)(c1?.Count ?? 0));
			if (c1 != null)
			{
				foreach (var __kv in c1)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(c2?.Count ?? 0));
			if (c2 != null)
			{
				foreach (var __kv in c2)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(c3?.Count ?? 0));
			if (c3 != null)
			{
				foreach (var __kv in c3)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out a1);
			binaryReader.ReadValue(out a2);
			binaryReader.ReadValue(out a3);
			binaryReader.ReadValue(out b1);
			binaryReader.ReadValue(out b2);
			binaryReader.ReadValue(out b3);
			{
				var __cnt = binaryReader.ReadUInt16();
				c1 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					c1[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				c2 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					c2[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				c3 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					c3[__k] = __v;
				}
			}
		}
	}
	public class tRepeatAll : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tRepeatAll>
	{
		public const int ProtoId = 2012;
		public bool b1;
		public bool b2;
		public bool b3;
		public int i32_1;
		public int i32_2;
		public int i32_3;
		public string gap_a;
		public int i32_4;
		public sbyte i8_1;
		public sbyte i8_2;
		public byte u8_1;
		public sbyte i8_3;
		public byte u8_2;
		public byte u8_3;
		public short i16_1;
		public short i16_2;
		public short i16_3;
		public ushort u16_1;
		public string gap_b;
		public ushort u16_2;
		public ushort u16_3;
		public int i32_5;
		public int i32_6;
		public int i32_7;
		public uint u32_1;
		public uint u32_2;
		public uint u32_3;
		public long i64_1;
		public long i64_2;
		public string gap_c;
		public long i64_3;
		public ulong u64_1;
		public ulong u64_2;
		public ulong u64_3;
		public long inte_1;
		public long inte_2;
		public long inte_3;
		public double num_1;
		public string gap_d;
		public double num_2;
		public double num_3;
		public float flt_1;
		public float flt_2;
		public float flt_3;
		public double dbl_1;
		public double dbl_2;
		public double dbl_3;
		public string str_1;
		public string str_2;
		public string str_3;
		public int gap_e;
		public string str_4;
		public byte[] bin_1;
		public byte[] bin_2;
		public byte[] bin_3;
		public union uni_1;
		public union uni_2;
		public union uni_3;
		public List<int> li32_1;
		public List<int> li32_2;
		public List<int> li32_3;
		public List<string> lstr_1;
		public int gap_f;
		public List<string> lstr_2;
		public List<string> lstr_3;
		public List<byte[]> lbin_1;
		public List<byte[]> lbin_2;
		public List<byte[]> lbin_3;
		public List<bool> lbool_1;
		public List<bool> lbool_2;
		public List<bool> lbool_3;
		public List<long> lint_1;
		public List<long> lint_2;
		public List<long> lint_3;
		public List<double> lnum_1;
		public List<double> lnum_2;
		public List<double> lnum_3;
		public List<phoneNumber> lph_1;
		public List<phoneNumber> lph_2;
		public string gap_g;
		public List<phoneNumber> lph_3;
		public List<comboLeaf> lleaf_1;
		public List<comboLeaf> lleaf_2;
		public List<comboLeaf> lleaf_3;
		public Dictionary<string, int> ms_i_1;
		public Dictionary<string, int> ms_i_2;
		public Dictionary<string, int> ms_i_3;
		public Dictionary<int, string> mi_s_1;
		public int gap_h;
		public Dictionary<int, string> mi_s_2;
		public Dictionary<int, string> mi_s_3;
		public Dictionary<string, string> ms_s_1;
		public Dictionary<string, string> ms_s_2;
		public Dictionary<string, string> ms_s_3;
		public Dictionary<int, int> mi_i_1;
		public Dictionary<int, int> mi_i_2;
		public Dictionary<int, int> mi_i_3;
		public Dictionary<string, byte[]> ms_b_1;
		public Dictionary<string, byte[]> ms_b_2;
		public Dictionary<string, byte[]> ms_b_3;
		public Dictionary<int, byte[]> mi_b_1;
		public Dictionary<int, byte[]> mi_b_2;
		public Dictionary<int, byte[]> mi_b_3;
		public Dictionary<string, phoneNumber> ms_p_1;
		public Dictionary<string, phoneNumber> ms_p_2;
		public Dictionary<string, phoneNumber> ms_p_3;
		public Dictionary<int, comboLeaf> mi_l_1;
		public Dictionary<int, comboLeaf> mi_l_2;
		public Dictionary<int, comboLeaf> mi_l_3;
		public (byte tag, object value)? pick1;
		public int gap_i;
		public (byte tag, object value)? pick2;
		public string gap_j;
		public (byte tag, object value)? pick3;
		public phoneNumber ph_1;
		public phoneNumber ph_2;
		public string gap_k;
		public phoneNumber ph_3;
		public comboLeaf leaf_1;
		public comboLeaf leaf_2;
		public comboLeaf leaf_3;
		public comboNode node_1;
		public comboNode node_2;
		public comboNode node_3;
		public comboWrap wrap_1;
		public comboWrap wrap_2;
		public comboWrap wrap_3;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(b1);
			binaryWriter.WriteValue(b2);
			binaryWriter.WriteValue(b3);
			binaryWriter.WriteValue(i32_1);
			binaryWriter.WriteValue(i32_2);
			binaryWriter.WriteValue(i32_3);
			binaryWriter.WriteValue(gap_a);
			binaryWriter.WriteValue(i32_4);
			binaryWriter.WriteValue(i8_1);
			binaryWriter.WriteValue(i8_2);
			binaryWriter.WriteValue(u8_1);
			binaryWriter.WriteValue(i8_3);
			binaryWriter.WriteValue(u8_2);
			binaryWriter.WriteValue(u8_3);
			binaryWriter.WriteValue(i16_1);
			binaryWriter.WriteValue(i16_2);
			binaryWriter.WriteValue(i16_3);
			binaryWriter.WriteValue(u16_1);
			binaryWriter.WriteValue(gap_b);
			binaryWriter.WriteValue(u16_2);
			binaryWriter.WriteValue(u16_3);
			binaryWriter.WriteValue(i32_5);
			binaryWriter.WriteValue(i32_6);
			binaryWriter.WriteValue(i32_7);
			binaryWriter.WriteValue(u32_1);
			binaryWriter.WriteValue(u32_2);
			binaryWriter.WriteValue(u32_3);
			binaryWriter.WriteValue(i64_1);
			binaryWriter.WriteValue(i64_2);
			binaryWriter.WriteValue(gap_c);
			binaryWriter.WriteValue(i64_3);
			binaryWriter.WriteValue(u64_1);
			binaryWriter.WriteValue(u64_2);
			binaryWriter.WriteValue(u64_3);
			binaryWriter.WriteValue(inte_1);
			binaryWriter.WriteValue(inte_2);
			binaryWriter.WriteValue(inte_3);
			binaryWriter.WriteValue(num_1);
			binaryWriter.WriteValue(gap_d);
			binaryWriter.WriteValue(num_2);
			binaryWriter.WriteValue(num_3);
			binaryWriter.WriteValue(flt_1);
			binaryWriter.WriteValue(flt_2);
			binaryWriter.WriteValue(flt_3);
			binaryWriter.WriteValue(dbl_1);
			binaryWriter.WriteValue(dbl_2);
			binaryWriter.WriteValue(dbl_3);
			binaryWriter.WriteValue(str_1);
			binaryWriter.WriteValue(str_2);
			binaryWriter.WriteValue(str_3);
			binaryWriter.WriteValue(gap_e);
			binaryWriter.WriteValue(str_4);
			binaryWriter.WriteValue(bin_1);
			binaryWriter.WriteValue(bin_2);
			binaryWriter.WriteValue(bin_3);
			if (uni_1 != null)
			{
				binaryWriter.Write((byte)1);
				uni_1.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (uni_2 != null)
			{
				binaryWriter.Write((byte)1);
				uni_2.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (uni_3 != null)
			{
				binaryWriter.Write((byte)1);
				uni_3.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			binaryWriter.WriteValue(li32_1);
			binaryWriter.WriteValue(li32_2);
			binaryWriter.WriteValue(li32_3);
			binaryWriter.WriteValue(lstr_1);
			binaryWriter.WriteValue(gap_f);
			binaryWriter.WriteValue(lstr_2);
			binaryWriter.WriteValue(lstr_3);
			binaryWriter.WriteValue(lbin_1);
			binaryWriter.WriteValue(lbin_2);
			binaryWriter.WriteValue(lbin_3);
			binaryWriter.WriteValue(lbool_1);
			binaryWriter.WriteValue(lbool_2);
			binaryWriter.WriteValue(lbool_3);
			binaryWriter.WriteValue(lint_1);
			binaryWriter.WriteValue(lint_2);
			binaryWriter.WriteValue(lint_3);
			binaryWriter.WriteValue(lnum_1);
			binaryWriter.WriteValue(lnum_2);
			binaryWriter.WriteValue(lnum_3);
			binaryWriter.WriteValue(lph_1);
			binaryWriter.WriteValue(lph_2);
			binaryWriter.WriteValue(gap_g);
			binaryWriter.WriteValue(lph_3);
			binaryWriter.WriteValue(lleaf_1);
			binaryWriter.WriteValue(lleaf_2);
			binaryWriter.WriteValue(lleaf_3);
			binaryWriter.Write((ushort)(ms_i_1?.Count ?? 0));
			if (ms_i_1 != null)
			{
				foreach (var __kv in ms_i_1)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(ms_i_2?.Count ?? 0));
			if (ms_i_2 != null)
			{
				foreach (var __kv in ms_i_2)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(ms_i_3?.Count ?? 0));
			if (ms_i_3 != null)
			{
				foreach (var __kv in ms_i_3)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(mi_s_1?.Count ?? 0));
			if (mi_s_1 != null)
			{
				foreach (var __kv in mi_s_1)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.WriteValue(gap_h);
			binaryWriter.Write((ushort)(mi_s_2?.Count ?? 0));
			if (mi_s_2 != null)
			{
				foreach (var __kv in mi_s_2)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(mi_s_3?.Count ?? 0));
			if (mi_s_3 != null)
			{
				foreach (var __kv in mi_s_3)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(ms_s_1?.Count ?? 0));
			if (ms_s_1 != null)
			{
				foreach (var __kv in ms_s_1)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(ms_s_2?.Count ?? 0));
			if (ms_s_2 != null)
			{
				foreach (var __kv in ms_s_2)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(ms_s_3?.Count ?? 0));
			if (ms_s_3 != null)
			{
				foreach (var __kv in ms_s_3)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(mi_i_1?.Count ?? 0));
			if (mi_i_1 != null)
			{
				foreach (var __kv in mi_i_1)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(mi_i_2?.Count ?? 0));
			if (mi_i_2 != null)
			{
				foreach (var __kv in mi_i_2)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(mi_i_3?.Count ?? 0));
			if (mi_i_3 != null)
			{
				foreach (var __kv in mi_i_3)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(ms_b_1?.Count ?? 0));
			if (ms_b_1 != null)
			{
				foreach (var __kv in ms_b_1)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(ms_b_2?.Count ?? 0));
			if (ms_b_2 != null)
			{
				foreach (var __kv in ms_b_2)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(ms_b_3?.Count ?? 0));
			if (ms_b_3 != null)
			{
				foreach (var __kv in ms_b_3)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(mi_b_1?.Count ?? 0));
			if (mi_b_1 != null)
			{
				foreach (var __kv in mi_b_1)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(mi_b_2?.Count ?? 0));
			if (mi_b_2 != null)
			{
				foreach (var __kv in mi_b_2)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(mi_b_3?.Count ?? 0));
			if (mi_b_3 != null)
			{
				foreach (var __kv in mi_b_3)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(ms_p_1?.Count ?? 0));
			if (ms_p_1 != null)
			{
				foreach (var __kv in ms_p_1)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			binaryWriter.Write((ushort)(ms_p_2?.Count ?? 0));
			if (ms_p_2 != null)
			{
				foreach (var __kv in ms_p_2)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			binaryWriter.Write((ushort)(ms_p_3?.Count ?? 0));
			if (ms_p_3 != null)
			{
				foreach (var __kv in ms_p_3)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			binaryWriter.Write((ushort)(mi_l_1?.Count ?? 0));
			if (mi_l_1 != null)
			{
				foreach (var __kv in mi_l_1)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			binaryWriter.Write((ushort)(mi_l_2?.Count ?? 0));
			if (mi_l_2 != null)
			{
				foreach (var __kv in mi_l_2)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			binaryWriter.Write((ushort)(mi_l_3?.Count ?? 0));
			if (mi_l_3 != null)
			{
				foreach (var __kv in mi_l_3)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			if (pick1 == null || pick1.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (pick1.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)pick1.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)pick1.Value.value);
						break;
					case 3:
						binaryWriter.Write((byte)3);
						binaryWriter.WriteValue((byte[])pick1.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(gap_i);
			if (pick2 == null || pick2.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (pick2.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)pick2.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)pick2.Value.value);
						break;
					case 3:
						binaryWriter.Write((byte)3);
						((phoneNumber)pick2.Value.value).Serialize(binaryWriter);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(gap_j);
			if (pick3 == null || pick3.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (pick3.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)pick3.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						((comboLeaf)pick3.Value.value).Serialize(binaryWriter);
						break;
					case 3:
						binaryWriter.Write((byte)3);
						binaryWriter.WriteValue((List<int>)pick3.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			if (ph_1 != null)
			{
				binaryWriter.Write((byte)1);
				ph_1.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (ph_2 != null)
			{
				binaryWriter.Write((byte)1);
				ph_2.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			binaryWriter.WriteValue(gap_k);
			if (ph_3 != null)
			{
				binaryWriter.Write((byte)1);
				ph_3.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (leaf_1 != null)
			{
				binaryWriter.Write((byte)1);
				leaf_1.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (leaf_2 != null)
			{
				binaryWriter.Write((byte)1);
				leaf_2.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (leaf_3 != null)
			{
				binaryWriter.Write((byte)1);
				leaf_3.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (node_1 != null)
			{
				binaryWriter.Write((byte)1);
				node_1.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (node_2 != null)
			{
				binaryWriter.Write((byte)1);
				node_2.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (node_3 != null)
			{
				binaryWriter.Write((byte)1);
				node_3.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (wrap_1 != null)
			{
				binaryWriter.Write((byte)1);
				wrap_1.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (wrap_2 != null)
			{
				binaryWriter.Write((byte)1);
				wrap_2.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (wrap_3 != null)
			{
				binaryWriter.Write((byte)1);
				wrap_3.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out b1);
			binaryReader.ReadValue(out b2);
			binaryReader.ReadValue(out b3);
			binaryReader.ReadValue(out i32_1);
			binaryReader.ReadValue(out i32_2);
			binaryReader.ReadValue(out i32_3);
			binaryReader.ReadValue(out gap_a);
			binaryReader.ReadValue(out i32_4);
			binaryReader.ReadValue(out i8_1);
			binaryReader.ReadValue(out i8_2);
			binaryReader.ReadValue(out u8_1);
			binaryReader.ReadValue(out i8_3);
			binaryReader.ReadValue(out u8_2);
			binaryReader.ReadValue(out u8_3);
			binaryReader.ReadValue(out i16_1);
			binaryReader.ReadValue(out i16_2);
			binaryReader.ReadValue(out i16_3);
			binaryReader.ReadValue(out u16_1);
			binaryReader.ReadValue(out gap_b);
			binaryReader.ReadValue(out u16_2);
			binaryReader.ReadValue(out u16_3);
			binaryReader.ReadValue(out i32_5);
			binaryReader.ReadValue(out i32_6);
			binaryReader.ReadValue(out i32_7);
			binaryReader.ReadValue(out u32_1);
			binaryReader.ReadValue(out u32_2);
			binaryReader.ReadValue(out u32_3);
			binaryReader.ReadValue(out i64_1);
			binaryReader.ReadValue(out i64_2);
			binaryReader.ReadValue(out gap_c);
			binaryReader.ReadValue(out i64_3);
			binaryReader.ReadValue(out u64_1);
			binaryReader.ReadValue(out u64_2);
			binaryReader.ReadValue(out u64_3);
			binaryReader.ReadValue(out inte_1);
			binaryReader.ReadValue(out inte_2);
			binaryReader.ReadValue(out inte_3);
			binaryReader.ReadValue(out num_1);
			binaryReader.ReadValue(out gap_d);
			binaryReader.ReadValue(out num_2);
			binaryReader.ReadValue(out num_3);
			binaryReader.ReadValue(out flt_1);
			binaryReader.ReadValue(out flt_2);
			binaryReader.ReadValue(out flt_3);
			binaryReader.ReadValue(out dbl_1);
			binaryReader.ReadValue(out dbl_2);
			binaryReader.ReadValue(out dbl_3);
			binaryReader.ReadValue(out str_1);
			binaryReader.ReadValue(out str_2);
			binaryReader.ReadValue(out str_3);
			binaryReader.ReadValue(out gap_e);
			binaryReader.ReadValue(out str_4);
			binaryReader.ReadValue(out bin_1);
			binaryReader.ReadValue(out bin_2);
			binaryReader.ReadValue(out bin_3);
			if (binaryReader.ReadByte() != 0)
			{
				uni_1 = new union();
				uni_1.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				uni_2 = new union();
				uni_2.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				uni_3 = new union();
				uni_3.Deserialize(binaryReader);
			}
			binaryReader.ReadValue(out li32_1, ProtocolCore.BasicTypeEnum.Int32);
			binaryReader.ReadValue(out li32_2, ProtocolCore.BasicTypeEnum.Int32);
			binaryReader.ReadValue(out li32_3, ProtocolCore.BasicTypeEnum.Int32);
			binaryReader.ReadValue(out lstr_1, ProtocolCore.BasicTypeEnum.String);
			binaryReader.ReadValue(out gap_f);
			binaryReader.ReadValue(out lstr_2, ProtocolCore.BasicTypeEnum.String);
			binaryReader.ReadValue(out lstr_3, ProtocolCore.BasicTypeEnum.String);
			binaryReader.ReadValue(out lbin_1, ProtocolCore.BasicTypeEnum.Binary);
			binaryReader.ReadValue(out lbin_2, ProtocolCore.BasicTypeEnum.Binary);
			binaryReader.ReadValue(out lbin_3, ProtocolCore.BasicTypeEnum.Binary);
			binaryReader.ReadValue(out lbool_1, ProtocolCore.BasicTypeEnum.Boolean);
			binaryReader.ReadValue(out lbool_2, ProtocolCore.BasicTypeEnum.Boolean);
			binaryReader.ReadValue(out lbool_3, ProtocolCore.BasicTypeEnum.Boolean);
			binaryReader.ReadValue(out lint_1, ProtocolCore.BasicTypeEnum.Int64);
			binaryReader.ReadValue(out lint_2, ProtocolCore.BasicTypeEnum.Int64);
			binaryReader.ReadValue(out lint_3, ProtocolCore.BasicTypeEnum.Int64);
			binaryReader.ReadValue(out lnum_1, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lnum_2, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lnum_3, ProtocolCore.BasicTypeEnum.Double);
			binaryReader.ReadValue(out lph_1, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out lph_2, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out gap_g);
			binaryReader.ReadValue(out lph_3, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out lleaf_1, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out lleaf_2, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out lleaf_3, ProtocolCore.BasicTypeEnum.Custom);
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_i_1 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					ms_i_1[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_i_2 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					ms_i_2[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_i_3 = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					ms_i_3[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_s_1 = new Dictionary<int, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					mi_s_1[__k] = __v;
				}
			}
			binaryReader.ReadValue(out gap_h);
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_s_2 = new Dictionary<int, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					mi_s_2[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_s_3 = new Dictionary<int, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					mi_s_3[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_s_1 = new Dictionary<string, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					ms_s_1[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_s_2 = new Dictionary<string, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					ms_s_2[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_s_3 = new Dictionary<string, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					ms_s_3[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_i_1 = new Dictionary<int, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					mi_i_1[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_i_2 = new Dictionary<int, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					mi_i_2[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_i_3 = new Dictionary<int, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					mi_i_3[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_b_1 = new Dictionary<string, byte[]>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					byte[] __v; binaryReader.ReadValue(out __v);
					ms_b_1[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_b_2 = new Dictionary<string, byte[]>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					byte[] __v; binaryReader.ReadValue(out __v);
					ms_b_2[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_b_3 = new Dictionary<string, byte[]>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					byte[] __v; binaryReader.ReadValue(out __v);
					ms_b_3[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_b_1 = new Dictionary<int, byte[]>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					byte[] __v; binaryReader.ReadValue(out __v);
					mi_b_1[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_b_2 = new Dictionary<int, byte[]>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					byte[] __v; binaryReader.ReadValue(out __v);
					mi_b_2[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_b_3 = new Dictionary<int, byte[]>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					byte[] __v; binaryReader.ReadValue(out __v);
					mi_b_3[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_p_1 = new Dictionary<string, phoneNumber>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					var __v = new phoneNumber(); __v.Deserialize(binaryReader);
					ms_p_1[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_p_2 = new Dictionary<string, phoneNumber>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					var __v = new phoneNumber(); __v.Deserialize(binaryReader);
					ms_p_2[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				ms_p_3 = new Dictionary<string, phoneNumber>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					var __v = new phoneNumber(); __v.Deserialize(binaryReader);
					ms_p_3[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_l_1 = new Dictionary<int, comboLeaf>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					var __v = new comboLeaf(); __v.Deserialize(binaryReader);
					mi_l_1[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_l_2 = new Dictionary<int, comboLeaf>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					var __v = new comboLeaf(); __v.Deserialize(binaryReader);
					mi_l_2[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				mi_l_3 = new Dictionary<int, comboLeaf>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					var __v = new comboLeaf(); __v.Deserialize(binaryReader);
					mi_l_3[__k] = __v;
				}
			}
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						pick1 = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						pick1 = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						pick1 = ((byte)2, (object)__ov);
						break;
					}
					case 3:
					{
						byte[] __ov; binaryReader.ReadValue(out __ov);
						pick1 = ((byte)3, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out gap_i);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						pick2 = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						pick2 = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						pick2 = ((byte)2, (object)__ov);
						break;
					}
					case 3:
					{
						var __ov = new phoneNumber(); __ov.Deserialize(binaryReader);
						pick2 = ((byte)3, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out gap_j);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						pick3 = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						pick3 = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						var __ov = new comboLeaf(); __ov.Deserialize(binaryReader);
						pick3 = ((byte)2, (object)__ov);
						break;
					}
					case 3:
					{
						List<int> __ov; binaryReader.ReadValue(out __ov, ProtocolCore.BasicTypeEnum.Custom);
						pick3 = ((byte)3, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			if (binaryReader.ReadByte() != 0)
			{
				ph_1 = new phoneNumber();
				ph_1.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				ph_2 = new phoneNumber();
				ph_2.Deserialize(binaryReader);
			}
			binaryReader.ReadValue(out gap_k);
			if (binaryReader.ReadByte() != 0)
			{
				ph_3 = new phoneNumber();
				ph_3.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				leaf_1 = new comboLeaf();
				leaf_1.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				leaf_2 = new comboLeaf();
				leaf_2.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				leaf_3 = new comboLeaf();
				leaf_3.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				node_1 = new comboNode();
				node_1.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				node_2 = new comboNode();
				node_2.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				node_3 = new comboNode();
				node_3.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				wrap_1 = new comboWrap();
				wrap_1.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				wrap_2 = new comboWrap();
				wrap_2.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				wrap_3 = new comboWrap();
				wrap_3.Deserialize(binaryReader);
			}
		}
	}
	public class benchLeaf : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<benchLeaf>
	{
		public const int ProtoId = 3001;
		public int id;
		public string tag;
		public byte[] blob;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(id);
			binaryWriter.WriteValue(tag);
			binaryWriter.WriteValue(blob);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out id);
			binaryReader.ReadValue(out tag);
			binaryReader.ReadValue(out blob);
		}
	}
	public class tBenchScalar : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tBenchScalar>
	{
		public const int ProtoId = 3002;
		public int a1;
		public int a2;
		public int a3;
		public int a4;
		public int a5;
		public int a6;
		public int a7;
		public int a8;
		public long b1;
		public long b2;
		public long b3;
		public long b4;
		public string s1;
		public string s2;
		public string s3;
		public string s4;
		public byte[] p1;
		public byte[] p2;
		public byte[] p3;
		public byte[] p4;
		public bool f1;
		public bool f2;
		public float f3;
		public double f4;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(a1);
			binaryWriter.WriteValue(a2);
			binaryWriter.WriteValue(a3);
			binaryWriter.WriteValue(a4);
			binaryWriter.WriteValue(a5);
			binaryWriter.WriteValue(a6);
			binaryWriter.WriteValue(a7);
			binaryWriter.WriteValue(a8);
			binaryWriter.WriteValue(b1);
			binaryWriter.WriteValue(b2);
			binaryWriter.WriteValue(b3);
			binaryWriter.WriteValue(b4);
			binaryWriter.WriteValue(s1);
			binaryWriter.WriteValue(s2);
			binaryWriter.WriteValue(s3);
			binaryWriter.WriteValue(s4);
			binaryWriter.WriteValue(p1);
			binaryWriter.WriteValue(p2);
			binaryWriter.WriteValue(p3);
			binaryWriter.WriteValue(p4);
			binaryWriter.WriteValue(f1);
			binaryWriter.WriteValue(f2);
			binaryWriter.WriteValue(f3);
			binaryWriter.WriteValue(f4);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out a1);
			binaryReader.ReadValue(out a2);
			binaryReader.ReadValue(out a3);
			binaryReader.ReadValue(out a4);
			binaryReader.ReadValue(out a5);
			binaryReader.ReadValue(out a6);
			binaryReader.ReadValue(out a7);
			binaryReader.ReadValue(out a8);
			binaryReader.ReadValue(out b1);
			binaryReader.ReadValue(out b2);
			binaryReader.ReadValue(out b3);
			binaryReader.ReadValue(out b4);
			binaryReader.ReadValue(out s1);
			binaryReader.ReadValue(out s2);
			binaryReader.ReadValue(out s3);
			binaryReader.ReadValue(out s4);
			binaryReader.ReadValue(out p1);
			binaryReader.ReadValue(out p2);
			binaryReader.ReadValue(out p3);
			binaryReader.ReadValue(out p4);
			binaryReader.ReadValue(out f1);
			binaryReader.ReadValue(out f2);
			binaryReader.ReadValue(out f3);
			binaryReader.ReadValue(out f4);
		}
	}
	public class tBenchList : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tBenchList>
	{
		public const int ProtoId = 3003;
		public List<int> scores;
		public List<string> labels;
		public List<byte[]> chunks;
		public List<benchLeaf> items;
		public List<comboLeaf> extras;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(scores);
			binaryWriter.WriteValue(labels);
			binaryWriter.WriteValue(chunks);
			binaryWriter.WriteValue(items);
			binaryWriter.WriteValue(extras);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out scores, ProtocolCore.BasicTypeEnum.Int32);
			binaryReader.ReadValue(out labels, ProtocolCore.BasicTypeEnum.String);
			binaryReader.ReadValue(out chunks, ProtocolCore.BasicTypeEnum.Binary);
			binaryReader.ReadValue(out items, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out extras, ProtocolCore.BasicTypeEnum.Custom);
		}
	}
	public class tBenchMap : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tBenchMap>
	{
		public const int ProtoId = 3004;
		public Dictionary<string, int> attrs;
		public Dictionary<int, string> rev;
		public Dictionary<string, benchLeaf> lookup;
		public Dictionary<string, phoneNumber> phones;
		public Dictionary<string, string> tags;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.Write((ushort)(attrs?.Count ?? 0));
			if (attrs != null)
			{
				foreach (var __kv in attrs)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(rev?.Count ?? 0));
			if (rev != null)
			{
				foreach (var __kv in rev)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(lookup?.Count ?? 0));
			if (lookup != null)
			{
				foreach (var __kv in lookup)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			binaryWriter.Write((ushort)(phones?.Count ?? 0));
			if (phones != null)
			{
				foreach (var __kv in phones)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			binaryWriter.Write((ushort)(tags?.Count ?? 0));
			if (tags != null)
			{
				foreach (var __kv in tags)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			{
				var __cnt = binaryReader.ReadUInt16();
				attrs = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					attrs[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				rev = new Dictionary<int, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					rev[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				lookup = new Dictionary<string, benchLeaf>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					var __v = new benchLeaf(); __v.Deserialize(binaryReader);
					lookup[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				phones = new Dictionary<string, phoneNumber>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					var __v = new phoneNumber(); __v.Deserialize(binaryReader);
					phones[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				tags = new Dictionary<string, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					tags[__k] = __v;
				}
			}
		}
	}
	public class tBenchOneof : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tBenchOneof>
	{
		public const int ProtoId = 3005;
		public int seq;
		public (byte tag, object value)? reward;
		public (byte tag, object value)? meta;
		public (byte tag, object value)? payload;
		public string tip;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(seq);
			if (reward == null || reward.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (reward.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)reward.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)reward.Value.value);
						break;
					case 3:
						binaryWriter.Write((byte)3);
						((benchLeaf)reward.Value.value).Serialize(binaryWriter);
						break;
					case 4:
						binaryWriter.Write((byte)4);
						((phoneNumber)reward.Value.value).Serialize(binaryWriter);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			if (meta == null || meta.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (meta.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)meta.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)meta.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			if (payload == null || payload.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (payload.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((byte[])payload.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)payload.Value.value);
						break;
					case 3:
						binaryWriter.Write((byte)3);
						((benchLeaf)payload.Value.value).Serialize(binaryWriter);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(tip);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out seq);
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						reward = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						reward = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						reward = ((byte)2, (object)__ov);
						break;
					}
					case 3:
					{
						var __ov = new benchLeaf(); __ov.Deserialize(binaryReader);
						reward = ((byte)3, (object)__ov);
						break;
					}
					case 4:
					{
						var __ov = new phoneNumber(); __ov.Deserialize(binaryReader);
						reward = ((byte)4, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						meta = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						meta = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						meta = ((byte)2, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						payload = null;
						break;
					case 1:
					{
						byte[] __ov; binaryReader.ReadValue(out __ov);
						payload = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						payload = ((byte)2, (object)__ov);
						break;
					}
					case 3:
					{
						var __ov = new benchLeaf(); __ov.Deserialize(binaryReader);
						payload = ((byte)3, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out tip);
		}
	}
	public class tBenchNested : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tBenchNested>
	{
		public const int ProtoId = 3006;
		public benchLeaf root;
		public phoneNumber contact;
		public comboLeaf leaf;
		public List<benchLeaf> children;
		public List<phoneNumber> contacts;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			if (root != null)
			{
				binaryWriter.Write((byte)1);
				root.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (contact != null)
			{
				binaryWriter.Write((byte)1);
				contact.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			if (leaf != null)
			{
				binaryWriter.Write((byte)1);
				leaf.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			binaryWriter.WriteValue(children);
			binaryWriter.WriteValue(contacts);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			if (binaryReader.ReadByte() != 0)
			{
				root = new benchLeaf();
				root.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				contact = new phoneNumber();
				contact.Deserialize(binaryReader);
			}
			if (binaryReader.ReadByte() != 0)
			{
				leaf = new comboLeaf();
				leaf.Deserialize(binaryReader);
			}
			binaryReader.ReadValue(out children, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out contacts, ProtocolCore.BasicTypeEnum.Custom);
		}
	}
	public class tBenchHeavy : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<tBenchHeavy>
	{
		public const int ProtoId = 3007;
		public int seq;
		public string title;
		public benchLeaf root;
		public List<benchLeaf> items;
		public List<int> scores;
		public List<string> labels;
		public List<byte[]> chunks;
		public Dictionary<string, int> attrs;
		public Dictionary<int, string> rev;
		public Dictionary<string, benchLeaf> lookup;
		public Dictionary<string, phoneNumber> phones;
		public (byte tag, object value)? reward;
		public (byte tag, object value)? meta;
		public byte[] payload;
		public List<comboLeaf> extras;

		public byte[] Serialize()
		{
			using var memoryStream = new MemoryStream();
			using var binaryWriter = new EndianBinaryWriter(EndianBitConverter.Big, memoryStream);
			Serialize(binaryWriter);
			return memoryStream.ToArray();
		}

		public void Deserialize(byte[] data)
		{
			using var memoryStream = new MemoryStream(data);
			using var binaryReader = new EndianBinaryReader(EndianBitConverter.Big, memoryStream);
			Deserialize(binaryReader);
		}
		public void Serialize(EndianBinaryWriter binaryWriter)
		{
			binaryWriter.WriteValue(seq);
			binaryWriter.WriteValue(title);
			if (root != null)
			{
				binaryWriter.Write((byte)1);
				root.Serialize(binaryWriter);
			}
			else
			{
				binaryWriter.Write((byte)0);
			}
			binaryWriter.WriteValue(items);
			binaryWriter.WriteValue(scores);
			binaryWriter.WriteValue(labels);
			binaryWriter.WriteValue(chunks);
			binaryWriter.Write((ushort)(attrs?.Count ?? 0));
			if (attrs != null)
			{
				foreach (var __kv in attrs)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(rev?.Count ?? 0));
			if (rev != null)
			{
				foreach (var __kv in rev)
				{
					binaryWriter.WriteValue(__kv.Key);
					binaryWriter.WriteValue(__kv.Value);
				}
			}
			binaryWriter.Write((ushort)(lookup?.Count ?? 0));
			if (lookup != null)
			{
				foreach (var __kv in lookup)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			binaryWriter.Write((ushort)(phones?.Count ?? 0));
			if (phones != null)
			{
				foreach (var __kv in phones)
				{
					binaryWriter.WriteValue(__kv.Key);
					__kv.Value.Serialize(binaryWriter);
				}
			}
			if (reward == null || reward.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (reward.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)reward.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)reward.Value.value);
						break;
					case 3:
						binaryWriter.Write((byte)3);
						((benchLeaf)reward.Value.value).Serialize(binaryWriter);
						break;
					case 4:
						binaryWriter.Write((byte)4);
						((phoneNumber)reward.Value.value).Serialize(binaryWriter);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			if (meta == null || meta.Value.tag == 0)
			{
				binaryWriter.Write((byte)0);
			}
			else
			{
				switch (meta.Value.tag)
				{
					case 1:
						binaryWriter.Write((byte)1);
						binaryWriter.WriteValue((int)meta.Value.value);
						break;
					case 2:
						binaryWriter.Write((byte)2);
						binaryWriter.WriteValue((string)meta.Value.value);
						break;
					default: throw new InvalidOperationException("invalid oneof tag");
				}
			}
			binaryWriter.WriteValue(payload);
			binaryWriter.WriteValue(extras);
		}
		public void Deserialize(EndianBinaryReader binaryReader)
		{
			binaryReader.ReadValue(out seq);
			binaryReader.ReadValue(out title);
			if (binaryReader.ReadByte() != 0)
			{
				root = new benchLeaf();
				root.Deserialize(binaryReader);
			}
			binaryReader.ReadValue(out items, ProtocolCore.BasicTypeEnum.Custom);
			binaryReader.ReadValue(out scores, ProtocolCore.BasicTypeEnum.Int32);
			binaryReader.ReadValue(out labels, ProtocolCore.BasicTypeEnum.String);
			binaryReader.ReadValue(out chunks, ProtocolCore.BasicTypeEnum.Binary);
			{
				var __cnt = binaryReader.ReadUInt16();
				attrs = new Dictionary<string, int>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					int __v; binaryReader.ReadValue(out __v);
					attrs[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				rev = new Dictionary<int, string>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					int __k; binaryReader.ReadValue(out __k);
					string __v; binaryReader.ReadValue(out __v);
					rev[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				lookup = new Dictionary<string, benchLeaf>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					var __v = new benchLeaf(); __v.Deserialize(binaryReader);
					lookup[__k] = __v;
				}
			}
			{
				var __cnt = binaryReader.ReadUInt16();
				phones = new Dictionary<string, phoneNumber>(__cnt);
				for (var __i = 0; __i < __cnt; __i++)
				{
					string __k; binaryReader.ReadValue(out __k);
					var __v = new phoneNumber(); __v.Deserialize(binaryReader);
					phones[__k] = __v;
				}
			}
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						reward = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						reward = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						reward = ((byte)2, (object)__ov);
						break;
					}
					case 3:
					{
						var __ov = new benchLeaf(); __ov.Deserialize(binaryReader);
						reward = ((byte)3, (object)__ov);
						break;
					}
					case 4:
					{
						var __ov = new phoneNumber(); __ov.Deserialize(binaryReader);
						reward = ((byte)4, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			{
				var __tag = binaryReader.ReadByte();
				switch (__tag)
				{
					case 0:
						meta = null;
						break;
					case 1:
					{
						int __ov; binaryReader.ReadValue(out __ov);
						meta = ((byte)1, (object)__ov);
						break;
					}
					case 2:
					{
						string __ov; binaryReader.ReadValue(out __ov);
						meta = ((byte)2, (object)__ov);
						break;
					}
					default: throw new InvalidOperationException($"invalid oneof tag: {__tag}");
				}
			}
			binaryReader.ReadValue(out payload);
			binaryReader.ReadValue(out extras, ProtocolCore.BasicTypeEnum.Custom);
		}
	}
}