using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;

namespace GenProto
{
    public static class Protocol
    {
        public enum BasicTypeEnum
        {
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
                _ => throw new InvalidOperationException($"unexpect type: {value.GetType().FullName}")
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
                    throw new InvalidOperationException($"unexpect type: {value.GetType().FullName}");
            }
        }


        public static void WriteList<T>(this BinaryWriter binaryWriter, IList<T> list)
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

        public static void ReadList(this BinaryReader binaryReader, out IList list)
        {
            var length = binaryReader.ReadUInt16();
            if (length <= 0)
            {
                list = default;
                return;
            }

            list = default;
            var basicTypeEnum = (BasicTypeEnum) binaryReader.ReadByte();
            for (int idx = 0; idx < length; idx++)
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
                    default:
                        throw new ArgumentOutOfRangeException();
                }
            }
        }
    }

    public class AllType
    {
        public bool Bool;
        public sbyte Int8;
        public byte UInt8;
        public ushort UInt16;
        public short Int16;
        public int Int32;
        public uint UInt32;
        public long Int64;
        public ulong UInt64;
        public float Float;
        public double Double;
        public string String;

        public List<bool> ListBool;
        public List<sbyte> ListInt8;
        public List<byte> ListUInt8;
        public List<ushort> ListUInt16;
        public List<short> ListInt16;
        public List<int> ListInt32;
        public List<uint> ListUInt32;
        public List<long> ListInt64;
        public List<ulong> ListUInt64;
        public List<float> ListFloat;
        public List<double> ListDouble;
        public List<string> ListString;

        public static byte[] Serialize(AllType allType)
        {
            using var memoryStream = new MemoryStream();
            using var binaryWriter = new BinaryWriter(memoryStream);
            binaryWriter.Write(allType.Bool);
            binaryWriter.Write(allType.Int8);
            binaryWriter.Write(allType.UInt8);
            binaryWriter.Write(allType.UInt16);
            binaryWriter.Write(allType.Int16);
            binaryWriter.Write(allType.Int32);
            binaryWriter.Write(allType.UInt32);
            binaryWriter.Write(allType.Int64);
            binaryWriter.Write(allType.UInt64);
            binaryWriter.Write(allType.Float);
            binaryWriter.Write(allType.Double);
            binaryWriter.Write(allType.String);

            binaryWriter.WriteList(allType.ListBool);
            binaryWriter.WriteList(allType.ListInt8);
            binaryWriter.WriteList(allType.ListUInt8);
            binaryWriter.WriteList(allType.ListUInt16);
            binaryWriter.WriteList(allType.ListInt16);
            binaryWriter.WriteList(allType.ListInt32);
            binaryWriter.WriteList(allType.ListUInt32);
            binaryWriter.WriteList(allType.ListInt64);
            binaryWriter.WriteList(allType.ListUInt64);
            binaryWriter.WriteList(allType.ListFloat);
            binaryWriter.WriteList(allType.ListDouble);
            binaryWriter.WriteList(allType.ListString);

            return memoryStream.ToArray();
        }

        public static AllType Deserialize(byte[] data)
        {
            using var memoryStream = new MemoryStream(data);
            using var binaryReader = new BinaryReader(memoryStream);
            var allType = new AllType();
            allType.Bool = binaryReader.ReadBoolean();
            allType.Int8 = binaryReader.ReadSByte();
            allType.UInt8 = binaryReader.ReadByte();
            allType.UInt16 = binaryReader.ReadUInt16();
            allType.Int16 = binaryReader.ReadInt16();
            allType.Int32 = binaryReader.ReadInt32();
            allType.UInt32 = binaryReader.ReadUInt32();
            allType.Int64 = binaryReader.ReadInt64();
            allType.UInt64 = binaryReader.ReadUInt64();
            allType.Float = binaryReader.ReadSingle();
            allType.Double = binaryReader.ReadDouble();
            allType.String = binaryReader.ReadString();

            binaryReader.ReadList(out var outListBool);
            allType.ListBool = outListBool as List<bool>;
            binaryReader.ReadList(out var outListInt8);
            allType.ListInt8 = outListInt8 as List<sbyte>;
            binaryReader.ReadList(out var outListUInt8);
            allType.ListUInt8 = outListUInt8 as List<byte>;
            binaryReader.ReadList(out var outListUInt16);
            allType.ListUInt16 = outListUInt16 as List<ushort>;
            binaryReader.ReadList(out var outListInt16);
            allType.ListInt16 = outListInt16 as List<short>;
            binaryReader.ReadList(out var outListInt32);
            allType.ListInt32 = outListInt32 as List<int>;
            binaryReader.ReadList(out var outListUInt32);
            allType.ListUInt32 = outListUInt32 as List<uint>;
            binaryReader.ReadList(out var outListInt64);
            allType.ListInt64 = outListInt64 as List<long>;
            binaryReader.ReadList(out var outListUInt64);
            allType.ListUInt64 = outListUInt64 as List<ulong>;
            binaryReader.ReadList(out var outListFloat);
            allType.ListFloat = outListFloat as List<float>;
            binaryReader.ReadList(out var outListDouble);
            allType.ListDouble = outListDouble as List<double>;
            binaryReader.ReadList(out var outListString);
            allType.ListString = outListString as List<string>;

            return allType;
        }
    }
}