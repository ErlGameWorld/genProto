using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;

namespace ProtoMsg
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
}

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