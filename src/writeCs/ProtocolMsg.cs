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
    }

    public class AllType : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<AllType>
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
        public SubType SubType;
        public List<SubType> ListSubType;
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
            binaryWriter.WriteValue(Bool);
            binaryWriter.WriteValue(Int8);
            binaryWriter.WriteValue(UInt8);
            binaryWriter.WriteValue(UInt16);
            binaryWriter.WriteValue(Int16);
            binaryWriter.WriteValue(Int32);
            binaryWriter.WriteValue(UInt32);
            binaryWriter.WriteValue(Int64);
            binaryWriter.WriteValue(UInt64);
            binaryWriter.WriteValue(Float);
            binaryWriter.WriteValue(Double);
            binaryWriter.WriteValue(String);
            binaryWriter.WriteValue(SubType);
            binaryWriter.WriteValue(ListSubType);
            binaryWriter.WriteValue(ListBool);
            binaryWriter.WriteValue(ListInt8);
            binaryWriter.WriteValue(ListUInt8);
            binaryWriter.WriteValue(ListUInt16);
            binaryWriter.WriteValue(ListInt16);
            binaryWriter.WriteValue(ListInt32);
            binaryWriter.WriteValue(ListUInt32);
            binaryWriter.WriteValue(ListInt64);
            binaryWriter.WriteValue(ListUInt64);
            binaryWriter.WriteValue(ListFloat);
            binaryWriter.WriteValue(ListDouble);
            binaryWriter.WriteValue(ListString);
        }


        public void Deserialize(BinaryReader binaryReader)
        {
            binaryReader.ReadValue(out Bool);
            binaryReader.ReadValue(out Int8);
            binaryReader.ReadValue(out UInt8);
            binaryReader.ReadValue(out UInt16);
            binaryReader.ReadValue(out Int16);
            binaryReader.ReadValue(out Int32);
            binaryReader.ReadValue(out UInt32);
            binaryReader.ReadValue(out Int64);
            binaryReader.ReadValue(out UInt64);
            binaryReader.ReadValue(out Float);
            binaryReader.ReadValue(out Double);
            binaryReader.ReadValue(out String);
            binaryReader.ReadValue(out SubType);
            binaryReader.ReadValue(out ListSubType);
            binaryReader.ReadValue(out ListBool);
            binaryReader.ReadValue(out ListInt8);
            binaryReader.ReadValue(out ListUInt8);
            binaryReader.ReadValue(out ListUInt16);
            binaryReader.ReadValue(out ListInt16);
            binaryReader.ReadValue(out ListInt32);
            binaryReader.ReadValue(out ListUInt32);
            binaryReader.ReadValue(out ListInt64);
            binaryReader.ReadValue(out ListUInt64);
            binaryReader.ReadValue(out ListFloat);
            binaryReader.ReadValue(out ListDouble);
            binaryReader.ReadValue(out ListString);
        }
    }

    public class SubType : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<SubType>
    {
        public int Int32;

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
            binaryWriter.WriteValue(Int32);
        }

        public void Deserialize(BinaryReader binaryReader)
        {
            binaryReader.ReadValue(out Int32);
        }
    }

    public class Test : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<SubType>
    {
        public string aa;
        public void Serialize(BinaryWriter binaryWriter)
        {
            binaryWriter.WriteValue(aa);
        }

        public byte[] Serialize()
        {
            using var memoryStream = new MemoryStream();
            using var binaryWriter = new BinaryWriter(memoryStream);
            Serialize(binaryWriter);
            return memoryStream.ToArray();
        }

        public void Deserialize(BinaryReader binaryReader)
        {
            binaryReader.ReadValue(out aa);
        }

        public void Deserialize(byte[] data)
        {
            using var memoryStream = new MemoryStream(data);
            using var binaryReader = new BinaryReader(memoryStream);
            Deserialize(binaryReader);
        }
    }

    public class PhoneNumber : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<SubType>
    {
        public Test number;
        public int type;
        public void Serialize(BinaryWriter binaryWriter)
        {
            binaryWriter.WriteValue(number);
            binaryWriter.WriteValue(type);
        }

        public byte[] Serialize()
        {
            using var memoryStream = new MemoryStream();
            using var binaryWriter = new BinaryWriter(memoryStream);
            Serialize(binaryWriter);
            return memoryStream.ToArray();
        }

        public void Deserialize(BinaryReader binaryReader)
        {
            binaryReader.ReadValue(out number);
            binaryReader.ReadValue(out type);
        }

        public void Deserialize(byte[] data)
        {
            using var memoryStream = new MemoryStream(data);
            using var binaryReader = new BinaryReader(memoryStream);
            Deserialize(binaryReader);
        }
    }
    
    public class Person : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<SubType>
    {
        public string Name;
        public int id;
        public string email;
        public List<PhoneNumber> phone;
        
        
        public void Serialize(BinaryWriter binaryWriter)
        {
            binaryWriter.WriteValue(Name);
            binaryWriter.WriteValue(id);
            binaryWriter.WriteValue(email);
            binaryWriter.WriteValue(phone);
        }

        public byte[] Serialize()
        {
            using var memoryStream = new MemoryStream();
            using var binaryWriter = new BinaryWriter(memoryStream);
            Serialize(binaryWriter);
            return memoryStream.ToArray();
        }

        public void Deserialize(BinaryReader binaryReader)
        {
            binaryReader.ReadValue(out Name);
            binaryReader.ReadValue(out id);
            binaryReader.ReadValue(out email);
            binaryReader.ReadValue(out phone);
        }

        public void Deserialize(byte[] data)
        {
            using var memoryStream = new MemoryStream(data);
            using var binaryReader = new BinaryReader(memoryStream);
            Deserialize(binaryReader);
        }
    }

    public class AddressBook : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<SubType>
    {
        public List<Person> person;
        public List<Person> other;
        public void Serialize(BinaryWriter binaryWriter)
        {
            binaryWriter.WriteValue(person);
            binaryWriter.WriteValue(other);
        }

        public byte[] Serialize()
        {
            using var memoryStream = new MemoryStream();
            using var binaryWriter = new BinaryWriter(memoryStream);
            Serialize(binaryWriter);
            return memoryStream.ToArray();
        }

        public void Deserialize(BinaryReader binaryReader)
        {
            binaryReader.ReadValue(out person);
            binaryReader.ReadValue(out other);
        }

        public void Deserialize(byte[] data)
        {
            using var memoryStream = new MemoryStream(data);
            using var binaryReader = new BinaryReader(memoryStream);
            Deserialize(binaryReader);
        }
    }
}