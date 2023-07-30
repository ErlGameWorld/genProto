-module(gCsGen).

-export([
   genCs/4
]).

protoHeader() ->
   <<"using System;
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
                                throw new InvalidOperationException($\"unexpect type: {value.GetType().FullName}\");
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
                throw new InvalidOperationException($\"error type: {typeof(T).FullName}\");
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
    }">>.

protoEnd() ->
   <<"}">>.

spellClassHead(MsgName, MsgId) ->
   <<"\tpublic class ", MsgName/binary, " : ProtocolCore.ISerialize, ProtocolCore.IDeserialize<", MsgName/binary, ">\n\t{\n\t\tpublic const int ProtoId = ", (integer_to_binary(MsgId))/binary, ";\n">>.

spellClassMember(FieldList) ->
   <<<<(gCsField:builtMemberStr(OneTypeName))/binary>> || OneTypeName <- FieldList>>.

spellCalssDSTem() ->
   <<"\n\t\tpublic byte[] Serialize()\n\t\t{\n\t\t\tusing var memoryStream = new MemoryStream();
\t\t\tusing var binaryWriter = new BinaryWriter(memoryStream);
\t\t\tSerialize(binaryWriter);
\t\t\treturn memoryStream.ToArray();
\t\t}

\t\tpublic void Deserialize(byte[] data)
\t\t{
\t\t\tusing var memoryStream = new MemoryStream(data);
\t\t\tusing var binaryReader = new BinaryReader(memoryStream);
\t\t\tDeserialize(binaryReader);
\t\t}\n">>.

spellCalssSerialize(FieldList) ->
   FunHead = <<"\t\tpublic void Serialize(BinaryWriter binaryWriter)\n\t\t{\n">>,
   FunBody = <<<<"\t\t\tbinaryWriter.WriteValue(", NameStr/binary, ");\n">> || {_TypeStr, NameStr} <- FieldList>>,
   <<FunHead/binary, FunBody/binary, "\t\t}\n">>.

spellCalssDeserialize(FieldList) ->
   FunHead = <<"\t\tpublic void Deserialize(BinaryReader binaryReader)\n\t\t{\n">>,
   FunBody = <<<<"\t\t\tbinaryReader.ReadValue(out ", NameStr/binary, ");\n">> || {_TypeStr, NameStr} <- FieldList>>,
   <<FunHead/binary, FunBody/binary, "\t\t}\n">>.

spellClassEnd() ->
   <<"\t}\n">>.

genCs(SortedSProtoList, _SortedErrList, CSDir, _) ->
   FunSpell =
      fun({MsgName, MsgId, FieldList}, ClassBinAcc) ->
         H = spellClassHead(MsgName, MsgId),
         M = spellClassMember(FieldList),
         DS = spellCalssDSTem(),
         S = spellCalssSerialize(FieldList),
         D = spellCalssDeserialize(FieldList),
         E = spellClassEnd(),
         <<ClassBinAcc/binary, H/binary, M/binary, DS/binary, S/binary, D/binary, E/binary>>
      end,
   LastClassBinAcc = lists:foldl(FunSpell, <<>>, SortedSProtoList),

   %% todo error code

   CSHeaderStr = protoHeader(),
   CSEndStr = protoEnd(),
   OutputCsStr = <<CSHeaderStr/binary, LastClassBinAcc/binary, CSEndStr/binary>>,
   CSFilename = do_write_cs(CSDir, protoMsg, OutputCsStr),

   io:format("protoConvert cs dir : ~s ~n", [CSDir]),
   io:format("protoConvert to cs file ~s succ.~n", [CSFilename]),
   ok.

do_write_cs(OutDir, Mod, BinStr) ->
   Filename = filename:join([OutDir, atom_to_list(Mod) ++ ".cs"]),
   case file:write_file(Filename, BinStr) of
      ok ->
         ok;
      _Ret ->
         io:format("write to cs file error:~p ~n", [_Ret])
   end,
   Filename.

