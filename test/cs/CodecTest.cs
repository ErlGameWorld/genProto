using System;
using System.Collections.Generic;
using System.IO;
using System.Text.Json;
using GenProto;
using MiscUtil.Conversion;
using MiscUtil.IO;

namespace GenProtoTest;

internal static class Program
{
    private static string VectorsDir =>
        Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", "vectors"));

    public static int Main(string[] args)
    {
        Console.WriteLine("CodecTest.cs");
        try
        {
            RoundTrip();
            DecodeErlangVectors();
            ExportCrossVectors();
            Console.WriteLine("CodecTest.cs: all ok");
            return 0;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"CodecTest FAILED: {ex.Message}");
            return 1;
        }
    }

    private static void RoundTrip()
    {
        RoundTripPhone();
        RoundTripTbinary();
        RoundTripTmap();
        RoundTripToneof();
    }

    private static void RoundTripPhone()
    {
        var m = new phoneNumber
        {
            number = new test { aa = "hello" },
            type = 100
        };
        var body = m.Serialize();
        var d = new phoneNumber();
        d.Deserialize(body);
        Console.WriteLine("  [cs round-trip] phoneNumber_simple ok");
    }

    private static void RoundTripTbinary()
    {
        var m = new tbinary
        {
            data = new byte[] { 1, 2, 3 },
            chunks = new List<byte[]> { new byte[] { 4, 5 }, new byte[] { 6 } }
        };
        var body = m.Serialize();
        var d = new tbinary();
        d.Deserialize(body);
        Console.WriteLine("  [cs round-trip] tbinary_basic ok");
    }

    private static void RoundTripTmap()
    {
        var m = new tmap
        {
            attrs = new Dictionary<string, int> { ["a"] = 1 },
            names = new Dictionary<int, string> { [1] = "x" },
            phones = new Dictionary<string, phoneNumber>()
        };
        var body = m.Serialize();
        var d = new tmap();
        d.Deserialize(body);
        Console.WriteLine("  [cs round-trip] tmap_full ok");
    }

    private static void RoundTripToneof()
    {
        var m = new toneof
        {
            reason = 1,
            item = (1, (object)100),
            item2 = null,
            tip = "hi"
        };
        var body = m.Serialize();
        var d = new toneof();
        d.Deserialize(body);
        Console.WriteLine("  [cs round-trip] toneof_gold_nil ok");
    }

    private static void DecodeErlangVectors()
    {
        var path = Path.Combine(VectorsDir, "codec_vectors.json");
        using var doc = JsonDocument.Parse(File.ReadAllText(path));
        foreach (var c in doc.RootElement.GetProperty("cases").EnumerateArray())
        {
            var name = c.GetProperty("name").GetString()!;
            var msgId = c.GetProperty("msgId").GetInt32();
            var bodyHex = c.GetProperty("body_hex").GetString()!;
            var body = HexToBytes(bodyHex);
            DecodeBody(msgId, body);
            Console.WriteLine($"  [cs decode erl vector] {name} ok");
        }
    }

    private static void DecodeBody(int msgId, byte[] body)
    {
        using var ms = new MemoryStream(body);
        using var br = new EndianBinaryReader(EndianBitConverter.Big, ms);
        switch (msgId)
        {
            case 2:
                new phoneNumber().Deserialize(br);
                break;
            case 37:
                new tbinary().Deserialize(br);
                break;
            case 38:
                new tmap().Deserialize(br);
                break;
            case 39:
                new toneof().Deserialize(br);
                break;
            default:
                throw new InvalidOperationException($"unsupported msgId {msgId}");
        }
    }

    private static void ExportCrossVectors()
    {
        var m = new toneof
        {
            reason = 0,
            item = null,
            item2 = null,
            tip = ""
        };
        var bodyHex = BytesToHex(m.Serialize());
        var outPath = Path.Combine(VectorsDir, "cross_cs_toneof_all_null.json");
        var json = $"{{\n  \"name\": \"toneof_all_null\",\n  \"msgId\": 39,\n  \"body_hex\": \"{bodyHex}\"\n}}\n";
        File.WriteAllText(outPath, json);
        Console.WriteLine($"  [cs export cross] {outPath}");
    }

    private static byte[] HexToBytes(string hex)
    {
        var bytes = new byte[hex.Length / 2];
        for (var i = 0; i < bytes.Length; i++)
            bytes[i] = Convert.ToByte(hex.Substring(i * 2, 2), 16);
        return bytes;
    }

    private static string BytesToHex(byte[] bytes)
    {
        return Convert.ToHexString(bytes).ToLowerInvariant();
    }
}
