using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text.Json;
using System.Text.Json.Serialization;
using GenProto;

namespace GenProtoBench;

internal static class BenchProgram
{
    private const int DefaultIters = 50_000;
    private const int Warmup = 200;

    private static string ResultsDir =>
        Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", "bench", "results"));

    public static int Main(string[] args)
    {
        try
        {
            Run();
            return 0;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"bench_heavy FAILED: {ex}");
            return 1;
        }
    }

    private static void Run()
    {
        var iters = Iterations();
        var cases = BuildCases();
        var results = new List<CaseResult>();
        foreach (var c in cases)
            results.Add(RunCase(c, iters));

        var file = new BenchFile
        {
            Language = "csharp",
            WireFormat = "genproto",
            Iterations = iters,
            Cases = results,
        };

        Directory.CreateDirectory(ResultsDir);
        var outPath = Path.Combine(ResultsDir, "csharp.json");
        File.WriteAllText(outPath, JsonSerializer.Serialize(file, new JsonSerializerOptions { WriteIndented = true }) + "\n");
        Console.WriteLine($"bench_heavy.cs: {outPath} ({results.Count} cases, {iters} iters)");
        foreach (var r in results)
            Console.WriteLine($"  [{r.Category}] {r.Message} msgId={r.MsgId} body={r.BodyBytes}B enc={r.EncodeMs} ms ({r.EncodeOpsPerSec} ops/s) dec={r.DecodeMs} ms ({r.DecodeOpsPerSec} ops/s)");
    }

    private sealed class CaseDef
    {
        public string Category = "";
        public string Message = "";
        public int MsgId;
        public Func<object> Sample = () => new object();
        public Func<object> Factory = () => new object();
        public Func<object, byte[]> Encode = _ => Array.Empty<byte>();
        public Action<object, byte[]> Decode = (_, _) => { };
    }

    private static CaseResult RunCase(CaseDef c, int iters)
    {
        var sample = c.Sample();
        var body = c.Encode(sample);
        c.Decode(c.Factory(), body);
        for (var i = 0; i < Warmup; i++)
        {
            c.Encode(sample);
            c.Decode(c.Factory(), body);
        }

        var sw = Stopwatch.StartNew();
        for (var i = 0; i < iters; i++)
            body = c.Encode(sample);
        sw.Stop();
        var encMs = sw.Elapsed.TotalMilliseconds;

        sw.Restart();
        for (var i = 0; i < iters; i++)
            c.Decode(c.Factory(), body);
        sw.Stop();
        var decMs = sw.Elapsed.TotalMilliseconds;

        return new CaseResult
        {
            Category = c.Category,
            Message = c.Message,
            MsgId = c.MsgId,
            BodyBytes = body.Length,
            EncodeMs = Math.Round(encMs, 2),
            DecodeMs = Math.Round(decMs, 2),
            EncodeOpsPerSec = Ops(iters, encMs),
            DecodeOpsPerSec = Ops(iters, decMs),
        };
    }

    private static List<CaseDef> BuildCases() =>
    [
        new CaseDef
        {
            Category = "scalar", Message = "tBenchScalar", MsgId = 3002,
            Sample = BuildScalar, Factory = () => new tBenchScalar(),
            Encode = o => ((tBenchScalar)o).Serialize(),
            Decode = (o, b) => ((tBenchScalar)o).Deserialize(b),
        },
        new CaseDef
        {
            Category = "list", Message = "tBenchList", MsgId = 3003,
            Sample = BuildList, Factory = () => new tBenchList(),
            Encode = o => ((tBenchList)o).Serialize(),
            Decode = (o, b) => ((tBenchList)o).Deserialize(b),
        },
        new CaseDef
        {
            Category = "map", Message = "tBenchMap", MsgId = 3004,
            Sample = BuildMap, Factory = () => new tBenchMap(),
            Encode = o => ((tBenchMap)o).Serialize(),
            Decode = (o, b) => ((tBenchMap)o).Deserialize(b),
        },
        new CaseDef
        {
            Category = "oneof", Message = "tBenchOneof", MsgId = 3005,
            Sample = BuildOneof, Factory = () => new tBenchOneof(),
            Encode = o => ((tBenchOneof)o).Serialize(),
            Decode = (o, b) => ((tBenchOneof)o).Deserialize(b),
        },
        new CaseDef
        {
            Category = "nested", Message = "tBenchNested", MsgId = 3006,
            Sample = BuildNested, Factory = () => new tBenchNested(),
            Encode = o => ((tBenchNested)o).Serialize(),
            Decode = (o, b) => ((tBenchNested)o).Deserialize(b),
        },
        new CaseDef
        {
            Category = "heavy", Message = "tBenchHeavy", MsgId = 3007,
            Sample = BuildHeavy, Factory = () => new tBenchHeavy(),
            Encode = o => ((tBenchHeavy)o).Serialize(),
            Decode = (o, b) => ((tBenchHeavy)o).Deserialize(b),
        },
    ];

    private static int Iterations()
    {
        var env = Environment.GetEnvironmentVariable("BENCH_ITERS");
        return env is not null && int.TryParse(env, out var n) ? n : DefaultIters;
    }

    private static long Ops(int iters, double ms) => ms <= 0 ? 0 : (long)Math.Round(iters / (ms / 1000.0));

    private static benchLeaf Leaf(int id) => new()
    {
        id = id,
        tag = "leaf",
        blob = new byte[] { 1, 2, 3, 4 },
    };

    private static phoneNumber Phone() => new()
    {
        number = new test { aa = "bench" },
        type = 7,
    };

    private static object BuildScalar()
    {
        var m = new tBenchScalar
        {
            a1 = 1, a2 = -1, a3 = 100, a4 = -100, a5 = 1000, a6 = -1000, a7 = 100000, a8 = -100000,
            b1 = 1, b2 = -1, b3 = 9007199254740991, b4 = -9007199254740991,
            s1 = "alpha", s2 = "beta-beta", s3 = "gamma-gamma-gamma", s4 = "utf8-test",
            p1 = new byte[] { 0, 1, 2, 3 },
            p2 = new byte[32],
            p3 = new byte[64],
            p4 = new byte[128],
            f1 = true, f2 = false, f3 = 1.5f, f4 = 2.5,
        };
        Array.Fill(m.p2, (byte)9);
        Array.Fill(m.p3, (byte)7);
        Array.Fill(m.p4, (byte)3);
        return m;
    }

    private static object BuildList()
    {
        var m = new tBenchList
        {
            scores = new List<int>(),
            labels = new List<string>(),
            chunks = new List<byte[]>(),
            items = new List<benchLeaf>(),
            extras = new List<comboLeaf>(),
        };
        for (var i = 1; i <= 128; i++) m.scores.Add(i);
        for (var i = 1; i <= 48; i++) m.labels.Add($"lbl{i}");
        for (var i = 1; i <= 16; i++) m.chunks.Add(new byte[] { (byte)i, (byte)(i + 1), (byte)(i + 2), (byte)(i + 3) });
        for (var i = 1; i <= 48; i++) m.items.Add(Leaf(i));
        for (var i = 1; i <= 32; i++) m.extras.Add(new comboLeaf { id = i, tag = "x" });
        return m;
    }

    private static object BuildMap()
    {
        var m = new tBenchMap
        {
            attrs = new Dictionary<string, int>(),
            rev = new Dictionary<int, string>(),
            lookup = new Dictionary<string, benchLeaf>(),
            phones = new Dictionary<string, phoneNumber>(),
            tags = new Dictionary<string, string>(),
        };
        for (var i = 1; i <= 32; i++) m.attrs[$"k{i}"] = i;
        for (var i = 1; i <= 32; i++) m.rev[i] = $"v{i}";
        for (var i = 1; i <= 16; i++) m.lookup[$"L{i}"] = Leaf(i);
        for (var i = 1; i <= 8; i++) m.phones[$"p{i}"] = Phone();
        for (var i = 1; i <= 24; i++) m.tags[$"t{i}"] = $"tag-{i}";
        return m;
    }

    private static object BuildOneof()
    {
        var raw = new byte[64];
        Array.Fill(raw, (byte)42);
        return new tBenchOneof
        {
            seq = 7,
            reward = (1, (object)999),
            meta = (2, (object)"meta-note"),
            payload = (1, (object)raw),
            tip = "tip",
        };
    }

    private static object BuildNested()
    {
        var m = new tBenchNested
        {
            root = Leaf(0),
            contact = Phone(),
            leaf = new comboLeaf { id = 1, tag = "root" },
            children = new List<benchLeaf>(),
            contacts = new List<phoneNumber>(),
        };
        for (var i = 1; i <= 24; i++) m.children.Add(Leaf(i));
        for (var i = 1; i <= 12; i++) m.contacts.Add(Phone());
        return m;
    }

    private static object BuildHeavy()
    {
        var m = new tBenchHeavy
        {
            seq = 42,
            title = "bench heavy",
            root = Leaf(0),
            items = new List<benchLeaf>(),
            scores = new List<int>(),
            labels = new List<string>(),
            chunks = new List<byte[]>(),
            attrs = new Dictionary<string, int>(),
            rev = new Dictionary<int, string>(),
            lookup = new Dictionary<string, benchLeaf>(),
            phones = new Dictionary<string, phoneNumber>(),
            reward = (1, (object)999),
            meta = (2, (object)"meta"),
            payload = new byte[256],
            extras = new List<comboLeaf>(),
        };
        for (var i = 1; i <= 32; i++) m.items.Add(Leaf(i));
        for (var i = 1; i <= 64; i++) m.scores.Add(i);
        for (var i = 1; i <= 24; i++) m.labels.Add($"lbl{i}");
        for (var i = 1; i <= 8; i++) m.chunks.Add(new byte[] { (byte)i, (byte)(i + 1), (byte)(i + 2) });
        for (var i = 1; i <= 16; i++) m.attrs[$"k{i}"] = i;
        for (var i = 1; i <= 16; i++) m.rev[i] = $"v{i}";
        for (var i = 1; i <= 8; i++) m.lookup[$"L{i}"] = Leaf(i);
        for (var i = 1; i <= 4; i++) m.phones[$"p{i}"] = Phone();
        for (var i = 1; i <= 16; i++) m.extras.Add(new comboLeaf { id = i, tag = "x" });
        return m;
    }

    private sealed class BenchFile
    {
        [JsonPropertyName("language")] public string Language { get; set; } = "";
        [JsonPropertyName("wire_format")] public string WireFormat { get; set; } = "";
        [JsonPropertyName("iterations")] public int Iterations { get; set; }
        [JsonPropertyName("cases")] public List<CaseResult> Cases { get; set; } = new();
    }

    private sealed class CaseResult
    {
        [JsonPropertyName("category")] public string Category { get; set; } = "";
        [JsonPropertyName("message")] public string Message { get; set; } = "";
        [JsonPropertyName("msgId")] public int MsgId { get; set; }
        [JsonPropertyName("body_bytes")] public int BodyBytes { get; set; }
        [JsonPropertyName("encode_ms")] public double EncodeMs { get; set; }
        [JsonPropertyName("decode_ms")] public double DecodeMs { get; set; }
        [JsonPropertyName("encode_ops_per_sec")] public long EncodeOpsPerSec { get; set; }
        [JsonPropertyName("decode_ops_per_sec")] public long DecodeOpsPerSec { get; set; }
    }
}
