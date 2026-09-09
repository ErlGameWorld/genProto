# genProto 跨语言测试指南

本文档说明 `test/` 目录下 **正确性测试** 与 **性能测试** 的环境依赖、执行流程、产出物，以及结果解读。仓库根目录 [README.md](../README.md) 侧重协议格式与 genProto 用法；本文档专注测试本身。

---

## 1. 目录概览

```
test/
  run_all.ps1 / run_all.sh     # 一键：构建 → 生成 → 正确性 → 性能 → 汇总
  TESTING.md                   # 本文档
  README.md                    # 简要索引（指向本文档）

  vectors/                     # 正确性：跨语言 hex 向量
  bench/
    results/                   # 性能：各语言 JSON 原始数据
    bench_report.md            # 性能对比表（自动生成）
    bench_summary.json
    aggregate.ts               # 汇总脚本

  erl/   codec_test.erl, bench_heavy.erl
  ts/    codec_test.ts,  bench_heavy.ts
  cs/    CodecTest.*,    BenchHeavy.*
  lua/   codec_test.lua, bench_heavy.lua
```

协议文件：

| 文件 | 用途 |
|------|------|
| `proto/0_test.mpdf` | 基础类型、map/oneof/binary 正确性 |
| `proto/2_combo.mpdf` | 组合矩阵、codegen 回归 |
| `proto/3_bench.mpdf` | 性能负载 `tBenchHeavy`（msgId 3002） |

---

## 2. 运行环境

### 2.1 必需

| 组件 | 版本建议 | 用途 |
|------|----------|------|
| **Erlang/OTP** | 系统已装 `erl` / `erlc` | 生成器宿主、正确性枢纽、性能 baseline |
| **rebar3** | 用于 `rebar3 escriptize` | 构建 `genProto` 可执行文件 |

### 2.2 可选（未安装则脚本 SKIP 对应语言）

| 组件 | 版本建议 | 用途 | 安装提示 |
|------|----------|------|----------|
| **Node.js** | 20+ | TS 测试与 `aggregate.ts` | https://nodejs.org/ |
| **.NET SDK** | 10+（目标框架 `net10.0`） | C# 测试 | https://dotnet.microsoft.com/download |
| **Lua** | 5.4 / 5.5 | Lua 测试 | Windows 常见可执行文件为 `lua55.exe` |

### 2.3 环境变量

| 变量 | 作用 |
|------|------|
| `BENCH_ITERS` | 性能测试迭代次数（默认 **50000**）；四语言应设相同值以便对比 |
| `SKIP_BUILD=1` | （shell）跳过 `rebar3 escriptize` |
| `SKIP_GEN=1` | （shell）跳过 `genProto` 重新生成 |
| PowerShell | `-SkipBuild` / `-SkipGen` / `-SkipBench` 同上 |

### 2.4 PATH 说明

- `run_all.ps1` 启动时会 **重新加载** 系统 PATH，避免 IDE 未重启读不到新装的 `dotnet` / `lua55`。
- Lua 脚本会自动识别 `lua`、`lua55`、`lua54` 等命令名。

---

## 3. 一键执行流程

```
┌─────────────────────────────────────────────────────────────┐
│  run_all.ps1 / run_all.sh                                   │
├─────────────────────────────────────────────────────────────┤
│  ① rebar3 escriptize          构建 genProto                  │
│  ② genProto proto …           生成 erl/ts/cs/lua 代码        │
├─────────────────────────────────────────────────────────────┤
│  正确性（Correctness）                                       │
│  ③ Erlang codec_test          round-trip + 导出 vectors      │
│  ④ TS / CS / Lua codec_test   round-trip + 解码 Erlang 向量  │
│  ⑤ Erlang verify_cross        解码各语言 cross_*.json        │
├─────────────────────────────────────────────────────────────┤
│  性能（Benchmark，可用 -SkipBench 跳过）                      │
│  ⑥ 各语言 bench_heavy         写 bench/results/<lang>.json   │
│  ⑦ node bench/aggregate.ts    控制台表格 + bench_report.md   │
└─────────────────────────────────────────────────────────────┘
```

### Windows

```powershell
cd test
.\run_all.ps1
```

常用变体：

```powershell
.\run_all.ps1 -SkipBuild -SkipGen    # 只跑测试，不编译/生成
.\run_all.ps1 -SkipBench             # 只跑正确性
$env:BENCH_ITERS = "100000"
.\run_all.ps1                        # 加长性能测试
```

### Linux / macOS

```bash
cd test
chmod +x run_all.sh
./run_all.sh
SKIP_BENCH=1 ./run_all.sh
BENCH_ITERS=100000 ./run_all.sh
```

---

## 4. 正确性测试

### 4.1 测什么

1. **各语言 round-trip**：编码 → 解码，样本含 `undefined`/null oneof、空 map 等。
2. **Erlang → 其他语言**：`vectors/codec_vectors.json`（Erlang 导出 `body_hex`）。
3. **其他语言 → Erlang**：`vectors/cross_<lang>_*.json` 由 TS/CS/Lua 导出，Erlang `decodeBin/2` 验证。

### 4.2 线格式

| 语言 | 网络包 | 互通向量 |
|------|--------|----------|
| Erlang / Lua | `<<MsgId:16/big, Body>>` | `codec_vectors.json` 仅 **Body** hex |
| TS / C# | 仅 **Body** | 同上 |

### 4.3 扩展用例

编辑 `test/erl/codec_test.erl` 的 `erl_cases/0`，再跑 `run_all` 即可刷新向量并驱动各语言。

---

## 5. 性能测试

### 5.1 负载说明

按场景分类（`proto/3_bench.mpdf`）：

| 场景 | 消息 | msgId | 说明 |
|------|------|------:|------|
| scalar | `tBenchScalar` | 3002 | 密集标量（int32/64、string、binary、bool/float） |
| list | `tBenchList` | 3003 | 大 list（int32 / string / binary / record） |
| map | `tBenchMap` | 3004 | 多类型 map |
| oneof | `tBenchOneof` | 3005 | 多个 oneof |
| nested | `tBenchNested` | 3006 | 嵌套 record + list |
| heavy | `tBenchHeavy` | 3007 | 综合混合（原单场景） |

gpb 对照协议：`test/gpb/proto/bench_heavy.proto`（逻辑负载对齐，线格式不同）。

### 5.2 度量方式

每种语言对每个场景：

1. 预热 200 次
2. 计时 **N 次纯编码**（`BENCH_ITERS`，默认 50000）
3. 计时 **N 次纯解码**
4. 记录 **`body_bytes`**（编码后包体字节数，便于与 gpb 做体积对比）

结果写入 `bench/results/<language>.json`（含 `cases[]` 数组）。

### 5.3 对比表

`bench/aggregate.ts` 读取所有结果，生成：

- 控制台表格（按场景分组）
- `bench/bench_report.md`：包体大小对比表 + 各场景性能表（含相对 Erlang 倍率）
- `bench/bench_summary.json`

### 5.4 如何解读结果

- 数字是 **本机、当前生成代码、当前运行时** 的相对比较，不是行业标准 benchmark。
- 应用 **相同 `BENCH_ITERS`、Release 构建（C#）、多跑几次** 减少噪声。
- **body_bytes**：genProto 与 gpb 线格式不同，只比体积趋势，不要做字节级等价断言。
- 主要用途：对比 genProto 各语言后端，以及与 gpb（Protobuf）的相对开销。

### 5.5 gpb（Protobuf）对比

`test/gpb/` 目录包含 [gpb](https://github.com/tomas-abrahamsson/gpb) 上游 clone（`vendor/`，首次 `compile.ps1` 自动拉取）及与 bench 场景等价的 **`bench_heavy.proto`**。

| 项 | genProto Erlang | gpb |
|----|-----------------|-----|
| 线格式 | 自定义（定长 int + 16-bit len string 等） | Google Protobuf |
| 结果文件 | `bench/results/erl.json` | `bench/results/gpb.json` |
| 负载 | `bench_heavy.erl` sample | `bench_heavy_gpb.erl` 同结构 sample |

`run_all` 在 Erlang bench 之后自动跑 gpb bench 并写入汇总报告。详见 `test/gpb/README.md`。

---

## 6. 为什么 C# 明显慢于 Erlang / TypeScript？

这是 **当前生成代码形态** 导致的，并不表示 .NET 只能这么慢。主要原因：

### 6.1 每次编码都重新分配缓冲区

C# 生成的 `Serialize()` 典型模式：

```csharp
using var memoryStream = new MemoryStream();
using var binaryWriter = new EndianBinaryWriter(...);
Serialize(binaryWriter);
return memoryStream.ToArray();  // 新 byte[] 每次分配
```

性能脚本在循环里 **每次迭代** 都调用 `Serialize()`，因此每轮都有 `MemoryStream` 扩容 + `ToArray()` 拷贝。  
Erlang 的 `encodeIol/encodeBin` 直接拼 **iolist / binary**，BEAM 对二进制分配高度优化，热路径更短。

### 6.2 解码大量堆分配

C# 解码 `tBenchHeavy` 时，每个 map 字段都会 `new Dictionary<...>()`，每个 list `new List<...>()`，每个嵌套 record `new benchLeaf()` 等。  
**每次 decode 都是一整棵新对象树**；Erlang 解码同样会建 term，但 record/map 在 BEAM 上分配成本更低，且无 GC 停顿模式与 generational GC 不同。

### 6.3 抽象层与动态分发

- `ProtocolCore.WriteValue<T>` / `ReadValue<T>` 含 **类型 switch**；oneof 使用 `(byte tag, object value)?`，分支上可能有 **装箱**。
- 使用 MiscUtil 的 `EndianBinaryWriter/Reader`，比 TS 里内联的 `ByteArray.write_int32`、Erlang 宏展开多一层间接调用。

### 6.4 字符串与集合

- .NET 字符串为 **UTF-16**；编解码时与 UTF-8 线格式转换有额外成本。
- `Dictionary` 迭代 + 多次 `WriteValue` 调用；Erlang 侧 map/list  comprehension 直接产出 iolist 片段。

### 6.5 测试方式对 C# 更“严”

当前 bench **刻意复用同一套 API**（公开 `Serialize()`/`Deserialize()`），包含上述所有分配，  
**没有** 复用 `MemoryStream`、没有 object pool、没有 `ArrayBuffer` 零拷贝——若优化生成器或 bench 写法，C# 数字会显著改善。

### 6.6 为何 Erlang 往往最快

- genProto **最初为 Erlang 网络场景优化**，encode 产出 iolist，decode 手写 bit syntax，路径最短。
- 本次 bench 的样本构建、计时也在 Erlang 进程内，无 FFI。
- OTP 对 binary 有 sub-binary 等优化，适合协议编解码类 workload。

### 6.7 TypeScript 介于两者之间

Node 单线程 + V8 对短循环 JIT 较好；`ByteArray` 在数组上就地写入，比 C# 每轮 `MemoryStream` 轻。  
解码仍要分配 JS 对象，故 decode 通常慢于 encode，且一般不如 Erlang decode。

---

## 7. 单独运行命令

### 正确性

```powershell
cd test/erl
erlc -I . codec_test.erl protoMsg.erl
erl -noshell -pa . -eval "codec_test:run(), init:stop()."

cd test/ts
node --experimental-strip-types codec_test.ts

cd test/cs
dotnet run --project CodecTest.csproj -c Release

cd test/lua
lua55 codec_test.lua
```

### 性能

```powershell
cd test/erl
erlc -I . bench_heavy.erl protoMsg.erl
erl -noshell -pa . -eval "bench_heavy:run(), init:stop()."

cd test/ts
node --experimental-strip-types bench_heavy.ts

cd test/cs
dotnet run --project BenchHeavy.csproj -c Release

cd test/lua
lua55 bench_heavy.lua

cd test/bench
node --experimental-strip-types aggregate.ts
```

---

## 8. 常见问题

| 现象 | 处理 |
|------|------|
| 找不到 `dotnet` / `lua55` | 安装后重启终端；或直接跑 `run_all.ps1`（会刷新 PATH） |
| C# 只跑了 bench、没跑 CodecTest | 确认 `CodecTest.csproj` 已排除 `BenchHeavy.cs` |
| `bench_report.md` 缺某语言 | 对应 runtime 未安装被 SKIP；或该语言 bench 失败 |
| Erlang bench 写 JSON 失败 | 确认 `test/bench/results/` 存在（脚本会自动创建） |
| 性能波动大 | 增大 `BENCH_ITERS`；关闭其它占 CPU 程序；C# 用 `-c Release` |

---

## 9. 后续可优化方向（非测试范围）

若希望 **C# 性能接近 TS/Erlang**，需要在 **代码生成器** 侧改进，例如：

- 提供 `Serialize(Span<byte>)` / 复用 buffer 的 API
- 解码使用 struct/预分配池（视场景而定）
- 减少 oneof 的 `object` 装箱
- 可选 Source Generator 静态 dispatch

这些属于生成器增强，当前测试 intentionally 反映「开箱即用 API」的性能。
