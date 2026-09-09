# 跨语言测试（索引）

完整说明见 **[TESTING.md](./TESTING.md)**，包括：

- 运行环境依赖（Erlang / Node / .NET / Lua）
- `run_all` 步骤流程（正确性 + 性能）
- 向量与 benchmark 产出物
- **为何 C# 比 Erlang/TS 慢** 的原因解读

## 快速开始

```powershell
cd test
.\run_all.ps1
```

| 脚本参数 | 含义 |
|----------|------|
| `-SkipBuild` | 跳过 `rebar3 escriptize` |
| `-SkipGen` | 跳过 `genProto` 生成 |
| `-SkipBench` | 跳过性能测试 |

## 主要目录

| 路径 | 内容 |
|------|------|
| `vectors/` | 正确性 hex 向量 |
| `bench/results/` | 性能原始 JSON |
| `bench/bench_report.md` | 性能对比表（自动生成） |
| `erl/` `ts/` `cs/` `lua/` | 各语言测试脚本 |
| `gpb/` | gpb Protobuf 对比（`bench_heavy.proto` + `bench_heavy_gpb.erl`） |

协议命名约定见根目录 [README.md](../README.md#协议命名约定)。
