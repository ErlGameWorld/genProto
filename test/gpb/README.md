# gpb 性能对比（Erlang Protobuf）

与 genProto `proto/3_bench.mpdf` 使用**相同逻辑负载**（多场景），线格式为 **Google Protobuf**，由 [gpb](https://github.com/tomas-abrahamsson/gpb) 编解码。

## 场景

| category | message | 说明 |
|----------|---------|------|
| scalar | tBenchScalar | 密集标量 |
| list | tBenchList | 大 list |
| map | tBenchMap | 多类型 map |
| oneof | tBenchOneof | 多个 oneof |
| nested | tBenchNested | 嵌套 record |
| heavy | tBenchHeavy | 综合混合 |

## 目录

| 路径 | 说明 |
|------|------|
| `vendor/` | gpb 源码（`compile.ps1` 自动 shallow clone，不提交） |
| `proto/bench_heavy.proto` | 从 mpdf 翻译的 Protobuf 定义 |
| `generated/` | `gpb_compile` 生成的 `bench_heavy_pb.erl/.hrl` |
| `bench_heavy_gpb.erl` | 多场景性能测试 → `../bench/results/gpb.json` |
| `compile.ps1` / `compile.sh` | 编译 gpb + 生成并编译 Erlang 模块 |

## 单独运行

```powershell
cd test/gpb
.\compile.ps1
erlc -I generated -I vendor/include -o . bench_heavy_gpb.erl
$env:BENCH_ITERS=10000
erl -noshell -pa generated -pa . -eval "bench_heavy_gpb:run(), init:stop()."
```

结果含每场景的 **`body_bytes`**，可在 `test/bench/bench_report.md` 与 genProto 做体积对比（线格式不同，不可字节等价）。
