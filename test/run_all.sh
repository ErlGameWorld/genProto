#!/usr/bin/env bash
# One-click cross-language codec tests for genProto
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TEST_DIR="$(cd "$(dirname "$0")" && pwd)"
VECTORS_DIR="$TEST_DIR/vectors"
FAILED=()
SKIPPED=()

step() { echo ""; echo "==> $1"; }
skip() { SKIPPED+=("$1: $2"); echo "SKIP: $1 ($2)"; }

mkdir -p "$VECTORS_DIR"

if [[ "${SKIP_BUILD:-}" != "1" ]]; then
  step "Build genProto (rebar3 escriptize)"
  (cd "$ROOT" && rebar3 escriptize)
fi

if [[ "${SKIP_GEN:-}" != "1" ]]; then
  step "Generate code from proto/*.mpdf"
  GEN="$ROOT/_build/default/bin/genProto"
  (cd "$ROOT" && "$GEN" proto \
    erl "$TEST_DIR/erl" "$TEST_DIR/erl" \
    ts  "$TEST_DIR/ts" \
    cs  "$TEST_DIR/cs" \
    lua "$TEST_DIR/lua")
fi

run_or_fail() {
  local label="$1"; shift
  step "$label"
  if "$@"; then return 0; fi
  FAILED+=("$label")
  return 0
}

run_or_fail "Erlang codec_test" bash -c "
  cd '$TEST_DIR/erl' &&
  erlc -I . codec_test.erl protoMsg.erl &&
  erl -noshell -pa . -eval 'codec_test:run(), init:stop().'
"

if command -v node >/dev/null 2>&1; then
  run_or_fail "TypeScript codec_test" bash -c "
    cd '$TEST_DIR/ts' && node --experimental-strip-types codec_test.ts
  "
else
  skip "TypeScript codec_test" "node not found"
fi

if command -v dotnet >/dev/null 2>&1; then
  run_or_fail "C# CodecTest" bash -c "
    cd '$TEST_DIR/cs' && dotnet run --project CodecTest.csproj -c Release
  "
else
  skip "C# CodecTest" "dotnet not found"
fi

if command -v lua >/dev/null 2>&1; then
  LUA=lua
elif command -v lua55 >/dev/null 2>&1; then
  LUA=lua55
elif command -v lua54 >/dev/null 2>&1; then
  LUA=lua54
else
  LUA=
fi

if [ -n "$LUA" ]; then
  run_or_fail "Lua codec_test" bash -c "
    cd '$TEST_DIR/lua' && $LUA codec_test.lua
  "
else
  skip "Lua codec_test" "lua/lua55 not found"
fi

step "Erlang verify cross vectors"
(cd "$TEST_DIR/erl" && erl -noshell -pa . -eval 'codec_test:verify_cross(), init:stop().')

if [[ "${SKIP_BENCH:-}" != "1" ]]; then
  run_or_fail "Erlang bench_heavy" bash -c "
    cd '$TEST_DIR/erl' &&
    erlc -I . bench_heavy.erl protoMsg.erl &&
    erl -noshell -pa . -eval 'bench_heavy:run(), init:stop().'
  "

  run_or_fail "gpb bench_heavy (protobuf)" bash -c "
    cd '$TEST_DIR/gpb' &&
    bash compile.sh &&
    erlc -I generated -I vendor/include -o . bench_heavy_gpb.erl &&
    erl -noshell -pa generated -pa . -eval 'bench_heavy_gpb:run(), init:stop().'
  "

  if command -v node >/dev/null 2>&1; then
    run_or_fail "TypeScript bench_heavy" bash -c "
      cd '$TEST_DIR/ts' && node --experimental-strip-types bench_heavy.ts
    "
  fi

  if command -v dotnet >/dev/null 2>&1; then
    run_or_fail "C# bench_heavy" bash -c "
      cd '$TEST_DIR/cs' && dotnet run --project BenchHeavy.csproj -c Release
    "
  fi

  if [ -n "${LUA:-}" ]; then
    run_or_fail "Lua bench_heavy" bash -c "
      cd '$TEST_DIR/lua' && $LUA bench_heavy.lua
    "
  fi

  if command -v node >/dev/null 2>&1; then
    run_or_fail "Aggregate bench report" bash -c "
      cd '$TEST_DIR/bench' && node --experimental-strip-types aggregate.ts
    "
  fi
fi

echo ""
echo "========== Summary =========="
if ((${#SKIPPED[@]})); then
  echo "Skipped (${#SKIPPED[@]}):"
  printf '  - %s\n' "${SKIPPED[@]}"
fi
if ((${#FAILED[@]})); then
  echo "Failed (${#FAILED[@]}):"
  printf '  - %s\n' "${FAILED[@]}"
  exit 1
fi
echo "All runnable tests passed."
