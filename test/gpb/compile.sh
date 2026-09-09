#!/usr/bin/env bash
set -euo pipefail
GPB_DIR="$(cd "$(dirname "$0")" && pwd)"
VENDOR="$GPB_DIR/vendor"
PROTO_DIR="$GPB_DIR/proto"
GEN_DIR="$GPB_DIR/generated"
GPB_EBIN="$VENDOR/_build/default/lib/gpb/ebin"
GPB_INCLUDE="$VENDOR/include"
PROTO="$PROTO_DIR/bench_heavy.proto"

if [[ ! -d "$VENDOR" ]]; then
  echo "Cloning gpb into vendor/ ..."
  git clone --depth 1 https://github.com/tomas-abrahamsson/gpb "$VENDOR"
fi

if [[ ! -f "$GPB_EBIN/gpb_compile.beam" ]]; then
  echo "Building gpb (rebar3 compile) ..."
  (cd "$VENDOR" && rebar3 compile)
fi

mkdir -p "$GEN_DIR"

echo "Generating bench_heavy_pb.erl from $PROTO ..."
erl -noshell -pa "$GPB_EBIN" -eval "
  Proto = \"$PROTO\",
  Opts = [
    {module_name_suffix, \"_pb\"},
    {strings_as_binaries, true},
    {o_erl, \"$GEN_DIR\"},
    {o_hrl, \"$GEN_DIR\"}
  ],
  ok = gpb_compile:file(Proto, Opts),
  init:stop()."

echo "Compiling generated modules ..."
erlc -I "$GPB_INCLUDE" -I "$GEN_DIR" -o "$GEN_DIR" "$GEN_DIR/bench_heavy_pb.erl"

echo "gpb compile ok: $GEN_DIR"
