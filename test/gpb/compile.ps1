# Compile gpb (if needed), generate Erlang from proto, compile generated modules.
$ErrorActionPreference = "Stop"
$GpbDir = $PSScriptRoot
$Vendor = Join-Path $GpbDir "vendor"
$ProtoDir = Join-Path $GpbDir "proto"
$GenDir = Join-Path $GpbDir "generated"
$GpbEbin = Join-Path $Vendor "_build\default\lib\gpb\ebin"
$GpbInclude = Join-Path $Vendor "include"

if (-not (Test-Path $Vendor)) {
    Write-Host "Cloning gpb into vendor/ ..."
    git clone --depth 1 https://github.com/tomas-abrahamsson/gpb (Join-Path $GpbDir "vendor")
}

if (-not (Test-Path (Join-Path $GpbEbin "gpb_compile.beam"))) {
    Write-Host "Building gpb (rebar3 compile) ..."
    Push-Location $Vendor
    try { rebar3 compile }
    finally { Pop-Location }
}

New-Item -ItemType Directory -Force -Path $GenDir | Out-Null
$Proto = Join-Path $ProtoDir "bench_heavy.proto"

Write-Host "Generating bench_heavy_pb.erl from $Proto ..."
Push-Location $GenDir
try {
    $eval = @"
        code:add_patha(""$(($GpbEbin -replace '\\','/'))""),
        code:add_patha(""$(($GpbInclude -replace '\\','/'))""),
        Proto = ""$(($Proto -replace '\\','/'))"",
        Opts = [
            {module_name_suffix, ""_pb""},
            {strings_as_binaries, true},
            {o_erl, ""$(($GenDir -replace '\\','/'))""},
            {o_hrl, ""$(($GenDir -replace '\\','/'))""}
        ],
        ok = gpb_compile:file(Proto, Opts),
        init:stop().
"@
    erl -noshell -pa $GpbEbin -eval $eval
}
finally { Pop-Location }

Write-Host "Compiling generated modules ..."
Push-Location $GenDir
try {
    erlc -I $GpbInclude -I . bench_heavy_pb.erl
}
finally { Pop-Location }

Write-Host "gpb compile ok: $GenDir"
