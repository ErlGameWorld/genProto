#Requires -Version 5.1
<#
.SYNOPSIS
  One-click cross-language codec tests for genProto.

.DESCRIPTION
  1. Build genProto escript (rebar3 escriptize)
  2. Regenerate all language outputs from proto/*.mpdf
  3. Run Erlang round-trip + export vectors + verify cross-lang files
  4. Run TypeScript / C# / Lua tests when runtimes are available

.EXAMPLE
  .\run_all.ps1
  .\run_all.ps1 -SkipBuild
#>
param(
    [switch]$SkipBuild,
    [switch]$SkipGen,
    [switch]$SkipBench
)

$ErrorActionPreference = "Stop"
$Root = Split-Path -Parent $PSScriptRoot
$TestDir = $PSScriptRoot
$VectorsDir = Join-Path $TestDir "vectors"
$Failed = @()
$Skipped = @()

# Reload PATH so newly installed runtimes are visible without restarting the IDE
$env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
            [System.Environment]::GetEnvironmentVariable("Path", "User")

function Write-Step($msg) {
    Write-Host ""
    Write-Host "==> $msg" -ForegroundColor Cyan
}

function Test-Command($name) {
    return [bool](Get-Command $name -ErrorAction SilentlyContinue)
}

function Resolve-LuaExe {
    foreach ($name in @("lua", "lua55", "lua54", "lua53", "luajit")) {
        $cmd = Get-Command $name -ErrorAction SilentlyContinue
        if ($cmd) { return $cmd.Source }
    }
    return $null
}

function Invoke-Step($label, [scriptblock]$Block) {
    Write-Step $label
    try {
        & $Block
        if ($LASTEXITCODE -and $LASTEXITCODE -ne 0) {
            throw "exit code $LASTEXITCODE"
        }
    }
    catch {
        $script:Failed += "$label`: $($_.Exception.Message)"
        Write-Host "FAILED: $label" -ForegroundColor Red
        Write-Host $_.Exception.Message -ForegroundColor Red
    }
}

function Invoke-Skip($label, $reason) {
    $script:Skipped += "$label`: $reason"
    Write-Host "SKIP: $label ($reason)" -ForegroundColor Yellow
}

New-Item -ItemType Directory -Force -Path $VectorsDir | Out-Null

if (-not $SkipBuild) {
    Invoke-Step "Build genProto (rebar3 escriptize)" {
        Push-Location $Root
        try { rebar3 escriptize }
        finally { Pop-Location }
    }
}

if (-not $SkipGen) {
    Invoke-Step "Generate code from proto/*.mpdf" {
        $Gen = Join-Path $Root "_build\default\bin\genProto.cmd"
        if (-not (Test-Path $Gen)) { $Gen = Join-Path $Root "_build\default\bin\genProto" }
        Push-Location $Root
        try {
            & $Gen proto `
                erl (Join-Path $TestDir "erl") (Join-Path $TestDir "erl") `
                ts  (Join-Path $TestDir "ts") `
                cs  (Join-Path $TestDir "cs") `
                lua (Join-Path $TestDir "lua")
        }
        finally { Pop-Location }
    }
}

Invoke-Step "Erlang codec_test" {
    Push-Location (Join-Path $TestDir "erl")
    try {
        erlc -I . codec_test.erl protoMsg.erl
        erl -noshell -pa . -eval "codec_test:run(), init:stop()."
    }
    finally { Pop-Location }
}

if (Test-Command "node") {
    Invoke-Step "TypeScript codec_test (node)" {
        Push-Location (Join-Path $TestDir "ts")
        try {
            node --experimental-strip-types codec_test.ts
        }
        finally { Pop-Location }
    }
}
else {
    Invoke-Skip "TypeScript codec_test" "node not found - install Node.js 20+"
}

if (Test-Command "dotnet") {
    Invoke-Step "C# CodecTest (dotnet)" {
        Push-Location (Join-Path $TestDir "cs")
        try {
            dotnet run --project CodecTest.csproj -c Release
        }
        finally { Pop-Location }
    }
}
else {
    Invoke-Skip "C# CodecTest" "dotnet not found - install .NET SDK 8+"
}

$LuaExe = Resolve-LuaExe
if ($LuaExe) {
    Invoke-Step "Lua codec_test ($([System.IO.Path]::GetFileName($LuaExe)))" {
        Push-Location (Join-Path $TestDir "lua")
        try { & $LuaExe codec_test.lua }
        finally { Pop-Location }
    }
}
else {
    Invoke-Skip "Lua codec_test" "lua/lua55 not found - install Lua 5.4+ and add to PATH"
}

# Re-verify cross vectors after other langs exported
Invoke-Step "Erlang verify cross vectors" {
    Push-Location (Join-Path $TestDir "erl")
    try {
        erl -noshell -pa . -eval "codec_test:verify_cross(), init:stop()."
    }
    finally { Pop-Location }
}

if (-not $SkipBench) {
    Invoke-Step "Erlang bench_heavy" {
        Push-Location (Join-Path $TestDir "erl")
        try {
            erlc -I . bench_heavy.erl protoMsg.erl
            erl -noshell -pa . -eval "bench_heavy:run(), init:stop()."
        }
        finally { Pop-Location }
    }

    Invoke-Step "gpb bench_heavy (protobuf)" {
        Push-Location (Join-Path $TestDir "gpb")
        try {
            & (Join-Path $TestDir "gpb\compile.ps1")
            erlc -I generated -I vendor/include -o . bench_heavy_gpb.erl
            erl -noshell -pa generated -pa . -eval "bench_heavy_gpb:run(), init:stop()."
        }
        finally { Pop-Location }
    }

    if (Test-Command "node") {
        Invoke-Step "TypeScript bench_heavy" {
            Push-Location (Join-Path $TestDir "ts")
            try { node --experimental-strip-types bench_heavy.ts }
            finally { Pop-Location }
        }
    }

    if (Test-Command "dotnet") {
        Invoke-Step "C# bench_heavy" {
            Push-Location (Join-Path $TestDir "cs")
            try { dotnet run --project BenchHeavy.csproj -c Release }
            finally { Pop-Location }
        }
    }

    if ($LuaExe) {
        Invoke-Step "Lua bench_heavy ($([System.IO.Path]::GetFileName($LuaExe)))" {
            Push-Location (Join-Path $TestDir "lua")
            try { & $LuaExe bench_heavy.lua }
            finally { Pop-Location }
        }
    }
    elseif (-not (Resolve-LuaExe)) {
        Invoke-Skip "Lua bench_heavy" "lua/lua55 not found"
    }

    if (Test-Command "node") {
        Invoke-Step "Aggregate bench report" {
            Push-Location (Join-Path $TestDir "bench")
            try { node --experimental-strip-types aggregate.ts }
            finally { Pop-Location }
        }
    }
}

Write-Host ""
Write-Host "========== Summary ==========" -ForegroundColor Cyan
if ($Skipped.Count -gt 0) {
    Write-Host "Skipped ($($Skipped.Count)):" -ForegroundColor Yellow
    $Skipped | ForEach-Object { Write-Host "  - $_" }
}
if ($Failed.Count -gt 0) {
    Write-Host "Failed ($($Failed.Count)):" -ForegroundColor Red
    $Failed | ForEach-Object { Write-Host "  - $_" }
    exit 1
}
Write-Host "All runnable tests passed." -ForegroundColor Green
exit 0
