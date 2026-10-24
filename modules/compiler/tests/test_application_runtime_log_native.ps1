#Requires -Version 7.0
# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param([string]$CMake = 'cmake')
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$root = Join-Path $repo ('temp/compiler-runtime-log-' + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $root | Out-Null
$ctest = Join-Path (Split-Path (Get-Command $CMake).Source -Parent) 'ctest'
& $CMake -S (Join-Path $PSScriptRoot 'fixtures/runtime_log') -B (Join-Path $root 'build') *> (Join-Path $root 'configure.log')
if ($LASTEXITCODE -ne 0) { throw "Configuration failed: $root" }
& $CMake --build (Join-Path $root 'build') --config Release *> (Join-Path $root 'build.log')
if ($LASTEXITCODE -ne 0) { throw "Build failed: $root" }
& $ctest --test-dir (Join-Path $root 'build') -C Release --output-on-failure *> (Join-Path $root 'test.log')
$result = $LASTEXITCODE
Get-Content (Join-Path $root 'test.log')
if ($result -ne 0) { throw "Native runtime log failed: $root" }
$suffix = if ($IsWindows) { '.exe' } else { '' }
$executables = @(Get-ChildItem -LiteralPath (Join-Path $root 'build') -Recurse -File -Filter "runtime_log_unicode$suffix")
if ($executables.Count -ne 1) { throw 'Expected one Unicode logging test executable.' }
$utf8 = [Text.UTF8Encoding]::new($false, $true)
$message = "nelsonc: Initializing application runtime.`n"
$log = Join-Path $root 'concurrent-runtime.log'
[IO.File]::WriteAllText($log, "EXISTING`n", $utf8)
$workers = [Collections.Generic.List[object]]::new()
try {
    foreach ($id in 0..5) {
        $info = [Diagnostics.ProcessStartInfo]::new($executables[0].FullName)
        $info.UseShellExecute = $false
        $info.CreateNoWindow = $true
        $info.WindowStyle = [Diagnostics.ProcessWindowStyle]::Hidden
        $info.RedirectStandardOutput = $true
        $info.RedirectStandardError = $true
        foreach ($argument in @($root, 'writer', [string]$id)) { $info.ArgumentList.Add($argument) }
        $process = [Diagnostics.Process]::Start($info)
        $workers.Add(@{ Process = $process; Output = $process.StandardOutput.ReadToEndAsync(); Error = $process.StandardError.ReadToEndAsync() })
    }
    $deadline = [DateTime]::UtcNow.AddSeconds(5)
    while (@(Get-ChildItem -LiteralPath $root -Filter 'writer-ready-*' -File).Count -ne 6) {
        if ([DateTime]::UtcNow -gt $deadline) { throw 'Concurrent writers did not become ready.' }
        Start-Sleep -Milliseconds 10
    }
    [IO.File]::WriteAllText((Join-Path $root 'writers-start'), 'GO', $utf8)
    foreach ($worker in $workers) {
        if (-not $worker.Process.WaitForExit(20000)) { throw 'Concurrent writer timeout.' }
        if ($worker.Process.ExitCode -ne 0) { throw "Concurrent writer failed: $($worker.Error.Result)" }
    }
    $bytes = [IO.File]::ReadAllBytes($log)
    $text = $utf8.GetString($bytes)
    $count = $text.Split(@($message), [StringSplitOptions]::None).Count - 1
    if ($count -ne 6 -or -not $text.StartsWith("EXISTING`n")) { throw 'Lost initialization or preexisting log.' }
    $payload = $text.Substring(9).Replace($message, '')
    foreach ($character in @([string][char]0x00e9, [string][char]0x20ac, [char]::ConvertFromUtf32(0x1f600))) {
        if ($payload.Split(@($character), [StringSplitOptions]::None).Count - 1 -ne 120000) {
            throw 'Concurrent log lost or duplicated Unicode output.'
        }
        $payload = $payload.Replace($character, '')
    }
    if ($payload.Split("`n").Count - 1 -ne 120000) { throw 'Lost line breaks.' }
    $payload = $payload.Replace("`n", '')
    foreach ($id in 0..5) {
        foreach ($code in @((65 + $id), (97 + $id))) {
            $marker = [string][char]$code
            if ($payload.Split(@($marker), [StringSplitOptions]::None).Count - 1 -ne 10000) {
                throw 'Concurrent log lost or duplicated process output.'
            }
            $payload = $payload.Replace($marker, '')
        }
    }
    if ($payload.Length) { throw 'Unexpected bytes in concurrent output.' }
    "RUNTIME_LOG_CONCURRENT_OK=6 processes, 12 streams, $($bytes.Length) bytes"
} finally {
    foreach ($worker in $workers) {
        if (-not $worker.Process.HasExited) { $worker.Process.Kill($true); $worker.Process.WaitForExit() }
        $worker.Process.Dispose()
    }
}
"RUNTIME_LOG_NATIVE_OK=$root"
