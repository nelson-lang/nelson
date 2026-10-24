# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param(
    [Parameter(Mandatory)][string]$Work,
    [Parameter(Mandatory)][ValidatePattern('^[0-9a-f]{64}$')][string]$Fingerprint,
    [ValidateSet('win64', 'woa64')][string]$Architecture = 'win64'
)
$ErrorActionPreference = 'Stop'
function Stop-ProcessTree($Process) {
    # .NET Framework has no Process.Kill(entireProcessTree); the launched installers spawn children.
    if (-not $Process.HasExited) { $null = cmd /c "taskkill /PID $($Process.Id) /T /F >nul 2>&1" }
    $Process.WaitForExit()
}
function Join-Arguments([string[]]$Values) {
    $quoted = foreach ($value in $Values) {
        if ($value -match '[\s"]') {
            '"' + (($value -replace '(\\*)"', '$1$1\"') -replace '(\\+)$', '$1$1') + '"'
        } else {
            $value
        }
    }
    return $quoted -join ' '
}
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$Work = (Resolve-Path -LiteralPath $Work).Path
if (-not $Work.StartsWith((Join-Path $repo 'temp') + '\', [StringComparison]::OrdinalIgnoreCase)) {
    throw 'The fixture must stay inside repository temp.'
}
$key = "HKCU:\Software\Nelson\Runtime\$Architecture\$Fingerprint"
if (Test-Path -LiteralPath $key) { throw 'An existing runtime registration must not be modified.' }
$setup = Join-Path $Work 'package/runtime.exe'
$target = Join-Path $Work 'registered runtime'
$log = Join-Path $Work 'registered.log'
$control = Join-Path $Work 'registered-control.txt'
[IO.File]::WriteAllText($control, "agreeToLicense=yes`r`ndestinationFolder=$target`r`noutputFile=$log`r`n", [Text.UTF8Encoding]::new($true))

function Run-Setup([string]$Executable, [string]$Arguments) {
    $env:TEMP = $Work; $env:TMP = $Work
    $process = Start-Process -FilePath $Executable -ArgumentList $Arguments -WindowStyle Hidden -PassThru -Wait
    try {
        if ($process.ExitCode -ne 0) { throw "Runtime operation failed: $($process.ExitCode); $log" }
    } finally { $process.Dispose() }
}

try {
    Run-Setup $setup ('-inputfile "' + $control + '" /CURRENTUSER')
    if ((Get-ItemPropertyValue -LiteralPath $key -Name Root) -ne $target) { throw 'Missing runtime registration.' }
    if (-not (Test-Path -LiteralPath $log)) { throw 'Missing requested installation log.' }
    $record = Join-Path $Work 'registered-runtime-used.txt'
    $info = [Diagnostics.ProcessStartInfo]::new((Join-Path $Work 'runtime_cli_app.exe'))
    $info.UseShellExecute = $false
    $info.CreateNoWindow = $true
    $info.WindowStyle = [Diagnostics.ProcessWindowStyle]::Hidden
    $info.WorkingDirectory = $Work
    $info.RedirectStandardOutput = $true
    $info.RedirectStandardError = $true
    foreach ($name in @($info.Environment.Keys)) {
        if ($name.StartsWith('NELSON')) { $null = $info.Environment.Remove($name) }
    }
    $info.Environment['TEMP'] = $Work
    $info.Environment['TMP'] = $Work
    $info.Environment['PATH'] = Join-Path $env:SystemRoot 'System32'
    $info.Environment['LOCALAPPDATA'] = Join-Path $Work 'empty-local-data'
    $info.Arguments = Join-Arguments @($record)
    $process = [Diagnostics.Process]::Start($info)
    try {
        $stdout = $process.StandardOutput.ReadToEndAsync()
        $stderr = $process.StandardError.ReadToEndAsync()
        if (-not $process.WaitForExit(60000)) { Stop-ProcessTree $process; throw 'Application timed out.' }
        if ($process.ExitCode -ne 0 -or $stdout.Result -notmatch 'RUNTIME_INSTALLER_CLI_OK') {
            throw "Registered runtime application failed: $($stdout.Result) $($stderr.Result)"
        }
        $actual = [IO.Path]::GetFullPath([IO.File]::ReadAllText($record).Trim()).TrimEnd('\')
        if ($actual -ne $target) { throw "Unexpected runtime: $actual" }
    } finally { $process.Dispose() }
} finally {
    if (Test-Path -LiteralPath $target) {
        $uninstallers = @(Get-ChildItem -LiteralPath $target -Filter 'unins*.exe' -File)
        if ($uninstallers.Count -eq 1) {
            Run-Setup $uninstallers[0].FullName '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART'
        } elseif ($uninstallers.Count -gt 1) { throw 'Ambiguous runtime uninstaller.' }
    }
}
if (Test-Path -LiteralPath $key) { throw 'Runtime registration survived removal.' }
if (Test-Path -LiteralPath $target) { throw 'Empty runtime root survived removal.' }
if (-not (Test-Path -LiteralPath (Join-Path $Work 'runtime_cli_app.exe'))) { throw 'Application was removed.' }
Write-Output 'RUNTIME_CLI_REGISTRATION_OK'
