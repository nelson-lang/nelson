# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param([Parameter(Mandatory)][string]$Work, [Parameter(Mandatory)][string]$Launcher)
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
$allowed = [IO.Path]::GetTempPath()
$Work = [IO.Path]::GetFullPath($Work)
if (-not $Work.StartsWith($allowed, [StringComparison]::OrdinalIgnoreCase)) { throw 'Test work must be inside temporary directory.' }
$target = Join-Path $Work 'native-output'
$source = Join-Path $Work 'native-source'
New-Item -ItemType Directory -Path $target, $source | Out-Null
function Quote([string]$Value) { return "'" + $Value.Replace("'", "''") + "'" }
function Start-Probe([string]$Command) {
    $info = [Diagnostics.ProcessStartInfo]::new($Launcher)
    $info.UseShellExecute = $false
    $info.CreateNoWindow = $true
    $info.WindowStyle = [Diagnostics.ProcessWindowStyle]::Hidden
    $info.RedirectStandardOutput = $true
    $info.RedirectStandardError = $true
    $info.Arguments = Join-Arguments @('--no-ipc', '--no-user-modules', '--no-user-startup', '--quiet', '--execute', $Command)
    return [Diagnostics.Process]::Start($info)
}
function Finish-Probe($Process) {
    $stdout = $Process.StandardOutput.ReadToEndAsync()
    $stderr = $Process.StandardError.ReadToEndAsync()
    if (-not $Process.WaitForExit(60000)) { Stop-ProcessTree $Process; throw 'Recovery probe timed out.' }
    if ($Process.ExitCode -ne 0) { throw ($stdout.Result + $stderr.Result) }
    return $stdout.Result
}
$inventory = @()
foreach ($name in @('app.bin', 'readme.txt', 'buildresult.json')) {
    $old = Join-Path $target $name
    $new = Join-Path $source $name
    [IO.File]::WriteAllText($old, "OLD $name")
    [IO.File]::WriteAllText($new, "NEW $name")
    if ($name -eq 'app.bin') {
        # Hashing the moved backup gives the controller time to terminate mid-publication.
        $stream = [IO.File]::OpenWrite($old)
        try { $stream.SetLength(128MB) } finally { $stream.Dispose() }
    }
    $inventory += @{ relative = $name; sha256 = (Get-FileHash -LiteralPath $old -Algorithm SHA256).Hash.ToLowerInvariant() }
}
$encoded = ConvertTo-Json -InputObject $inventory -Compress
$recover = "ncc('--help'); __nelsonc_executable__('recover-publication', $(Quote $target));"
$lock = Join-Path $target '.nelsonc-publish.lock'
$held = [IO.File]::Open($lock, [IO.FileMode]::OpenOrCreate, [IO.FileAccess]::ReadWrite, [IO.FileShare]::None)
try {
    $process = Start-Probe "try; failed=false; try; $recover catch; failed=true; end; asserts.istrue(failed); disp('BUSY_REFUSED'); exit(0); catch ex; disp(ex.message); exit(1); end"
    try { if ((Finish-Probe $process) -notmatch 'BUSY_REFUSED') { throw 'Missing lock rejection.' } }
    finally { $process.Dispose() }
} finally { $held.Dispose() }
$publish = "ncc('--help'); __nelsonc_executable__('publish', $(Quote $source), $(Quote $target), $(Quote $encoded)); exit(0);"
$process = Start-Probe $publish
try {
    $deadline = [DateTime]::UtcNow.AddSeconds(60)
    $backup = Join-Path $target '.nelsonc-publish/previous/app.bin'
    while (-not (Test-Path -LiteralPath $backup)) {
        if ($process.HasExited) { throw ('Publication finished before interruption: ' + $process.StandardOutput.ReadToEnd()) }
        if ([DateTime]::UtcNow -gt $deadline) { throw 'Publication did not reach its backup phase.' }
        Start-Sleep -Milliseconds 2
    }
    Stop-ProcessTree $process
} finally {
    if (-not $process.HasExited) { Stop-ProcessTree $process }
    $process.Dispose()
}
if (-not (Test-Path -LiteralPath (Join-Path $target '.nelsonc-publish/journal.json'))) { throw 'No interrupted publication was retained.' }
$process = Start-Probe "try; $recover disp('RECOVERY_OK'); exit(0); catch ex; disp(ex.message); exit(1); end"
try { if ((Finish-Probe $process) -notmatch 'RECOVERY_OK') { throw 'Missing recovery completion.' } }
finally { $process.Dispose() }
foreach ($item in $inventory) {
    if ((Get-FileHash -LiteralPath (Join-Path $target $item.relative)).Hash.ToLowerInvariant() -cne $item.sha256) { throw 'Old output was not restored.' }
}
if (Test-Path -LiteralPath (Join-Path $target '.nelsonc-publish')) { throw 'Recovery left an active transaction.' }
$process = Start-Probe "try; $publish catch ex; disp(ex.message); exit(1); end"
try { $null = Finish-Probe $process } finally { $process.Dispose() }
foreach ($name in @('app.bin', 'readme.txt', 'buildresult.json')) {
    if ((Get-FileHash -LiteralPath (Join-Path $target $name)).Hash -cne (Get-FileHash -LiteralPath (Join-Path $source $name)).Hash) { throw 'Subsequent publication failed.' }
}
Write-Output 'PROCESS_RECOVERY_OK'
