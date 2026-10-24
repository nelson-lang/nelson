# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param([Parameter(Mandatory)][string]$Work, [Parameter(Mandatory)][string]$First,
    [Parameter(Mandatory)][string]$Second, [string]$AppFirst, [string]$AppSecond)
$ErrorActionPreference = 'Stop'
# Completion bounds for the installers and recovered applications launched
# below. They are generous so a heavily loaded host - for example the full
# test suite running many real installers at once - does not trip them, while
# still bounding a genuine hang. NELSON_RECOVERY_TIMEOUT_MS overrides them.
$installerTimeoutMs = 180000
if ($env:NELSON_RECOVERY_TIMEOUT_MS) { $installerTimeoutMs = [int]$env:NELSON_RECOVERY_TIMEOUT_MS }
$lockTimeoutMs = 60000
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
$allowed = [IO.Path]::GetFullPath((Join-Path $repo 'temp')) + [IO.Path]::DirectorySeparatorChar
$Work = [IO.Path]::GetFullPath($Work)
if (-not $Work.StartsWith($allowed, [StringComparison]::OrdinalIgnoreCase)) { throw 'Recovery work must stay in repository temp.' }

function Run-Setup([string]$Executable, [string]$Destination, [string]$Pause = '', [switch]$Uninstall,
    [string]$Portable = '1', [switch]$KeepLock) {
    $id = [Guid]::NewGuid().ToString('N')
    $log = Join-Path $Work "$id.log"
    $marker = Join-Path $Work "$id.pause"
    $unknown = ''
    if ($Uninstall) {
        # Wait for the copied uninstaller process too, before another runtime operation.
        $env:TEMP = $Work; $env:TMP = $Work
        $process = Start-Process -FilePath $Executable -WindowStyle Hidden -PassThru -Wait `
            -ArgumentList "/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /LOG=`"$log`""
        try { return @{Code = $process.ExitCode; Log = $log} }
        finally { $process.Dispose() }
    }
    $info = [Diagnostics.ProcessStartInfo]::new($Executable)
    $info.UseShellExecute = $false
    $info.CreateNoWindow = $true
    $info.WindowStyle = [Diagnostics.ProcessWindowStyle]::Hidden
    $info.Environment['TEMP'] = $Work
    $info.Environment['TMP'] = $Work
    $info.Arguments = Join-Arguments @('/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART', "/LOG=$log")
    if (-not $Uninstall) {
        $info.Arguments += ' ' + (Join-Arguments @('/SP-', '/CURRENTUSER', "/PORTABLE=$Portable", "/DIR=$Destination", "/RECOVERYPAUSE=$Pause", "/RECOVERYMARKER=$marker"))
    }
    $process = [Diagnostics.Process]::Start($info)
    try {
        if ($Pause) {
            $deadline = [DateTime]::UtcNow.AddSeconds(30)
            while (-not (Test-Path -LiteralPath $marker)) {
                if ($process.HasExited -or [DateTime]::UtcNow -gt $deadline) { throw "No $Pause pause: $(Get-Content -LiteralPath $log -Raw)" }
                Start-Sleep -Milliseconds 10
            }
            if ($Pause -eq 'activation-lock') {
                $prepared = @(Get-ChildItem -LiteralPath $Destination -Directory -Filter '.nelson-runtime-prepare-*')
                if ($prepared.Count -ne 1) { throw 'Expected one journal preparation to lock.' }
                $reader = [IO.File]::Open((Join-Path $prepared[0].FullName 'journal.ini'), 'Open', 'Read', 'ReadWrite')
                try {
                    [IO.File]::WriteAllText(($marker + '.release'), 'CONTINUE')
                    $deadline = [DateTime]::UtcNow.AddSeconds(5)
                    do {
                        $retried = (Get-Content -LiteralPath $log -Raw) -match 'Retrying busy runtime recovery move'
                        if ($process.HasExited -or [DateTime]::UtcNow -gt $deadline) { throw "Missing bounded retry; see $log" }
                        if (-not $retried) { Start-Sleep -Milliseconds 10 }
                    } until ($retried)
                    if ($KeepLock -and -not $process.WaitForExit($lockTimeoutMs)) { throw 'Persistent reader lock was not bounded.' }
                } finally { $reader.Dispose() }
            } elseif ($Pause -eq 'finalization-failure') {
                $unknown = Join-Path $Destination '.nelson-runtime-update/unclaimed.txt'
                [IO.File]::WriteAllText($unknown, 'USER DATA')
                [IO.File]::WriteAllText(($marker + '.release'), 'CONTINUE')
            } else {
                Stop-ProcessTree $process
                return @{Code = $process.ExitCode; Log = $log}
            }
        }
        if (-not $process.WaitForExit($installerTimeoutMs)) { throw "Installer timeout: $log" }
        return @{Code = $process.ExitCode; Log = $log}
    } finally {
        if (-not $process.HasExited) { Stop-ProcessTree $process }
        $process.Dispose()
        if ($unknown) {
            if ((Get-Content -LiteralPath $unknown -Raw) -cne 'USER DATA') { throw 'Finalization changed unclaimed content.' }
            Remove-Item -LiteralPath $unknown
        }
    }
}
function Require-Success($Result) { if ($Result.Code -ne 0) { throw "Installer failed: $(Get-Content -LiteralPath $Result.Log -Raw)" } }
function Snapshot([string]$Directory) {
    return @((Get-ChildItem -LiteralPath $Directory -Recurse -File | Sort-Object FullName | ForEach-Object {
        $_.FullName + ':' + (Get-FileHash -LiteralPath $_.FullName -Algorithm SHA256).Hash
    })) -join "`n"
}
function Reject-Unchanged([string]$Installer, [string]$Destination, [string]$Message, [switch]$Uninstall, [string]$Portable = '1') {
    $before = Snapshot $Destination
    $result = Run-Setup $Installer $Destination -Uninstall:$Uninstall -Portable $Portable
    if ($result.Code -eq 0 -or (Get-Content -LiteralPath $result.Log -Raw) -notmatch $Message) { throw "Recovery refusal missing: $Message; $($result.Log)" }
    if ((Snapshot $Destination) -cne $before) { throw 'Refused recovery changed existing files.' }
}

function Check-Refusals([string]$Destination) {
    $journal = Join-Path $Destination '.nelson-runtime-update'
    Reject-Unchanged $Second $Destination 'different installation settings' -Portable '0'
    foreach ($relative in @('unknown.txt', 'previous/unknown.txt', 'next/modules/unknown.txt')) {
        $unknown = Join-Path $journal $relative
        [IO.File]::WriteAllText($unknown, 'USER CONTENT')
        try { Reject-Unchanged $Second $Destination 'Unknown runtime recovery content' }
        finally { Remove-Item -LiteralPath $unknown }
    }
    $unknown = Join-Path $journal 'unknown-directory'
    $null = New-Item -ItemType Directory -Path $unknown
    try { Reject-Unchanged $Second $Destination 'Unknown runtime recovery content' }
    finally { Remove-Item -LiteralPath $unknown }
    $outside = Join-Path $Work 'redirect-target'
    $link = Join-Path $journal 'redirected'
    $null = New-Item -ItemType Directory -Path $outside
    [IO.File]::WriteAllText((Join-Path $outside 'user.txt'), 'UNCHANGED')
    $null = New-Item -ItemType Junction -Path $link -Value $outside
    try {
        Reject-Unchanged $Second $Destination 'Runtime paths cannot contain reparse points'
        if ((Get-Content -LiteralPath (Join-Path $outside 'user.txt') -Raw) -cne 'UNCHANGED') { throw 'Recovery changed redirected data.' }
    } finally {
        [IO.Directory]::Delete($link)
        Remove-Item -LiteralPath (Join-Path $outside 'user.txt')
        Remove-Item -LiteralPath $outside
    }
    foreach ($relative in @('runtime.json', 'modules/modules.m', '.nelson-runtime-update/next/runtime.json')) {
        $path = Join-Path $Destination $relative
        $bytes = [IO.File]::ReadAllBytes($path)
        [IO.File]::WriteAllText($path, 'CHANGED METADATA')
        try { Reject-Unchanged $Second $Destination 'Runtime metadata changed after interruption|Changed runtime recovery metadata' }
        finally { [IO.File]::WriteAllBytes($path, $bytes) }
    }
    $marker = Join-Path $journal 'complete'
    [IO.File]::WriteAllText($marker, 'INCOMPLETE MARKER')
    try { Reject-Unchanged $Second $Destination 'Invalid runtime recovery completion marker' }
    finally { Remove-Item -LiteralPath $marker }
    Reject-Unchanged (Join-Path $Destination 'unins000.exe') $Destination 'Recover the interrupted runtime installation' -Uninstall
}
function Check-Completed([string]$Destination, [bool]$ExpectedExtra) {
    if (Test-Path -LiteralPath (Join-Path $Destination '.nelson-runtime-update')) { throw 'Active recovery journal remains after success.' }
    $manifest = Get-Content -LiteralPath (Join-Path $Destination 'runtime.json') -Raw | ConvertFrom-Json
    if (($manifest.modules -contains 'extra') -ne $ExpectedExtra) { throw 'Runtime module union was not preserved.' }
    foreach ($file in $manifest.files) {
        if ((Get-FileHash -LiteralPath (Join-Path $Destination $file.relative)).Hash.ToLowerInvariant() -cne $file.sha256) { throw 'Installed file differs from inventory.' }
    }
    $uninstallers = @(Get-ChildItem -LiteralPath $Destination -Filter 'unins*.exe' -File)
    if ($uninstallers.Count -ne 1) { throw 'Shared runtime needs one uninstaller.' }
    Require-Success (Run-Setup $uninstallers[0].FullName $Destination -Uninstall)
    foreach ($name in @('data/value-a.txt', 'data/value-b.txt', 'runtime-install.ini', 'runtime.json', 'modules/modules.m')) {
        if (Test-Path -LiteralPath (Join-Path $Destination $name)) { throw "Uninstall left an owned file: $name" }
    }
    if ((Get-Content -LiteralPath (Join-Path $Destination 'user.txt') -Raw) -cne 'PRESERVED') { throw 'Uninstall removed unrelated data.' }
}

function Run-Application([string]$Executable, [string]$Runtime, [string]$Marker) {
    $info = [Diagnostics.ProcessStartInfo]::new($Executable)
    $info.UseShellExecute = $false
    $info.CreateNoWindow = $true
    $info.RedirectStandardOutput = $true
    $info.RedirectStandardError = $true
    $info.WorkingDirectory = $Work
    $info.Environment['NELSONC_RUNTIME_ROOT'] = $Runtime
    $info.Environment['NELSON_RUNTIME_PATH'] = ''
    $info.Environment['PATH'] = Join-Path $env:SystemRoot 'System32'
    $info.Environment['TEMP'] = $Work
    $info.Environment['TMP'] = $Work
    $process = [Diagnostics.Process]::Start($info)
    try {
        $stdout = $process.StandardOutput.ReadToEndAsync()
        $stderr = $process.StandardError.ReadToEndAsync()
        if (-not $process.WaitForExit($installerTimeoutMs)) { throw 'Recovered application timed out.' }
        if ($process.ExitCode -ne 0 -or $stdout.Result -notmatch $Marker) {
            throw "Recovered application failed: $($stdout.Result) $($stderr.Result)"
        }
    } finally {
        if (-not $process.HasExited) { Stop-ProcessTree $process }
        $process.Dispose()
    }
}

function Check-LiveRuntime {
    $target = Join-Path $Work 'live-runtime'
    Require-Success (Run-Setup $First $target)
    $before = Get-Content -LiteralPath (Join-Path $target 'runtime.json') -Raw | ConvertFrom-Json
    $old = $before.files | Where-Object { $_.relative.EndsWith('/libnlsInterpreter.dll') }
    $path = Join-Path $target $old.relative
    $date = (Get-Item -LiteralPath $path).LastWriteTimeUtc
    [IO.File]::WriteAllText((Join-Path $target 'user.txt'), 'PRESERVED')
    Run-Application $AppFirst $target 'RECOVERY_A_OK'
    $null = Run-Setup $Second $target 'ledger'
    Run-Application $AppFirst $target 'RECOVERY_A_OK'
    $null = Run-Setup $Second $target 'restore'
    Run-Application $AppFirst $target 'RECOVERY_A_OK'
    Require-Success (Run-Setup $Second $target)
    Run-Application $AppFirst $target 'RECOVERY_A_OK'
    Run-Application $AppSecond $target 'RECOVERY_B_OK'
    $after = Get-Content -LiteralPath (Join-Path $target 'runtime.json') -Raw | ConvertFrom-Json
    if ($after.modules -notcontains 'trigonometric_functions') { throw 'Recovered runtime did not add the new module.' }
    if ((Get-FileHash -LiteralPath $path).Hash.ToLowerInvariant() -cne $old.sha256 -or
        (Get-Item -LiteralPath $path).LastWriteTimeUtc -ne $date) { throw 'Recovery changed the old interpreter.' }
    foreach ($file in $after.files) {
        if ((Get-FileHash -LiteralPath (Join-Path $target $file.relative)).Hash.ToLowerInvariant() -cne $file.sha256) { throw 'Recovered runtime inventory mismatch.' }
    }
    Check-Completed $target $false
    foreach ($file in $after.files) {
        if (Test-Path -LiteralPath (Join-Path $target $file.relative)) { throw 'Uninstall left recovered runtime payload.' }
    }
    Write-Output 'RUNTIME_APPLICATION_RECOVERY_OK'
}

if ($AppFirst -and $AppSecond) { Check-LiveRuntime; exit 0 }

$lockedTarget = Join-Path $Work 'reader-lock'
Require-Success (Run-Setup $Second $lockedTarget 'activation-lock')
[IO.File]::WriteAllText((Join-Path $lockedTarget 'user.txt'), 'PRESERVED')
Check-Completed $lockedTarget $true
Write-Output 'RECOVERY_LOCK_RETRY_OK'
$lockedTarget = Join-Path $Work 'persistent-reader-lock'
$failure = Run-Setup $Second $lockedTarget 'activation-lock' -KeepLock
if ($failure.Code -eq 0 -or (Get-Content -LiteralPath $failure.Log -Raw) -notmatch 'Cannot activate runtime recovery journal') {
    throw 'Persistent reader lock did not refuse journal activation.'
}
foreach ($file in @('runtime-install.ini', 'runtime.json', 'data/value-a.txt')) {
    if (Test-Path -LiteralPath (Join-Path $lockedTarget $file)) { throw 'Failed activation installed runtime files.' }
}
Require-Success (Run-Setup $Second $lockedTarget)
[IO.File]::WriteAllText((Join-Path $lockedTarget 'user.txt'), 'PRESERVED')
Check-Completed $lockedTarget $true
Write-Output 'RECOVERY_LOCK_FAILURE_OK'
$failedTarget = Join-Path $Work 'finalization-failure'
$failure = Run-Setup $Second $failedTarget 'finalization-failure'
if ($failure.Code -eq 0 -or (Get-Content -LiteralPath $failure.Log -Raw) -notmatch 'Runtime installation finalization is incomplete') {
    throw 'Incomplete finalization incorrectly reported success.'
}
Require-Success (Run-Setup $Second $failedTarget)
[IO.File]::WriteAllText((Join-Path $failedTarget 'user.txt'), 'PRESERVED')
Check-Completed $failedTarget $true
Write-Output 'RECOVERY_FINALIZATION_FAILURE_OK'

foreach ($phase in @('payload', 'ledger', 'manifest', 'startup', 'uninstall', 'completion', 'commit', 'retired', 'cleanup')) {
    $target = Join-Path $Work ("runtime ' " + [char]233 + [char]28450 + [char]23383 + " $phase")
    if ($phase -eq 'ledger') { $target = Join-Path $target ('long-' + ('x' * (220 - $target.Length - 6))) }
    Require-Success (Run-Setup $First $target)
    $old = Get-FileHash -LiteralPath (Join-Path $target 'data/value-a.txt')
    $date = (Get-Item -LiteralPath $old.Path).LastWriteTimeUtc
    [IO.File]::WriteAllText((Join-Path $target 'user.txt'), 'PRESERVED')
    $null = Run-Setup $Second $target $phase
    $retired = $phase -in @('retired', 'cleanup')
    if ((Test-Path -LiteralPath (Join-Path $target '.nelson-runtime-update/journal.ini')) -eq $retired) { throw 'Unexpected interrupted journal state.' }
    if ($phase -notin @('commit', 'retired', 'cleanup')) { Reject-Unchanged $First $target 'same installer' }
    if ($phase -eq 'startup') {
        $data = Join-Path $target 'data/value-b.txt'
        [IO.File]::WriteAllText($data, 'USER EDIT')
        Reject-Unchanged $Second $target 'Installed runtime file changed'
        [IO.File]::WriteAllText($data, 'second')
        $backup = Join-Path $target '.nelson-runtime-update/previous/runtime-install.ini'
        $bytes = [IO.File]::ReadAllBytes($backup)
        [IO.File]::WriteAllText($backup, 'DAMAGED')
        Reject-Unchanged $Second $target 'Changed or missing runtime recovery backup'
        [IO.File]::WriteAllBytes($backup, $bytes)
        Check-Refusals $target
        $null = Run-Setup $Second $target 'metadata'
        $null = Run-Setup $Second $target 'restore'
        $null = Run-Setup $Second $target 'restore'
    }
    if ($phase -eq 'manifest') { Remove-Item -LiteralPath (Join-Path $target 'runtime.json') }
    if ($phase -in @('commit', 'retired', 'cleanup')) { Require-Success (Run-Setup $First $target) }
    else { Require-Success (Run-Setup $Second $target) }
    if ((Get-FileHash -LiteralPath $old.Path).Hash -cne $old.Hash -or (Get-Item -LiteralPath $old.Path).LastWriteTimeUtc -ne $date) { throw 'Existing runtime bytes or timestamp changed.' }
    Check-Completed $target $true
    Write-Output "UPDATE_RECOVERY_OK=$phase"
}
foreach ($phase in @('payload', 'ledger', 'uninstall', 'completion')) {
    $target = Join-Path $Work "initial-$phase"
    $preparation = Join-Path $target '.nelson-runtime-prepare-0'
    $null = New-Item -ItemType Directory -Path $preparation -Force
    [IO.File]::WriteAllText((Join-Path $preparation 'unclaimed.txt'), 'PRESERVED')
    $null = Run-Setup $Second $target $phase
    [IO.File]::WriteAllText((Join-Path $target 'user.txt'), 'PRESERVED')
    Require-Success (Run-Setup $Second $target)
    Check-Completed $target $true
    if ((Get-Content -LiteralPath (Join-Path $preparation 'unclaimed.txt') -Raw) -cne 'PRESERVED') { throw 'Interrupted preparation was not preserved.' }
    Write-Output "INITIAL_RECOVERY_OK=$phase"
}
Write-Output 'RUNTIME_PROCESS_RECOVERY_OK'
