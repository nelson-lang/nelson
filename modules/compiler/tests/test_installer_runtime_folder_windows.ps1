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
$Work = (Resolve-Path -LiteralPath $Work).Path
if (-not $Work.StartsWith([IO.Path]::GetTempPath(), [StringComparison]::OrdinalIgnoreCase)) {
    throw 'The fixture must stay inside temporary directory.'
}
$key = "HKCU:\Software\Nelson\Runtime\$Architecture\$Fingerprint"
if (Test-Path -LiteralPath $key) { throw 'An existing user runtime registration must not be modified by this test.' }
$first = Join-Path $Work 'package-a/first.exe'
$second = Join-Path $Work 'package-b/second.exe'
$absent = Join-Path $Work 'package-none/absent.exe'
$runtimeParent = Join-Path $Work 'shared runtime'
$runtime = Join-Path $runtimeParent $Fingerprint
$appA = Join-Path $Work 'app-a'
$appB = Join-Path $Work 'app-b'
$private = Join-Path $Work 'private application'
$privateRuntime = Join-Path $private 'shared_a.runtime'
$refused = Join-Path $Work 'refused'
$sequence = 0

function Quote-Crt([string]$Value) {
    if ($Value.Contains('"')) { throw 'Unsupported test quote.' }
    return '"' + [regex]::Replace($Value, '(\\+)$', '$1$1') + '"'
}
function Invoke-Setup([string]$Executable, [string]$Parameters, [bool]$Success = $true, [bool]$Deployment = $true) {
    $script:sequence++
    $log = Join-Path $Work ("operation-$sequence.log")
    if ($Deployment) { $Parameters += ' -outputFile ' + (Quote-Crt $log) }
    else { $Parameters += ' /LOG="' + $log + '"' }
    $env:TEMP = $Work; $env:TMP = $Work
    $process = Start-Process -FilePath $Executable -ArgumentList $Parameters -WindowStyle Hidden -PassThru -Wait
    try {
        if (($process.ExitCode -eq 0) -ne $Success) {
            if (Test-Path -LiteralPath $log) { Get-Content -LiteralPath $log -Tail 30 | Write-Host }
            throw "Unexpected installer exit $($process.ExitCode): $log"
        }
        return $log
    } finally { $process.Dispose() }
}
function Install-App([string]$Executable, [string]$Target, [string]$RuntimeBase, [bool]$Success = $true) {
    $parameters = '-agreeToLicense yes /CURRENTUSER -applicationFolder ' + (Quote-Crt $Target)
    if ($RuntimeBase) { $parameters += ' -runtimeFolder ' + (Quote-Crt $RuntimeBase) }
    return Invoke-Setup $Executable $parameters $Success
}
function Remove-Installed([string]$Root, [bool]$Success = $true) {
    $Root = [IO.Path]::GetFullPath($Root)
    if (-not $Root.StartsWith($Work + '\', [StringComparison]::OrdinalIgnoreCase)) { throw 'Unsafe fixture removal.' }
    if (-not (Test-Path -LiteralPath $Root)) { return }
    $files = @(Get-ChildItem -LiteralPath $Root -Filter 'unins*.exe' -File)
    if ($files.Count -eq 0) { return }
    if ($files.Count -ne 1) { throw 'Ambiguous fixture uninstaller.' }
    $null = Invoke-Setup $files[0].FullName '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART' $Success $false
}
function Assert-Application([string]$Root, [string]$Name, [string]$ExpectedRuntime, [bool]$Success = $true) {
    $record = Join-Path $Work ([Guid]::NewGuid().ToString('N') + '.txt')
    $info = [Diagnostics.ProcessStartInfo]::new((Join-Path $Root ($Name + '.exe')))
    $info.UseShellExecute = $false
    $info.CreateNoWindow = $true
    $info.WindowStyle = [Diagnostics.ProcessWindowStyle]::Hidden
    $info.WorkingDirectory = $Work
    foreach ($name in @($info.Environment.Keys)) {
        if ($name.StartsWith('NELSON')) { $null = $info.Environment.Remove($name) }
    }
    $info.Environment['TEMP'] = $Work
    $info.Environment['TMP'] = $Work
    $info.Environment['PATH'] = Join-Path $env:SystemRoot 'System32'
    $info.Environment['LOCALAPPDATA'] = Join-Path $Work 'local-data'
    $info.Arguments = Join-Arguments @($record)
    $process = [Diagnostics.Process]::Start($info)
    try {
        if (-not $process.WaitForExit(60000)) { Stop-ProcessTree $process; throw 'Application timed out.' }
        if (-not $Success) {
            if ($process.ExitCode -ne 2 -or (Test-Path -LiteralPath $record)) { throw 'A missing runtime was not rejected.' }
            return
        }
        if ($process.ExitCode -ne 0 -or -not (Test-Path -LiteralPath $record)) { throw 'Installed application failed.' }
        $actual = [IO.Path]::GetFullPath([IO.File]::ReadAllText($record).Trim()).TrimEnd('\')
        if ($actual -ne [IO.Path]::GetFullPath($ExpectedRuntime).TrimEnd('\')) { throw "Unexpected runtime: $actual" }
    } finally { $process.Dispose() }
}
function Assert-Absent([string]$Path) {
    if (Test-Path -LiteralPath $Path) { throw "Unexpected installation output: $Path" }
}
try {
    $null = Install-App $first $appA ($runtimeParent + '\')
    if ((Get-ItemPropertyValue -LiteralPath $key -Name Root) -ne $runtime) { throw 'Shared runtime registration is missing.' }
    Assert-Absent (Join-Path $appA 'shared_a.runtime')
    Assert-Application $appA 'shared_a' $runtime
    $engine = Join-Path $runtime 'bin/x64/libnlsEngine.dll'
    if ($Architecture -eq 'woa64') { $engine = Join-Path $runtime 'bin/arm64/libnlsEngine.dll' }
    $digest = (Get-FileHash -LiteralPath $engine).Hash
    $timestamp = (Get-Item -LiteralPath $engine).LastWriteTimeUtc
    $control = Join-Path $Work 'runtime-control.txt'
    [IO.File]::WriteAllText($control, "agreeToLicense=yes`r`napplicationFolder=$appB`r`nruntimeFolder=$runtimeParent`r`n", [Text.UTF8Encoding]::new($true))
    $null = Invoke-Setup $second ('-inputFile ' + (Quote-Crt $control) + ' /CURRENTUSER')
    $manifest = Get-Content -LiteralPath (Join-Path $runtime 'runtime.json') -Raw | ConvertFrom-Json
    if ('trigonometric_functions' -notin $manifest.modules) { throw 'The shared runtime did not merge dependencies.' }
    if ((Get-FileHash -LiteralPath $engine).Hash -ne $digest -or (Get-Item -LiteralPath $engine).LastWriteTimeUtc -ne $timestamp) {
        throw 'The union replaced an unchanged runtime file.'
    }
    Assert-Application $appA 'shared_a' $runtime
    Assert-Application $appB 'shared_b' $runtime
    Remove-Installed $runtime
    Assert-Absent $key
    Assert-Application $appB 'shared_b' '' $false
    $null = Install-App $second $appB $runtimeParent
    Assert-Application $appB 'shared_b' $runtime
    $null = Install-App $first $appA $runtimeParent
    $null = Install-App $first $refused (Join-Path $Work 'conflicting-runtime') $false
    Assert-Absent $refused
    $null = Install-App $absent $refused $runtimeParent $false
    Assert-Absent $refused
    $ownedFile = Join-Path $runtime 'LICENSE'
    $originalFile = [IO.File]::ReadAllBytes($ownedFile)
    try {
        [IO.File]::WriteAllText($ownedFile, 'CHANGED')
        $null = Install-App $first $refused $runtimeParent $false
        Assert-Absent $refused
        if ([IO.File]::ReadAllText($ownedFile) -ne 'CHANGED') { throw 'An edited runtime file was overwritten.' }
    } finally { [IO.File]::WriteAllBytes($ownedFile, $originalFile) }
    $link = Join-Path $Work 'runtime-link'
    try {
        $null = New-Item -ItemType Junction -Path $link -Target $runtimeParent
        $null = Install-App $first $refused $link $false
        Assert-Absent $refused
    } finally {
        # Windows PowerShell prompts before removing a junction; delete only the reparse point.
        if (Test-Path -LiteralPath $link) { [IO.Directory]::Delete($link) }
    }
    foreach ($parameters in @(
        ('-destinationFolder ' + (Quote-Crt $refused) + ' -runtimeFolder ' + (Quote-Crt $runtimeParent)),
        ('-runtimeFolder ' + (Quote-Crt $runtimeParent) + ' -destinationFolder ' + (Quote-Crt $refused)),
        ('-applicationFolder ' + (Quote-Crt $refused) + ' -runtimeFolder relative'),
        ('-applicationFolder ' + (Quote-Crt $refused) + ' -runtimeFolder ' + (Quote-Crt $runtimeParent) + ' /PORTABLE=1')
    )) {
        $null = Invoke-Setup $first ('-agreeToLicense yes /CURRENTUSER ' + $parameters) $false
        Assert-Absent $refused
    }
    $null = Install-App $first $refused $refused $false
    Assert-Absent $refused
    $null = Install-App $first (Join-Path $runtime 'nested-app') $runtimeParent $false
    Assert-Absent (Join-Path $runtime 'nested-app')
    $null = Install-App $first $private ''
    Assert-Application $private 'shared_a' $privateRuntime
    $null = Install-App $first $private ''
    Assert-Application $private 'shared_a' $privateRuntime
    $null = Install-App $first $private $runtimeParent $false
    $null = Install-App $first $appA '' $false
    $journal = Join-Path $privateRuntime '.nelson-runtime-update'
    try {
        $null = New-Item -ItemType Directory -Path $journal
        Remove-Installed $private $false
        if (-not (Test-Path -LiteralPath (Join-Path $private 'shared_a.exe'))) { throw 'An interrupted runtime allowed application removal.' }
    } finally { Remove-Item -LiteralPath $journal }
    try {
        Set-ItemProperty -LiteralPath $key -Name Root -Value $privateRuntime
        Remove-Installed $private $false
    } finally { Set-ItemProperty -LiteralPath $key -Name Root -Value $runtime }
    $metadata = Join-Path $private '.nelson-install/application.ini'
    $original = [IO.File]::ReadAllBytes($metadata)
    try {
        [IO.File]::WriteAllText($metadata, '[Installation]')
        Remove-Installed $private $false
        if (-not (Test-Path -LiteralPath (Join-Path $private 'shared_a.exe'))) { throw 'Damaged metadata allowed application removal.' }
        Assert-Application $private 'shared_a' $privateRuntime
        Remove-Item -LiteralPath $metadata
        Remove-Installed $private $false
        if (-not (Test-Path -LiteralPath (Join-Path $private 'shared_a.exe'))) { throw 'Missing metadata allowed application removal.' }
    } finally { [IO.File]::WriteAllBytes($metadata, $original) }
    Remove-Installed $private
    Assert-Absent $privateRuntime
    $null = Install-App $first $private ''
    $userFile = Join-Path $privateRuntime 'user-data.txt'
    [IO.File]::WriteAllText($userFile, 'KEEP')
    Remove-Installed $private
    if ([IO.File]::ReadAllText($userFile) -ne 'KEEP') { throw 'Private runtime user data was removed.' }
    Assert-Absent (Join-Path $privateRuntime 'runtime.json')
    Assert-Application $appB 'shared_b' $runtime
    Remove-Installed $appA
    Assert-Application $appB 'shared_b' $runtime
    Remove-Installed $appB
    if (-not (Test-Path -LiteralPath $engine)) { throw 'Removing applications removed the shared runtime.' }
    Remove-Installed $runtime
    Assert-Absent $key
    Assert-Absent $runtime
    Write-Output 'WINDOWS_RUNTIME_FOLDER_OK'
} finally {
    foreach ($path in @($private, $appB, $appA, $runtime)) { Remove-Installed $path }
}
