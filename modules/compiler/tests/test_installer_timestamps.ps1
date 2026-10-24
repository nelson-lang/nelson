# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param(
    [Parameter(Mandatory)][ValidateSet('set', 'snapshot', 'check')][string]$Mode,
    [Parameter(Mandatory)][string]$Target,
    [AllowEmptyString()][string]$Timestamp = ''
)
$ErrorActionPreference = 'Stop'
$item = Get-Item -LiteralPath $Target
if ($Mode -ne 'snapshot') {
    $utc = [DateTime]::Parse($Timestamp, [Globalization.CultureInfo]::InvariantCulture,
        [Globalization.DateTimeStyles]::RoundtripKind).ToUniversalTime()
}
if ($Mode -eq 'check') {
    if ($item.PSIsContainer -or $item.LastWriteTimeUtc -ne $utc) {
        throw 'Installed file timestamp was not normalized to the expected UTC value.'
    }
    exit 0
}
if (-not $item.PSIsContainer) { throw 'Expected a fixture directory.' }
$files = @(Get-ChildItem -LiteralPath $item.FullName -File -Recurse | Sort-Object FullName)
if ($Mode -eq 'set') {
    foreach ($file in $files) { $file.LastWriteTimeUtc = $utc }
} else {
    @(foreach ($file in $files) {
        $hash = [Security.Cryptography.SHA256]::Create()
        $stream = [IO.File]::OpenRead($file.FullName)
        try { $digest = [BitConverter]::ToString($hash.ComputeHash($stream)).Replace('-', '') }
        finally { $stream.Dispose(); $hash.Dispose() }
        [pscustomobject]@{ Path = $file.FullName; Bytes = $file.Length;
            Modified = $file.LastWriteTimeUtc.ToString('O'); SHA256 = $digest }
    }) | ConvertTo-Json -Compress
}
