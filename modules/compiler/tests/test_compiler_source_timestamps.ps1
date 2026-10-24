# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param([Parameter(Mandatory)][string]$Directory)
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$allowed = [IO.Path]::GetFullPath((Join-Path $repo 'temp')) + [IO.Path]::DirectorySeparatorChar
$target = (Resolve-Path -LiteralPath $Directory).Path
if (-not $target.StartsWith($allowed, [StringComparison]::OrdinalIgnoreCase)) {
    throw 'Source timestamp checks must stay under repository temp.'
}
foreach ($file in Get-ChildItem -LiteralPath $target -File) {
    if ($file.Attributes -band [IO.FileAttributes]::ReparsePoint) { throw 'Ordinary source files required.' }
    $before = (Get-FileHash -LiteralPath $file.FullName).Hash
    $file.LastWriteTimeUtc = [datetime]::new(2001, 1, 1, 0, 0, 0, [DateTimeKind]::Utc)
    if ((Get-FileHash -LiteralPath $file.FullName).Hash -cne $before) { throw 'Source contents changed.' }
}
'SOURCE_TIMESTAMPS_OK'
