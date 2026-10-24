# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param([Parameter(Mandatory)][string]$LockedFile,
    [Parameter(Mandatory)][string]$Script, [Parameter(Mandatory)][string]$Launcher)
$ErrorActionPreference = 'Stop'
$stream = [IO.File]::Open($LockedFile, [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::Read)
try {
    & $Launcher --no-ipc --no-user-modules --no-user-startup --quiet --execute "run('$($Script.Replace("'", "''"))');"
    if ($LASTEXITCODE -ne 0) { throw "Locked-publication probe failed: $LASTEXITCODE" }
} finally { $stream.Dispose() }
