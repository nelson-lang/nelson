#Requires -Version 7.0
# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param([string]$CMake = 'cmake')
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$root = Join-Path $repo ('temp/compiler-runtime-library-search-' + [Guid]::NewGuid().ToString('N'))
$source = Join-Path $PSScriptRoot 'fixtures/runtime_library_search'
New-Item -ItemType Directory -Path $root | Out-Null
"RUNTIME_LIBRARY_SEARCH_TEST=$root"
$ctest = Join-Path (Split-Path (Get-Command $CMake).Source -Parent) 'ctest'
& $CMake -S $source -B (Join-Path $root 'build') *> (Join-Path $root 'configure.log')
if ($LASTEXITCODE -ne 0) { throw "Runtime library search configuration failed: $root/configure.log" }
& $CMake --build (Join-Path $root 'build') --config Release *> (Join-Path $root 'build.log')
if ($LASTEXITCODE -ne 0) { throw "Runtime library search build failed: $root/build.log" }
& $ctest --test-dir (Join-Path $root 'build') -C Release --output-on-failure *> (Join-Path $root 'test.log')
$result = $LASTEXITCODE
Get-Content -LiteralPath (Join-Path $root 'test.log')
if ($result -ne 0) { throw 'Runtime library search regression.' }
if ($IsLinux) {
    'PASS portable path/argument tests and Linux native bootstrap tests'
} else {
    'PASS portable path/argument tests; Linux native bootstrap tests require Linux'
}
"RUNTIME_LIBRARY_SEARCH_TEST=$root"
exit 0
