#Requires -Version 7.0
# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param([string]$CMake = 'cmake')
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$root = Join-Path $repo ('temp/compiler-runtime-layout-' + [Guid]::NewGuid().ToString('N'))
$source = Join-Path $PSScriptRoot 'fixtures/runtime_layout'
New-Item -ItemType Directory -Path $root | Out-Null
"RUNTIME_LAYOUT_TEST=$root"
$ctest = Join-Path (Split-Path (Get-Command $CMake).Source -Parent) 'ctest'
foreach ($configuration in @('standard', 'custom', 'absolute-contained')) {
    $build = Join-Path $root $configuration
    $options = if ($configuration -eq 'custom') {
        @('-DCMAKE_INSTALL_DATADIR=assets/../assets/data/', '-DCMAKE_INSTALL_BINDIR=tools/./native/bin/')
    } elseif ($configuration -eq 'absolute-contained') {
        $prefix = (Join-Path $root 'install-reference').Replace('\', '/')
        @("-DCMAKE_INSTALL_PREFIX=$prefix", "-DCMAKE_INSTALL_DATADIR=$prefix/share", "-DCMAKE_INSTALL_BINDIR=$prefix/bin")
    } else { @() }
    & $CMake -S $source -B $build "-DTEST_ROOT=$root/cases/$configuration" @options *> (Join-Path $root "$configuration-configure.log")
    if ($LASTEXITCODE -ne 0) { throw 'Runtime locator test configuration failed.' }
    & $CMake --build $build --config Release *> (Join-Path $root "$configuration-build.log")
    if ($LASTEXITCODE -ne 0) { throw 'Runtime locator test build failed.' }
    & $ctest --test-dir $build -C Release --output-on-failure *> (Join-Path $root "$configuration-test.log")
    if ($LASTEXITCODE -ne 0) { throw "Runtime locator regression: $root/$configuration-test.log" }
}
foreach ($invalid in @('absolute', 'parent')) {
    $value = if ($invalid -eq 'absolute') { Join-Path $root 'absolute-bin' } else { '../outside' }
    & $CMake -S $source -B (Join-Path $root $invalid) "-DCMAKE_INSTALL_BINDIR=$value" *> (Join-Path $root "$invalid-configure.log")
    if ($LASTEXITCODE -eq 0 -or (Get-Content (Join-Path $root "$invalid-configure.log") -Raw) -notlike '*compiler install directories must stay within the installation prefix*') {
        throw 'Nonrelocatable compiler install directory was not rejected.'
    }
}
'PASS runtime layout native tests: 6 cases, real launcher builds and invalid install directories'
"RUNTIME_LAYOUT_TEST=$root"
exit 0
