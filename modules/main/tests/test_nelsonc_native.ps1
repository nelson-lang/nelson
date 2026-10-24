# Dot-sourced by test_nelsonc.ps1; reuses its isolated process and archive helpers.
$nativeSource = Join-Path $root 'native-source'
$nativeDirectory = Join-Path $nativeSource 'native'
New-Item -ItemType Directory -Path $nativeDirectory | Out-Null
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_native/native_entry.m') -Destination $nativeSource
foreach ($name in @('nc_native_bridge.dll', 'nc_native_leaf.dll')) {
    Copy-Item -LiteralPath (Join-Path $NativeLibraries $name) -Destination $nativeDirectory
}
$nativeEntry = Join-Path $nativeSource 'native_entry.m'
$bridge = Join-Path $nativeDirectory 'nc_native_bridge.dll'
$leaf = Join-Path $nativeDirectory 'nc_native_leaf.dll'
$explain = Invoke-Application $compiler @($nativeEntry, '-a', $bridge, '--runtime', 'installed', '--explain-link')
if ($explain.ExitCode -ne 0) { throw ($explain.Output + $explain.Error) }
$nativePlan = $explain.Output | ConvertFrom-Json
foreach ($name in @('nc_native_bridge.dll', 'nc_native_leaf.dll')) {
    $matches = @($nativePlan.files | Where-Object { [IO.Path]::GetFileName($_.source) -eq $name })
    if ($matches.Count -ne 1 -or $matches[0].representation -ne 'native') { throw "Missing or duplicated native input: $name" }
}
$savedLeaf = [IO.Path]::GetFullPath((Join-Path $root 'saved-leaf.dll'))
foreach ($checked in @($leaf, $savedLeaf)) {
    if (-not [IO.Path]::GetFullPath($checked).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid native test path.' }
}
Move-Item -LiteralPath $leaf -Destination $savedLeaf
try {
    $missing = Invoke-Application $compiler @($nativeEntry, '-a', $bridge, '--runtime', 'installed', '--explain-link')
    if ($missing.ExitCode -eq 0 -or ($missing.Output + $missing.Error) -notmatch 'Missing native dependency nc_native_leaf.dll') {
        throw 'Missing transitive native dependency was not rejected.'
    }
} finally {
    Move-Item -LiteralPath $savedLeaf -Destination $leaf
}
$nativeDistribution = Join-Path $root 'native-distribution'
foreach ($mode in @('bundled', 'installed')) {
    $executable = Join-Path $nativeDistribution ($mode + '.exe')
    $build = Invoke-Application $compiler @($nativeEntry, '-a', $bridge, '-o', $executable, '--runtime', $mode)
    if ($build.ExitCode -ne 0) { throw ($build.Output + $build.Error) }
    $archive = Read-EmbeddedApplication $executable
    foreach ($name in @('nc_native_bridge.dll', 'nc_native_leaf.dll')) {
        if (@($archive.Names | Where-Object { $_ -eq "roots/1/native/$name" }).Count -ne 1) { throw "Native library not embedded exactly once: $name" }
    }
    if ($archive.Manifest.nativePaths.Count -ne 1 -or $archive.Manifest.nativePaths[0] -ne 'roots/1/native') {
        throw 'Incorrect native search directory manifest.'
    }
}
if (Test-Path -LiteralPath (Join-Path $nativeDistribution 'installed.runtime')) { throw 'Installed mode copied runtime.' }
$nativeRelocated = [IO.Path]::GetFullPath((Join-Path $root 'relocated native'))
foreach ($checked in @($nativeSource, $nativeDistribution, $nativeRelocated)) {
    if (-not [IO.Path]::GetFullPath($checked).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid native relocation path.' }
}
Remove-Item -LiteralPath $nativeSource -Recurse -Force
Move-Item -LiteralPath $nativeDistribution -Destination $nativeRelocated
$before = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
$nativeRuntime = Join-Path $nativeRelocated 'bundled.runtime'
$selectedInstalled = [IO.Path]::GetFullPath((Join-Path $runtimePath '../..'))
foreach ($mode in @('bundled', 'installed')) {
    $environment = @{ PATH = '' }
    if ($mode -eq 'installed') { $environment.NELSONC_RUNTIME_ROOT = $selectedInstalled }
    $run = Invoke-Application (Join-Path $nativeRelocated ($mode + '.exe')) @() 30000 '' $environment -CaptureModules
    if ($run.ExitCode -ne 0 -or $run.Output -notmatch 'NATIVE_VALUE=17') { throw ($run.Output + $run.Error) }
    foreach ($name in @('nc_native_bridge.dll', 'nc_native_leaf.dll')) {
        $loaded = @($run.Modules | Where-Object { [IO.Path]::GetFileName($_) -eq $name })
        if ($loaded.Count -ne 1 -or -not $loaded[0].StartsWith($root + '\', [StringComparison]::OrdinalIgnoreCase)) {
            throw "Native library was not observed in the private extraction: $name; observed=$($loaded -join ', ')"
        }
    }
}
$after = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
if (Compare-Object $before $after) { throw 'Native application left an extraction directory.' }
$null = Read-CheckedRuntime $nativeRuntime
Write-Output 'PASS native DLL closure, missing dependency rejection, embedded imports, relocated bundled and installed execution'
Write-Output "NATIVE_ARTIFACT_ROOT=$nativeRelocated"
if (Test-Path -LiteralPath (Join-Path $NativeLibraries 'nc_native_mex.nexw64')) {
    . (Join-Path $PSScriptRoot 'test_nelsonc_native_mex.ps1')
}
