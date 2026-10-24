# Dot-sourced by test_nelsonc_native.ps1 with the shared integration helpers.
$mexSource = Join-Path $root 'mex-source'
$mexPackage = Join-Path $mexSource '+nativepkg'
New-Item -ItemType Directory -Path $mexPackage | Out-Null
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_native/native_mex_entry.m') -Destination $mexSource
foreach ($name in @('nc_native_mex.nexw64', 'nc_native_bridge.dll', 'nc_native_leaf.dll')) {
    Copy-Item -LiteralPath (Join-Path $NativeLibraries $name) -Destination $mexPackage
}
$mexEntry = Join-Path $mexSource 'native_mex_entry.m'
$mexDistribution = Join-Path $root 'mex-distribution'
$mexExplain = Invoke-Application $compiler @($mexEntry, '--runtime', 'installed', '--explain-link')
if ($mexExplain.ExitCode -ne 0) { throw ($mexExplain.Output + $mexExplain.Error) }
$mexPlan = $mexExplain.Output | ConvertFrom-Json
if (-not $mexPlan.complete -or $mexPlan.modules.name -notcontains 'mex') { throw 'Native package function not resolved.' }
if (-not ($mexPlan.nativeRuntimeLibraries.filename -contains 'libnlsMex.dll')) { throw 'Native runtime imports not retained.' }
foreach ($mode in @('bundled', 'installed')) {
    $executable = Join-Path $mexDistribution ($mode + '.exe')
    $build = Invoke-Application $compiler @($mexEntry, '-o', $executable, '--runtime', $mode)
    if ($build.ExitCode -ne 0) { throw ($build.Output + $build.Error) }
    $archive = Read-EmbeddedApplication $executable
    foreach ($name in @('nc_native_mex.nexw64', 'nc_native_bridge.dll', 'nc_native_leaf.dll')) {
        if (@($archive.Names | Where-Object { $_ -eq "roots/1/+nativepkg/$name" }).Count -ne 1) { throw "Native package import not embedded once: $name" }
    }
    if (@($archive.Names | Where-Object { [IO.Path]::GetFileName($_) -like 'libnls*.dll' }).Count) { throw 'Runtime imports duplicated in payload.' }
}
$mexRelocated = [IO.Path]::GetFullPath((Join-Path $root 'relocated mex'))
foreach ($checked in @($mexSource, $mexDistribution, $mexRelocated)) {
    if (-not [IO.Path]::GetFullPath($checked).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid native function relocation path.' }
}
Remove-Item -LiteralPath $mexSource -Recurse -Force
Move-Item -LiteralPath $mexDistribution -Destination $mexRelocated
$before = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
foreach ($mode in @('bundled', 'installed')) {
    $environment = @{ PATH = '' }
    if ($mode -eq 'installed') { $environment.NELSONC_RUNTIME_ROOT = $selectedInstalled }
    $run = Invoke-Application (Join-Path $mexRelocated ($mode + '.exe')) @() 30000 '' $environment -CaptureModules
    if ($run.ExitCode -ne 0 -or $run.Output -notmatch 'MEX_VALUE=17') { throw ($run.Output + $run.Error) }
    foreach ($name in @('nc_native_mex.nexw64', 'nc_native_bridge.dll', 'nc_native_leaf.dll')) {
        $loaded = @($run.Modules | Where-Object { [IO.Path]::GetFileName($_) -eq $name })
        if ($loaded.Count -ne 1 -or -not $loaded[0].StartsWith($root + '\', [StringComparison]::OrdinalIgnoreCase)) {
            throw "Native function library was not observed in the extraction: $name; observed=$($loaded -join ', ')"
        }
    }
}
$after = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
if (Compare-Object $before $after) { throw 'Native function application left an extraction directory.' }
$null = Read-CheckedRuntime (Join-Path $mexRelocated 'bundled.runtime')
Write-Output 'PASS native package function, transitive imports, shared runtime imports and relocated bundled/installed execution'
Write-Output "MEX_ARTIFACT_ROOT=$mexRelocated"
