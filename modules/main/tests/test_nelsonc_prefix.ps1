# Invoked by test_nelsonc.ps1 with its isolated process and inventory helpers.
$prefix = Join-Path $root 'installed prefix'
$prefixData = [IO.Path]::GetFullPath((Join-Path $prefix $PrefixDataDirectory))
$prefixBin = [IO.Path]::GetFullPath((Join-Path $prefix $PrefixBinaryDirectory))
foreach ($path in @($prefixData, $prefixBin)) {
    if (-not $path.StartsWith($prefix + [IO.Path]::DirectorySeparatorChar)) {
        throw 'Invalid prefix test layout.'
    }
}
New-Item -ItemType Directory -Path $prefixData, $prefixBin | Out-Null
$sourceRuntime = Split-Path -Parent (Split-Path -Parent $runtimePath)
# Runtime inputs are shared read-only for this layout test; generated applications
# and their runtime inventories must contain only independent regular files.
foreach ($directory in @('modules', 'etc', 'resources', 'locale', 'tools')) {
    $target = Join-Path $sourceRuntime $directory
    if (Test-Path -LiteralPath $target -PathType Container) {
        New-Item -ItemType Junction -Path (Join-Path $prefixData $directory) -Target $target | Out-Null
    }
}
foreach ($file in Get-ChildItem -LiteralPath $runtimePath -File) {
    if ($file.Extension -in @('.dll', '.txt', '.json', '.conf', '.dat')) {
        New-Item -ItemType HardLink -Path (Join-Path $prefixBin $file.Name) -Target $file.FullName | Out-Null
    }
}
foreach ($notice in @('gpl-3.0.md', 'lgpl-3.0.md', 'THIRDPARTY.md', 'TRADEMARKS.md')) {
    Copy-Item -LiteralPath (Join-Path $sourceRuntime $notice) -Destination $prefixData
}
$launcherSource = if ($PrefixLaunchers) { (Resolve-Path -LiteralPath $PrefixLaunchers).Path } else { $runtimePath }
foreach ($name in @('nelsonc.exe', 'nelson-app.exe', 'nelson-app-windowed.exe')) {
    Copy-Item -LiteralPath (Join-Path $launcherSource $name) -Destination $prefixBin
}
$prefixCompiler = Join-Path $prefixBin 'nelsonc.exe'
$help = Invoke-Application $prefixCompiler @('--cli', '--help')
if ($help.ExitCode -ne 0 -or $help.Output -notmatch 'usage: nelsonc') {
    throw ('Installed-prefix compiler startup failed: ' + $help.Output + $help.Error)
}
$prefixSource = Join-Path $root 'prefix-source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_api') -Destination $prefixSource -Recurse
$prefixEntry = Join-Path $prefixSource 'api_entry.m'
$prefixOutput = Join-Path $root 'prefix-output'
$prefixBundled = Join-Path $prefixOutput 'bundled.exe'
$prefixInstalled = Join-Path $prefixOutput 'installed.exe'
$prefixWindowed = Join-Path $prefixOutput 'windowed.exe'
$windowedStub = [IO.Path]::GetFullPath((Join-Path $prefixBin 'nelson-app-windowed.exe'))
$savedWindowedStub = [IO.Path]::GetFullPath((Join-Path $root 'saved-windowed-launcher.exe'))
foreach ($checked in @($windowedStub, $savedWindowedStub)) {
    if (-not $checked.StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid launcher test path.' }
}
Move-Item -LiteralPath $windowedStub -Destination $savedWindowedStub
try {
    $missing = Invoke-Application $prefixCompiler @('--cli', $prefixEntry, '-o', $prefixWindowed, '--runtime', 'installed', '--no-console')
    if ($missing.ExitCode -eq 0 -or ($missing.Output + $missing.Error) -notmatch 'precompiled application launcher is missing' -or (Test-Path -LiteralPath $prefixWindowed)) {
        throw 'Missing windowed launcher was not rejected before assembly.'
    }
} finally {
    Move-Item -LiteralPath $savedWindowedStub -Destination $windowedStub
}
$result = Invoke-Application $prefixCompiler @('--cli', $prefixEntry, '-o', $prefixWindowed, '--runtime', 'installed', '--no-console')
if ($result.ExitCode -ne 0) { throw ('Prefix windowed build failed: ' + $result.Output + $result.Error) }
foreach ($mode in @('bundled', 'installed')) {
    $output = if ($mode -eq 'bundled') { $prefixBundled } else { $prefixInstalled }
    $result = Invoke-Application $prefixCompiler @('--cli', $prefixEntry, '-o', $output, '--runtime', $mode)
    if ($result.ExitCode -ne 0) { throw ("Prefix $mode build failed: " + $result.Output + $result.Error) }
    if (@((Read-EmbeddedApplication $output).Names | Where-Object { $_.EndsWith('.nbc') }).Count -ne 2) {
        throw 'Prefix build did not embed both bytecode functions.'
    }
}
$prefixDeployed = Join-Path $prefixOutput 'bundled.runtime'
$null = Read-CheckedRuntime $prefixDeployed
$prefixPlatform = if ((Split-Path -Leaf $runtimePath) -ieq 'ARM64') { 'ARM64' } else { 'x64' }
if (-not (Test-Path -LiteralPath (Join-Path $prefixDeployed "bin/$prefixPlatform/libnlsEngine.dll"))) {
    throw 'Bundled runtime directory depended on the installation directory name.'
}
if (@(Get-ChildItem -LiteralPath $prefixDeployed -Recurse -Attributes ReparsePoint).Count) {
    throw 'Prefix-produced runtime retained a link to its build inputs.'
}
$prefixSourceFull = [IO.Path]::GetFullPath($prefixSource)
if (-not $prefixSourceFull.StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid source cleanup path.' }
Remove-Item -LiteralPath $prefixSourceFull -Recurse -Force
$movedPrefix = [IO.Path]::GetFullPath((Join-Path $root 'relocated installed prefix'))
$prefixFull = [IO.Path]::GetFullPath($prefix)
foreach ($path in @($prefixFull, $movedPrefix)) {
    if (-not $path.StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid prefix relocation path.' }
}
Move-Item -LiteralPath $prefixFull -Destination $movedPrefix
$movedData = Join-Path $movedPrefix $PrefixDataDirectory
$movedBin = Join-Path $movedPrefix $PrefixBinaryDirectory
foreach ($environment in @(
    @{ NELSONC_RUNTIME_ROOT = $movedPrefix; PATH = '' },
    @{ NELSONC_RUNTIME_ROOT = $movedData; PATH = '' },
    @{ NELSON_RUNTIME_PATH = $movedBin; PATH = '' },
    @{ PATH = $movedBin }
)) {
    foreach ($executable in @($prefixInstalled, $prefixWindowed)) {
        $result = Invoke-Application $executable @() 30000 '' $environment
        if ($result.ExitCode -ne 0 -or $result.Output -notmatch 'API_VALUE=42') {
            throw ('Relocated installed-prefix application failed: ' + $result.Output + $result.Error)
        }
    }
}
$result = Invoke-Application $prefixBundled @() 30000 '' @{ PATH = ''; NELSONC_RUNTIME_ROOT = (Join-Path $root 'does-not-exist') }
if ($result.ExitCode -ne 0 -or $result.Output -notmatch 'API_VALUE=42') {
    throw ('Independent bundled application failed: ' + $result.Output + $result.Error)
}
$result = Invoke-Application (Join-Path $movedBin 'nelsonc.exe') @('--cli', '--help')
if ($result.ExitCode -ne 0 -or $result.Output -notmatch 'usage: nelsonc') { throw 'Relocated compiler failed.' }
Write-Output 'PASS installed prefix, direct data root, binary environment/PATH lookup, relocation and independent bundled execution'
Write-Output "PREFIX_ARTIFACT=$prefixOutput"
