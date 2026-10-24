# Dot-sourced by test_nelsonc.ps1; all processes and files are isolated there.
function Read-ExecutableSections([string]$Executable) {
    $bytes = [IO.File]::ReadAllBytes($Executable)
    $header = [BitConverter]::ToInt32($bytes, 60)
    $count = [BitConverter]::ToUInt16($bytes, $header + 6)
    $table = $header + 24 + [BitConverter]::ToUInt16($bytes, $header + 20)
    $sections = [ordered]@{}
    $sha = [Security.Cryptography.SHA256]::Create()
    try {
        for ($index = 0; $index -lt $count; $index++) {
            $section = $table + $index * 40
            $name = [Text.Encoding]::ASCII.GetString($bytes, $section, 8).TrimEnd([char]0)
            if ($name -eq '.rsrc') { continue }
            $size = [BitConverter]::ToInt32($bytes, $section + 16)
            $offset = [BitConverter]::ToInt32($bytes, $section + 20)
            $sections[$name] = [Convert]::ToHexString($sha.ComputeHash($bytes, $offset, $size))
        }
    } finally { $sha.Dispose() }
    return $sections | ConvertTo-Json -Compress
}

function Assert-ExecutableVersion([string]$Executable, [string]$Expected) {
    $version = [Diagnostics.FileVersionInfo]::GetVersionInfo($Executable)
    if ($version.FileVersion -cne $Expected -or $version.ProductVersion -cne $Expected) {
        throw "Executable text version differs: $Executable; $($version.FileVersion); $($version.ProductVersion)"
    }
    $fixed = @($version.FileMajorPart, $version.FileMinorPart, $version.FileBuildPart, $version.FilePrivatePart) -join '.'
    $product = @($version.ProductMajorPart, $version.ProductMinorPart, $version.ProductBuildPart, $version.ProductPrivatePart) -join '.'
    if ($fixed -cne $Expected -or $product -cne $Expected) { throw 'Fixed executable version differs.' }
}

$versionSource = Join-Path $root ('version source ' + [char]0xe9 + [char]0x4e2d)
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_version') -Destination $versionSource -Recurse
$versionEntry = Join-Path $versionSource 'version_entry.m'
$versionDistribution = Join-Path $root 'version-distribution'
$versionPlan = Invoke-Application $compiler @($versionEntry, '--runtime', 'installed', '--executable-version', '4.0', '--explain-link')
if ($versionPlan.ExitCode -ne 0) { throw ($versionPlan.Output + $versionPlan.Error) }
$plan = $versionPlan.Output | ConvertFrom-Json
if ($plan.executableVersion -cne '4.0' -or $plan.runtime.gui) { throw 'Executable version plan differs.' }
$launchersBefore = @{}
foreach ($name in @('nelson-app.exe', 'nelson-app-windowed.exe')) {
    $launchersBefore[$name] = (Get-FileHash -LiteralPath (Join-Path $runtimePath $name)).Hash
}
foreach ($deployment in @('bundled', 'installed')) {
    foreach ($kind in @('console', 'windowed')) {
        $original = Join-Path $versionDistribution "$deployment-$kind-default.exe"
        $versioned = Join-Path $versionDistribution "$deployment-$kind-versioned.exe"
        $common = @('--runtime', $deployment)
        $stubName = 'nelson-app.exe'
        if ($kind -eq 'windowed') { $common += '--no-console'; $stubName = 'nelson-app-windowed.exe' }
        $build = Invoke-Application $compiler (@($versionEntry, '-o', $original) + $common)
        if ($build.ExitCode -ne 0) { throw ($build.Output + $build.Error) }
        Assert-ExecutableVersion $original '1.0.0.0'
        $build = Invoke-Application $compiler (@($versionEntry, '-o', $versioned, '--executable-version', '4.0') + $common)
        if ($build.ExitCode -ne 0) { throw ($build.Output + $build.Error) }
        Assert-ExecutableVersion $versioned '4.0.0.0'
        if ((Read-ExecutableSections $versioned) -cne (Read-ExecutableSections (Join-Path $runtimePath $stubName))) {
            throw 'Version metadata changed non-resource native sections.'
        }
        $bytes = [IO.File]::ReadAllBytes($versioned)
        $other = [IO.File]::ReadAllBytes($original)
        if ([Text.Encoding]::ASCII.GetString($bytes, $bytes.Length - 320, 64) -cne
            [Text.Encoding]::ASCII.GetString($other, $other.Length - 320, 64)) { throw 'Version changed the payload digest.' }
        $archive = Read-EmbeddedApplication $versioned
        if (@($archive.Names | Where-Object { $_.EndsWith('.m') }).Count) { throw 'Source leaked into versioned application.' }
        if ($deployment -eq 'bundled') {
            $first = Read-CheckedRuntime ([IO.Path]::ChangeExtension($original, 'runtime'))
            $second = Read-CheckedRuntime ([IO.Path]::ChangeExtension($versioned, 'runtime'))
            if (($first | ConvertTo-Json -Depth 30 -Compress) -cne ($second | ConvertTo-Json -Depth 30 -Compress)) {
                throw 'Version metadata changed runtime bytes or dependencies.'
            }
        } elseif (Test-Path -LiteralPath ([IO.Path]::ChangeExtension($versioned, 'runtime'))) { throw 'Installed mode copied a runtime.' }
    }
}
$repeated = Join-Path $versionDistribution 'repeated.exe'
$build = Invoke-Application $compiler @($versionEntry, '-o', $repeated, '--runtime', 'installed', '--no-console', '--executable-version', '4.0')
if ($build.ExitCode -ne 0) { throw ($build.Output + $build.Error) }
if ((Get-FileHash -LiteralPath $repeated).Hash -cne (Get-FileHash -LiteralPath (Join-Path $versionDistribution 'installed-windowed-versioned.exe')).Hash) {
    throw 'Version resource writing is not reproducible.'
}
$edge = Join-Path $versionDistribution 'edge.exe'
$build = Invoke-Application $compiler @($versionEntry, '-o', $edge, '--runtime', 'installed', '--executable-version', '65535.65535.65535.65535')
if ($build.ExitCode -ne 0) { throw ($build.Output + $build.Error) }
Assert-ExecutableVersion $edge '65535.65535.65535.65535'
foreach ($invalidVersion in @('65536', '1.2.3.4.5', '1..2', '')) {
    $invalid = Join-Path $versionDistribution 'invalid/output.exe'
    $build = Invoke-Application $compiler @($versionEntry, '-o', $invalid, '--runtime', 'installed', '--executable-version', $invalidVersion)
    if ($build.ExitCode -eq 0 -or (Test-Path -LiteralPath (Split-Path -Parent $invalid))) { throw 'Invalid version was not rejected before assembly.' }
}
$versionRelocated = [IO.Path]::GetFullPath((Join-Path $root 'relocated version'))
foreach ($checked in @($versionSource, $versionDistribution, $versionRelocated)) {
    if (-not [IO.Path]::GetFullPath($checked).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid version test path.' }
}
Remove-Item -LiteralPath $versionSource -Recurse -Force
Move-Item -LiteralPath $versionDistribution -Destination $versionRelocated
foreach ($deployment in @('bundled', 'installed')) {
    foreach ($kind in @('console', 'windowed')) {
        $environment = @{ PATH = ''; QT_PLUGIN_PATH = 'invalid' }
        if ($deployment -eq 'installed') { $environment.NELSONC_RUNTIME_ROOT = Join-Path $versionRelocated 'bundled-console-versioned.runtime' }
        $application = Join-Path $versionRelocated "$deployment-$kind-versioned.exe"
        Assert-ExecutableVersion $application '4.0.0.0'
        $destination = Join-Path $root "$deployment-$kind-version.json"
        $arguments = @('', 'two words', ('caf' + [char]0xe9), 'a"b', 'ends\')
        $run = Invoke-Application $application (@($destination) + $arguments) 30000 '' $environment -CaptureModules
        if ($run.ExitCode -ne 0 -or $run.Output -notmatch 'VERSION_EXECUTION_OK') { throw ($run.Output + $run.Error) }
        $report = Get-Content -LiteralPath $destination -Raw | ConvertFrom-Json
        if (-not $report.deployed -or $report.value -ne 338350 -or
            ($report.arguments | ConvertTo-Json -Compress) -cne ($arguments | ConvertTo-Json -Compress)) { throw 'Versioned application output differs.' }
        if ([IO.Path]::GetFullPath($report.directory) -ine $root -or (Test-Path -LiteralPath $report.archive)) { throw 'Versioned application directory or cleanup differs.' }
        Assert-ConsoleLibraries $run.Modules
        if (@($run.Modules | Where-Object { [IO.Path]::GetFileName($_) -like 'libnlsCompiler*' }).Count) { throw 'Execution loaded build-only compiler services.' }
        if (-not @($run.Modules | Where-Object { [IO.Path]::GetFileName($_) -eq 'libnlsEngine.dll' }).Count) { throw 'Module capture did not observe the engine.' }
    }
}
foreach ($name in $launchersBefore.Keys) {
    if ((Get-FileHash -LiteralPath (Join-Path $runtimePath $name)).Hash -cne $launchersBefore[$name]) { throw 'Installed launcher was modified.' }
}
Write-Output 'PASS native file/product versions, unchanged code sections/payload/runtime, reproducibility and source-free relocated execution'
Write-Output "VERSION_ARTIFACTS=$versionRelocated"
