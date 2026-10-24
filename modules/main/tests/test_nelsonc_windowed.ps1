# Dot-sourced by test_nelsonc.ps1; all processes and files are isolated there.
function Read-WindowedImage([string]$Executable) {
    $bytes = [IO.File]::ReadAllBytes($Executable)
    $header = [BitConverter]::ToInt32($bytes, 60)
    [pscustomobject]@{
        Subsystem = [BitConverter]::ToUInt16($bytes, $header + 24 + 68)
        ArchiveHash = [Text.Encoding]::ASCII.GetString($bytes, $bytes.Length - 352 + 32, 64)
    }
}

function Assert-WindowedCleanup {
    if (@(Get-ChildItem -LiteralPath $root -Directory -Filter 'nelsonc-*').Count) {
        throw 'Windowed application left an extraction directory.'
    }
}

function Invoke-WindowedBuild([string]$Entry, [string]$Output, [string]$Deployment, [bool]$NoConsole) {
    $arguments = @($Entry, '-o', $Output, '--runtime', $Deployment)
    if ($NoConsole) { $arguments += '--no-console' }
    $build = Invoke-Application $compiler $arguments
    if ($build.ExitCode -ne 0 -or -not (Test-Path -LiteralPath $Output)) {
        throw ("Windowed packaging failed: " + $build.Output + $build.Error)
    }
    $image = Read-WindowedImage $Output
    $expected = if ($NoConsole) { 2 } else { 3 }
    if ($image.Subsystem -ne $expected) { throw 'Incorrect native executable subsystem.' }
    $archive = Read-EmbeddedApplication $Output
    if (@($archive.Names | Where-Object { $_.EndsWith('.m') }).Count) { throw 'Windowed source leaked into the executable.' }
}

$windowedSource = Join-Path $root 'windowed-source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_windowed') -Destination $windowedSource -Recurse
$windowedEntry = Join-Path $windowedSource 'windowed_entry.m'
$windowedDistribution = Join-Path $root 'windowed-distribution'
$planRun = Invoke-Application $compiler @($windowedEntry, '--no-console', '--cli', '--runtime', 'installed', '--explain-link')
if ($planRun.ExitCode -ne 0) { throw ($planRun.Output + $planRun.Error) }
$plan = $planRun.Output | ConvertFrom-Json
if (-not $plan.noConsole -or $plan.runtime.gui) { throw 'No-console mode changed graphics selection.' }
foreach ($deployment in @('bundled', 'installed')) {
    foreach ($kind in @('console', 'windowed')) {
        Invoke-WindowedBuild $windowedEntry (Join-Path $windowedDistribution "$deployment-$kind.exe") $deployment ($kind -eq 'windowed')
    }
    $consoleImage = Read-WindowedImage (Join-Path $windowedDistribution "$deployment-console.exe")
    $windowedImage = Read-WindowedImage (Join-Path $windowedDistribution "$deployment-windowed.exe")
    if ($consoleImage.ArchiveHash -cne $windowedImage.ArchiveHash) { throw 'No-console mode changed the embedded payload.' }
}
Invoke-WindowedBuild $windowedEntry (Join-Path $windowedDistribution 'repeated.exe') 'installed' $true
if ((Get-FileHash -LiteralPath (Join-Path $windowedDistribution 'repeated.exe')).Hash -cne
    (Get-FileHash -LiteralPath (Join-Path $windowedDistribution 'installed-windowed.exe')).Hash) { throw 'Windowed packaging is not reproducible.' }
if (Test-Path -LiteralPath (Join-Path $windowedDistribution 'installed-windowed.runtime')) { throw 'Installed mode copied a runtime.' }
$windowedRelocated = [IO.Path]::GetFullPath((Join-Path $root 'relocated windowed'))
foreach ($checked in @($windowedSource, $windowedDistribution, $windowedRelocated)) {
    if (-not [IO.Path]::GetFullPath($checked).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid windowed test path.' }
}
Remove-Item -LiteralPath $windowedSource -Recurse -Force
Move-Item -LiteralPath $windowedDistribution -Destination $windowedRelocated
$windowedRuntime = Join-Path $windowedRelocated 'bundled-windowed.runtime'
$consoleRuntime = Join-Path $windowedRelocated 'bundled-console.runtime'
$windowedInventory = Read-CheckedRuntime $windowedRuntime
$consoleInventory = Read-CheckedRuntime $consoleRuntime
if (($windowedInventory | ConvertTo-Json -Depth 30 -Compress) -cne ($consoleInventory | ConvertTo-Json -Depth 30 -Compress)) {
    throw 'No-console mode changed the runtime inventory.'
}
$unicode = 'caf' + [char]0xe9 + '-' + [char]0x4e2d + [char]::ConvertFromUtf32(0x1f600)
Add-Type -Path (Join-Path $PSScriptRoot 'fixtures/nelsonc_windowed/ConsoleLaunch.cs')
$savedEnvironment = @{}
foreach ($item in Get-ChildItem Env:) {
    if ($item.Name.StartsWith('NELSON') -or $item.Name -in @('TEMP', 'TMP')) { $savedEnvironment[$item.Name] = $item.Value }
}
try {
    foreach ($item in Get-ChildItem Env:NELSON*) { Remove-Item -LiteralPath $item.PSPath }
    $env:TEMP = $root
    $env:TMP = $root
    $env:NELSON_PREFERENCES_PATH = Join-Path $root 'preferences'
    foreach ($kind in @('console', 'windowed')) {
        $destination = Join-Path $root "native-$kind-console.json"
        $status = [NelsonConsoleLaunch]::Run((Join-Path $windowedRelocated "bundled-$kind.exe"), $destination, $root)
        if ($status -ne 0) { throw "Native console control failed: $status" }
        $report = Get-Content -LiteralPath $destination -Raw | ConvertFrom-Json
        if (($kind -eq 'windowed') -ne ($report.console -eq 0)) { throw "Native console allocation differs: $kind" }
        Assert-WindowedCleanup
    }
} finally {
    foreach ($item in Get-ChildItem Env:NELSON*) { Remove-Item -LiteralPath $item.PSPath }
    foreach ($name in @('TEMP', 'TMP')) { [Environment]::SetEnvironmentVariable($name, $null) }
    foreach ($name in $savedEnvironment.Keys) { [Environment]::SetEnvironmentVariable($name, $savedEnvironment[$name]) }
}
$forwarded = @('', 'two words', $unicode, 'a"b', 'ends\', '--literal', '"quoted"')
foreach ($deployment in @('bundled', 'installed')) {
    foreach ($kind in @('console', 'windowed')) {
        $environment = @{ PATH = ''; QT_PLUGIN_PATH = 'invalid' }
        if ($deployment -eq 'installed') { $environment.NELSONC_RUNTIME_ROOT = $windowedRuntime }
        $application = Join-Path $windowedRelocated "$deployment-$kind.exe"
        $destination = Join-Path $root "$deployment-$kind-result.json"
        # Retain pipe redirection for arguments/diagnostics; allocation is tested above.
        $run = Invoke-Application $application (@($destination, 'probe') + $forwarded) 30000 '' $environment -CaptureModules -AllowConsole
        $run | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $root "$deployment-$kind-process.json")
        if ($run.ExitCode -ne 0 -or -not (Test-Path -LiteralPath $destination)) { throw ($run.Output + $run.Error) }
        $report = Get-Content -LiteralPath $destination -Raw | ConvertFrom-Json
        if ($kind -eq 'windowed' -and $report.console -ne 0) { throw 'Windowed application unexpectedly attached a console.' }
        if (-not $report.deployed -or $report.value -ne 338350) { throw 'Windowed numeric result differs.' }
        if ($report.arguments.Count -ne $forwarded.Count) { throw 'Windowed argument count differs.' }
        for ($index = 0; $index -lt $forwarded.Count; $index++) {
            if ($report.arguments[$index] -cne $forwarded[$index]) { throw "Windowed argument differs: $index" }
        }
        if ([IO.Path]::GetFullPath($report.directory) -ine $root -or (Test-Path -LiteralPath $report.archive)) { throw 'Windowed working directory or cleanup differs.' }
        Assert-ConsoleLibraries $run.Modules
        if (@($run.Modules | Where-Object { [IO.Path]::GetFileName($_) -like 'libnlsCompiler*' }).Count) { throw 'Windowed execution loaded the compiler.' }
        if (-not @($run.Modules | Where-Object { [IO.Path]::GetFileName($_) -eq 'libnlsEngine.dll' }).Count) { throw 'Module capture did not observe the engine.' }
        Assert-WindowedCleanup
    }
    foreach ($mode in @('error', 'exit')) {
        $destination = Join-Path $root "$deployment-$mode-result.json"
        $run = Invoke-Application (Join-Path $windowedRelocated "$deployment-windowed.exe") @($destination, $mode) 30000 '' $environment -AllowConsole
        $expected = if ($mode -eq 'error') { 1 } else { 7 }
        if ($run.ExitCode -ne $expected) { throw "Windowed exit status differs: $($run.ExitCode); $($run.Output) $($run.Error)" }
        if ($mode -eq 'error' -and ($run.Output + $run.Error) -notmatch 'WINDOWED_EXPECTED_ERROR') { throw 'Windowed redirected diagnostic is missing.' }
        Assert-WindowedCleanup
    }
}
$missingRuntime = Join-Path $root 'absent-windowed-runtime'
$failure = Invoke-Application (Join-Path $windowedRelocated 'installed-windowed.exe') @() 30000 '' @{ NELSONC_RUNTIME_ROOT = $missingRuntime; PATH = '' } -AllowConsole
if ($failure.ExitCode -ne 2 -or ($failure.Output + $failure.Error) -notmatch 'runtime|Runtime') { throw 'Windowed startup failure was not reported.' }
Assert-WindowedCleanup
$null = Read-CheckedRuntime $windowedRuntime
$incompatible = Join-Path $root 'incompatible-windowed-runtime'
$incompatibleBin = Join-Path $incompatible ('bin/' + (Split-Path -Leaf $runtimePath))
New-Item -ItemType Directory -Path $incompatibleBin, (Join-Path $incompatible 'modules/interpreter/functions') | Out-Null
foreach ($relative in @('modules/modules.m', 'modules/interpreter/functions/__nelsonc_run__.m')) {
    Copy-Item -LiteralPath (Join-Path $windowedRuntime $relative) -Destination (Join-Path $incompatible $relative)
}
Copy-Item -LiteralPath (Join-Path $runtimePath 'libnlsEngine.dll') -Destination $incompatibleBin
New-Item -ItemType File -Path (Join-Path $incompatibleBin 'libnlsInterpreter.dll') | Out-Null
$failure = Invoke-Application (Join-Path $windowedRelocated 'installed-windowed.exe') @() 30000 '' @{ NELSONC_RUNTIME_ROOT = $incompatible; PATH = '' } -CaptureModules -AllowConsole
if ($failure.ExitCode -ne 2 -or ($failure.Output + $failure.Error) -notmatch 'Incompatible installed Nelson runtime') { throw 'Windowed incompatible runtime was not rejected.' }
if (@($failure.Modules | Where-Object { [IO.Path]::GetFileName($_) -eq 'libnlsEngine.dll' }).Count) { throw 'Incompatible windowed runtime loaded its engine.' }
Assert-WindowedCleanup
$null = Read-CheckedRuntime $consoleRuntime
Write-Output 'PASS native Windows subsystem, real console control, Unicode arguments, identical payload/runtime, reproducibility, relocation and exit cleanup'

# Reuse the graphical and textual callback fixtures through the windowed entry.
foreach ($fixture in @('graphics', 'callbacks')) {
    $source = Join-Path $root "windowed-$fixture-source"
    Copy-Item -LiteralPath (Join-Path $PSScriptRoot "fixtures/nelsonc_$fixture") -Destination $source -Recurse
    $distribution = Join-Path $root "windowed-$fixture-distribution"
    foreach ($deployment in @('bundled', 'installed')) {
        Invoke-WindowedBuild (Join-Path $source ($fixture + '_entry.m')) (Join-Path $distribution ($deployment + '.exe')) $deployment $true
    }
    $relocated = [IO.Path]::GetFullPath((Join-Path $root "windowed $fixture relocated"))
    foreach ($checked in @($source, $distribution, $relocated)) {
        if (-not [IO.Path]::GetFullPath($checked).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid windowed graphics test path.' }
    }
    Remove-Item -LiteralPath $source -Recurse -Force
    Move-Item -LiteralPath $distribution -Destination $relocated
    $graphicsRuntime = Join-Path $relocated 'bundled.runtime'
    $null = Read-CheckedRuntime $graphicsRuntime
    $modes = if ($fixture -eq 'graphics') { @('batch', 'offscreen', 'hide', 'lifetime') } else { @('create', 'close', 'timer') }
    foreach ($deployment in @('bundled', 'installed')) {
        foreach ($mode in $modes) {
            $environment = @{ PATH = ''; QT_PLUGIN_PATH = 'invalid' }
            if ($deployment -eq 'installed') { $environment.NELSONC_RUNTIME_ROOT = $graphicsRuntime }
            $run = Invoke-Application (Join-Path $relocated ($deployment + '.exe')) @($mode) 30000 '' $environment -CaptureModules -AllowConsole
            $run | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $root "windowed-$fixture-$deployment-$mode.json")
            $expected = switch ($mode) {
                'batch' { 'GUI_RENDER_OK' }
                'offscreen' { 'GUI_RENDER_OK[\s\S]*GUI_OFFSCREEN_RETURN' }
                'hide' { 'GUI_ENTRY_RETURN[\s\S]*GUI_HIDDEN_OK' }
                'lifetime' { 'GUI_ENTRY_RETURN[\s\S]*GUI_NEXT_WINDOW[\s\S]*GUI_LIFETIME_OK' }
                'create' { 'CREATE_TEXT_OK[\s\S]*CALLBACK_ENTRY_RETURN' }
                'close' { 'CLOSE_TEXT_OK[\s\S]*CALLBACK_ENTRY_RETURN' }
                'timer' { 'CALLBACK_ENTRY_RETURN[\s\S]*TIMER_TEXT_OK' }
            }
            if ($run.ExitCode -ne 0 -or $run.Output -notmatch $expected) { throw "Windowed GUI failure ($fixture/$mode): $($run.Output) $($run.Error)" }
            foreach ($library in @('Qt6Core.dll', 'Qt6Gui.dll', 'Qt6Widgets.dll', 'qwindows.dll')) {
                $paths = @($run.Modules | Where-Object { [IO.Path]::GetFileName($_) -ieq $library })
                if ($paths.Count -ne 1 -or -not $paths[0].StartsWith($graphicsRuntime + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase)) {
                    throw "Windowed graphics loaded the wrong library: $library"
                }
            }
            Assert-WindowedCleanup
        }
    }
    $null = Read-CheckedRuntime $graphicsRuntime
}
Write-Output 'PASS windowed rendering, uicontrol creation/close callbacks, post-return timers and successive/hidden-window lifetime in both runtime modes'
Write-Output "WINDOWED_ARTIFACTS=$root"
