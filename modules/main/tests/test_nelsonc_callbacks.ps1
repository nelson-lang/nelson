# Dot-sourced by test_nelsonc.ps1; uses its isolated process and archive helpers.
$callbackSource = Join-Path $root 'callback-source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_callbacks') -Destination $callbackSource -Recurse
$callbackEntry = Join-Path $callbackSource 'callbacks_entry.m'
$callbackPlanRun = Invoke-Application $compiler @($callbackEntry, '--runtime', 'installed', '--explain-link')
if ($callbackPlanRun.ExitCode -ne 0) { throw ($callbackPlanRun.Output + $callbackPlanRun.Error) }
$callbackPlan = $callbackPlanRun.Output | ConvertFrom-Json
if (-not $callbackPlan.complete -or -not $callbackPlan.dynamicFallback) { throw 'Text callbacks did not retain dynamic dependencies.' }
$callbackDistribution = Join-Path $root 'callback-distribution'
foreach ($mode in @('bundled', 'installed')) {
    $application = Join-Path $callbackDistribution ($mode + '.exe')
    $build = Invoke-Application $compiler @($callbackEntry, '-o', $application, '--runtime', $mode)
    if ($build.ExitCode -ne 0) { throw ($build.Output + $build.Error) }
    $archive = Read-EmbeddedApplication $application
    if (-not $archive.Manifest.gui) { throw 'Text callback application lost GUI lifetime.' }
    foreach ($name in @('callbacks_entry','callback_emit','callback_close','callback_finish')) {
        if ($archive.Names -notcontains "roots/1/$name.nbc") { throw "Missing bytecode callback: $name" }
    }
    if (@($archive.Names | Where-Object { $_.EndsWith('.m') }).Count) { throw 'Callback source leaked into the archive.' }
}
if (Test-Path -LiteralPath (Join-Path $callbackDistribution 'installed.runtime')) { throw 'Installed callback mode copied a runtime.' }
$callbackRelocated = [IO.Path]::GetFullPath((Join-Path $root 'relocated callbacks'))
foreach ($checked in @($callbackSource, $callbackDistribution, $callbackRelocated)) {
    if (-not [IO.Path]::GetFullPath($checked).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid callback test path.' }
}
Remove-Item -LiteralPath $callbackSource -Recurse -Force
Move-Item -LiteralPath $callbackDistribution -Destination $callbackRelocated
$callbackRuntime = Join-Path $callbackRelocated 'bundled.runtime'
$null = Read-CheckedRuntime $callbackRuntime
$callbackDirectories = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
foreach ($deployment in @('bundled', 'installed')) {
    foreach ($mode in @('create', 'close', 'timer')) {
        $environment = @{ PATH = ''; QT_PLUGIN_PATH = 'invalid' }
        if ($deployment -eq 'installed') { $environment.NELSONC_RUNTIME_ROOT = $callbackRuntime }
        $run = Invoke-Application (Join-Path $callbackRelocated ($deployment + '.exe')) @($mode) 30000 '' $environment
        $run | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $root "callback-$deployment-$mode.json")
        $expected = switch ($mode) {
            'create' { 'CREATE_TEXT_OK[\s\S]*CALLBACK_ENTRY_RETURN' }
            'close' { 'CLOSE_TEXT_OK[\s\S]*CALLBACK_ENTRY_RETURN' }
            'timer' { 'CALLBACK_ENTRY_RETURN[\s\S]*TIMER_TEXT_OK' }
        }
        if ($run.ExitCode -ne 0 -or $run.Output -notmatch $expected) {
            throw ("Relocated text callback failed ($deployment/$mode): " + $run.Output + $run.Error)
        }
    }
}
$callbackAfter = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
if (Compare-Object $callbackDirectories $callbackAfter) { throw 'Text callback application left an extraction directory.' }
$null = Read-CheckedRuntime $callbackRuntime
Write-Output 'PASS embedded text callbacks, creation, close requests, post-return timer, relocation and installed runtime'
Write-Output ("CALLBACK_ARTIFACT=" + (Join-Path $callbackRelocated 'bundled.exe'))
