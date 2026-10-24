# Dot-sourced by test_nelsonc.ps1; all output stays inside its unique test directory.
$helpSource = Join-Path $root 'help-source'
New-Item -ItemType Directory -Path $helpSource | Out-Null
$helpEntry = Join-Path $helpSource 'help_entry.m'
$helpFile = Join-Path $helpSource 'help.txt'
$expectedHelp = 'HELP_CONTENT 100% %s ' + [char]233 + [char]955 + [char]20013 + "`n"
[IO.File]::WriteAllText($helpFile, $expectedHelp)
[IO.File]::WriteAllText($helpEntry, "function help_entry(varargin); disp('HELP_ENTRY_EXECUTED'); disp(jsonencode(varargin)); end")
$helpDistribution = Join-Path $root 'help-distribution'
foreach ($mode in @('bundled','installed')) {
    $exe = Join-Path $helpDistribution ($mode + '.exe')
    $build = Invoke-Application $compiler @($helpEntry, '--help-file', $helpFile, '--cli', '--runtime', $mode, '-o', $exe)
    if ($build.ExitCode -ne 0) { throw ($build.Output + $build.Error) }
    $archive = Read-EmbeddedApplication $exe
    if ($archive.Manifest.helpText -cne $expectedHelp -or $archive.Names -notcontains 'roots/1/help_entry.nbc') { throw 'Embedded custom help differs.' }
    if (@($archive.Names | Where-Object { $_.EndsWith('.m') }).Count) { throw 'Custom help executable contains source.' }
}
[IO.File]::WriteAllText((Join-Path $root '--cli'), $expectedHelp)
$graphicalEntry = Join-Path $PSScriptRoot 'fixtures/nelsonc_graphics/graphics_entry.m'
$optionValue = Invoke-Application $compiler @($graphicalEntry, '--help-file', '--cli', '--runtime', 'installed', '--explain-link')
if ($optionValue.ExitCode -ne 0) { throw ($optionValue.Output + $optionValue.Error) }
$helpPlan = $optionValue.Output | ConvertFrom-Json
if (-not $helpPlan.runtime.gui -or $helpPlan.helpText -cne $expectedHelp) { throw 'Help filename changed compiler mode or explain output.' }
$relocatedHelp = [IO.Path]::GetFullPath((Join-Path $root 'relocated help'))
foreach ($path in @($helpSource, $helpDistribution, $relocatedHelp)) {
    if (-not [IO.Path]::GetFullPath($path).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid custom help test path.' }
}
Remove-Item -LiteralPath $helpSource -Force -Recurse
Move-Item -LiteralPath $helpDistribution -Destination $relocatedHelp
$helpRuntime = Join-Path $relocatedHelp 'bundled.runtime'
$null = Read-CheckedRuntime $helpRuntime
$beforeHelp = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
foreach ($mode in @('bundled','installed')) {
    $exe = Join-Path $relocatedHelp ($mode + '.exe')
    $environment = @{ PATH='' }
    if ($mode -eq 'installed') { $environment.NELSONC_RUNTIME_ROOT = $helpRuntime }
    foreach ($flag in @('-?', '/?', '--help')) {
        $run = Invoke-Application $exe @($flag) 30000 '' $environment
        if ($run.ExitCode -ne 0 -or $run.Output.Replace("`r`n", "`n") -cne $expectedHelp -or $run.Error) { throw "Custom help failed ($mode/$flag): $($run.Output)$($run.Error)" }
    }
    $run = Invoke-Application $exe @('caller', '--help') 30000 '' $environment
    if ($run.ExitCode -ne 0 -or $run.Output -notmatch 'HELP_ENTRY_EXECUTED' -or $run.Output -notmatch '\["caller","--help"\]') { throw 'Ordinary application arguments changed.' }
}
if (Compare-Object $beforeHelp @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)) { throw 'Custom help left an extraction directory.' }
$null = Read-CheckedRuntime $helpRuntime
Write-Output 'PASS UTF-8 embedded help, no entry execution, option-like filename, ordinary arguments, source-free relocation and both runtime modes'
Write-Output "HELP_ARTIFACT=$relocatedHelp"
