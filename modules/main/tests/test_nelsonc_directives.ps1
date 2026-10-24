# Dot-sourced by test_nelsonc.ps1, using its isolated process/archive helpers.
$directiveSource = Join-Path $root 'directive-source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_directives') -Destination $directiveSource -Recurse
$directiveEntry = Join-Path $directiveSource 'directives_entry.m'
$directivePlanRun = Invoke-Application $compiler @($directiveEntry, '--cli', '--runtime', 'installed', '--explain-link')
if ($directivePlanRun.ExitCode -ne 0) { throw ($directivePlanRun.Output + $directivePlanRun.Error) }
$directivePlan = $directivePlanRun.Output | ConvertFrom-Json
if (-not $directivePlan.complete -or -not $directivePlan.dynamicFallback) { throw 'Directive application lost conservative dynamic analysis.' }
if ($directivePlan.files.relative -contains 'development_only.m' -or $directivePlan.files.relative -contains 'ignored.txt') { throw 'Excluded dependency retained.' }
if ($directivePlan.exclusions.Count -ne 2 -or $directivePlan.files.relative -notcontains 'included data.txt') { throw 'Missing dependency directive records.' }
$directiveDistribution = Join-Path $root 'directive-distribution'
foreach ($mode in @('bundled', 'installed')) {
    $exe = Join-Path $directiveDistribution ($mode + '.exe')
    $build = Invoke-Application $compiler @($directiveEntry, '--cli', '--runtime', $mode, '-o', $exe)
    if ($build.ExitCode -ne 0) { throw ($build.Output + $build.Error) }
    $archive = Read-EmbeddedApplication $exe
    $expected = @('application.json', 'roots/1/directives_entry.nbc', 'roots/1/z_configuration.nbc', 'roots/1/callback_helper.nbc', 'roots/1/included data.txt')
    if (Compare-Object ($archive.Names | Where-Object { -not $_.EndsWith('/') }) $expected) { throw 'Unexpected embedded directive payload.' }
}
$repeatedDirective = Join-Path $root 'directive-repeat/installed.exe'
$repeatBuild = Invoke-Application $compiler @($directiveEntry, '--cli', '--runtime', 'installed', '-o', $repeatedDirective)
if ($repeatBuild.ExitCode -ne 0) { throw ($repeatBuild.Output + $repeatBuild.Error) }
if ((Get-FileHash -LiteralPath $repeatedDirective).Hash -ne (Get-FileHash -LiteralPath (Join-Path $directiveDistribution 'installed.exe')).Hash) { throw 'Directive packaging is not byte-identical.' }
$relocatedDirectives = [IO.Path]::GetFullPath((Join-Path $root 'relocated directives'))
foreach ($path in @($directiveSource, $directiveDistribution, $relocatedDirectives)) {
    if (-not [IO.Path]::GetFullPath($path).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid directive test path.' }
}
Remove-Item -LiteralPath $directiveSource -Recurse -Force
Move-Item -LiteralPath $directiveDistribution -Destination $relocatedDirectives
$directiveRuntime = Join-Path $relocatedDirectives 'bundled.runtime'
$null = Read-CheckedRuntime $directiveRuntime
$beforeDirectives = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
foreach ($mode in @('bundled','installed')) {
    $environment = @{ PATH='' }
    if ($mode -eq 'installed') { $environment.NELSONC_RUNTIME_ROOT = $directiveRuntime }
    $run = Invoke-Application (Join-Path $relocatedDirectives ($mode + '.exe')) @() 30000 '' $environment
    $run | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $root ('directives-' + $mode + '.json'))
    if ($run.ExitCode -ne 0 -or $run.Output -notmatch 'DIRECTIVES_VALUE=42' -or $run.Output -notmatch 'DIRECTIVE_RESOURCE_OK') { throw ($run.Output + $run.Error) }
}
if (Compare-Object $beforeDirectives @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)) { throw 'Directive application left extraction data.' }
$null = Read-CheckedRuntime $directiveRuntime
Write-Output 'PASS dependency directives, late exclusions, dynamic fallback, byte-identical repeated packaging, exact bytecode/resource payload, source-free relocation and both runtimes'
Write-Output "DIRECTIVES_ARTIFACT=$relocatedDirectives"
