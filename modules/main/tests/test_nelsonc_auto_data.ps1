# Dot-sourced by test_nelsonc.ps1 using its isolated process/archive helpers.
$dataSource = Join-Path $root 'auto-data-source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_auto_data') -Destination $dataSource -Recurse
$dataEntry = Join-Path $dataSource 'auto_data_entry.m'
$dataExplicit = Join-Path $dataSource 'forced_by_flag.txt'
$dataHelp = Join-Path $dataSource 'help.txt'
$dataCommon = @($dataEntry, '--cli', '--runtime', 'installed', '-a', $dataExplicit, '--help-file', $dataHelp)
$dataDefaultRun = Invoke-Application $compiler ($dataCommon + @('--explain-link'))
if ($dataDefaultRun.ExitCode -ne 0) { throw ($dataDefaultRun.Output + $dataDefaultRun.Error) }
$dataDefaultPlan = $dataDefaultRun.Output | ConvertFrom-Json
if (-not $dataDefaultPlan.autoDetectDataFiles -or -not $dataDefaultPlan.complete) { throw 'Default data detection is not enabled.' }
foreach ($name in @('caller-data.txt','transitive-data.txt')) {
    if ($dataDefaultPlan.files.relative -notcontains $name) { throw "Missing default automatic resource: $name" }
}
$dataPlanRun = Invoke-Application $compiler ($dataCommon + @('-X', '--explain-link'))
if ($dataPlanRun.ExitCode -ne 0) { throw ($dataPlanRun.Output + $dataPlanRun.Error) }
$dataPlan = $dataPlanRun.Output | ConvertFrom-Json
if ($dataPlan.autoDetectDataFiles -or -not $dataPlan.complete -or $dataPlan.dynamicFallback) { throw 'Invalid no-auto-data dependency plan.' }
foreach ($name in @('caller-data.txt','transitive-data.txt')) {
    if ($dataPlan.files.relative -contains $name) { throw "Automatic resource retained with -X: $name" }
}
if (-not $dataPlan.exclusions.explicitOverride -or $dataPlan.files.relative -notcontains 'forced_by_pragma.txt' -or $dataPlan.files.relative -notcontains 'forced_by_flag.txt') { throw 'Explicit resource priority was lost.' }
if (($dataPlan.runtime | ConvertTo-Json -Depth 100 -Compress) -cne ($dataDefaultPlan.runtime | ConvertTo-Json -Depth 100 -Compress)) { throw 'Data detection changed the required runtime.' }
$dataDistribution = Join-Path $root 'auto-data-distribution'
foreach ($mode in @('bundled','installed')) {
    $exe = Join-Path $dataDistribution ($mode + '.exe')
    $flag = if ($mode -eq 'bundled') { '-X' } else { '--no-auto-data' }
    $build = Invoke-Application $compiler @($dataEntry, '--cli', '--runtime', $mode, $flag, '-a', $dataExplicit, '--help-file', $dataHelp, '-o', $exe)
    if ($build.ExitCode -ne 0) { throw ($build.Output + $build.Error) }
    $archive = Read-EmbeddedApplication $exe
    $expected = @('application.json', 'roots/1/auto_data_entry.nbc', 'roots/1/auto_data_helper.nbc', 'roots/1/forced_by_pragma.txt', 'roots/1/forced_by_flag.txt')
    if (Compare-Object ($archive.Names | Where-Object { -not $_.EndsWith('/') }) $expected) { throw 'Unexpected automatic-data payload.' }
    if ($archive.Manifest.helpText -notmatch 'AUTO_DATA_HELP_OK') { throw 'Data option lost custom help.' }
}
$dataApiPlanFile = Join-Path $root 'auto-data-api-plan.json'
$dataApiOutput = Join-Path $dataDistribution 'api.exe'
$dataApiExpression = @"
try;
initialDirectory = pwd();
initialEnvironment = getenv('NELSONC_INVOKING_DIRECTORY');
plan = ncc('$($dataEntry.Replace('\','/').Replace("'","''"))', '--cli', '--runtime', 'installed', '--no-auto-data', '-a', '$($dataExplicit.Replace('\','/').Replace("'","''"))', '--help-file', '$($dataHelp.Replace('\','/').Replace("'","''"))', '--explain-link');
filewrite('$($dataApiPlanFile.Replace('\','/').Replace("'","''"))', jsonencode(plan));
ncc('$($dataEntry.Replace('\','/').Replace("'","''"))', '--cli', '--runtime', 'installed', '-X', '-a', '$($dataExplicit.Replace('\','/').Replace("'","''"))', '--help-file', '$($dataHelp.Replace('\','/').Replace("'","''"))', '-o', '$($dataApiOutput.Replace('\','/').Replace("'","''"))');
asserts.isequal(pwd(), initialDirectory);
asserts.isequal(getenv('NELSONC_INVOKING_DIRECTORY'), initialEnvironment);
disp('AUTO_DATA_API_OK'); exit(0);
catch ex; disp(ex.message); exit(1); end
"@
$dataApiRun = Invoke-Application (Join-Path $runtimePath 'NelSon-cli.exe') @('--quiet','--no-user-startup','--no-user-modules','--no-ipc','--execute',$dataApiExpression)
if ($dataApiRun.ExitCode -ne 0 -or $dataApiRun.Output -notmatch 'AUTO_DATA_API_OK') { throw ($dataApiRun.Output + $dataApiRun.Error) }
$dataApiPlan = Get-Content -LiteralPath $dataApiPlanFile -Raw | ConvertFrom-Json
foreach ($field in @('files','edges','exclusions','modules','runtime','autoDetectDataFiles','helpText')) {
    if (($dataApiPlan.$field | ConvertTo-Json -Depth 100 -Compress) -cne ($dataPlan.$field | ConvertTo-Json -Depth 100 -Compress)) { throw "Data option CLI/API mismatch: $field" }
}
$dataRepeat = Join-Path $root 'auto-data-repeat/installed.exe'
$dataRepeatRun = Invoke-Application $compiler ($dataCommon + @('--no-auto-data', '-o', $dataRepeat))
if ($dataRepeatRun.ExitCode -ne 0) { throw ($dataRepeatRun.Output + $dataRepeatRun.Error) }
if ((Get-FileHash -LiteralPath $dataRepeat).Hash -ne (Get-FileHash -LiteralPath (Join-Path $dataDistribution 'installed.exe')).Hash) { throw 'Data options did not produce byte-identical outputs.' }
Copy-Item -LiteralPath (Join-Path $dataSource 'caller-input.txt') -Destination (Join-Path $root 'caller-data.txt')
Copy-Item -LiteralPath (Join-Path $dataSource 'transitive-input.txt') -Destination (Join-Path $root 'transitive-data.txt')
$dataRelocated = [IO.Path]::GetFullPath((Join-Path $root 'relocated auto data'))
foreach ($path in @($dataSource, $dataDistribution, $dataRelocated)) {
    if (-not [IO.Path]::GetFullPath($path).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid automatic data test path.' }
}
Remove-Item -LiteralPath $dataSource -Recurse -Force
Move-Item -LiteralPath $dataDistribution -Destination $dataRelocated
$dataRuntime = Join-Path $dataRelocated 'bundled.runtime'
$null = Read-CheckedRuntime $dataRuntime
$dataBefore = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
foreach ($mode in @('bundled','installed','api')) {
    $environment = @{ PATH='' }
    if ($mode -ne 'bundled') { $environment.NELSONC_RUNTIME_ROOT = $dataRuntime }
    $exe = Join-Path $dataRelocated ($mode + '.exe')
    $run = Invoke-Application $exe @() 30000 '' $environment
    $run | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $root ('auto-data-' + $mode + '.json'))
    if ($run.ExitCode -ne 0) { throw ($run.Output + $run.Error) }
    foreach ($marker in @('CALLER_INPUT_OK','TRANSITIVE_INPUT_OK','PRAGMA_DATA_OK','EXPLICIT_DATA_OK')) {
        if ($run.Output -notmatch $marker) { throw "Missing $marker in $mode output: $($run.Output)$($run.Error)" }
    }
    $help = Invoke-Application $exe @('--help') 30000 '' $environment
    if ($help.ExitCode -ne 0 -or $help.Output -notmatch 'AUTO_DATA_HELP_OK' -or $help.Output -match 'CALLER_INPUT_OK') { throw 'No-auto-data application help failed.' }
}
if (Compare-Object $dataBefore @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)) { throw 'Data option application left extraction data.' }
$null = Read-CheckedRuntime $dataRuntime
Write-Output 'PASS automatic data opt-out, unchanged runtime, directive/-a priority, CLI/API parity, custom help, byte-identical packaging and relocated source-free execution'
Write-Output "AUTO_DATA_ARTIFACT=$dataRelocated"
