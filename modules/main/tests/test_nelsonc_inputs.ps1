# Dot-sourced by test_nelsonc.ps1 using its isolated process/archive helpers.
$inputSource = Join-Path $root 'input source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_inputs') -Destination $inputSource -Recurse
$inputEntry = Join-Path $inputSource 'app/input_entry.m'
$inputExtra = Join-Path $inputSource 'extra'
$inputPattern = Join-Path $inputSource 'masks/data[1]+(?).*'
$null = New-Item -ItemType Directory -Path (Join-Path $inputExtra 'empty')
$inputArgs = @($inputEntry, '--cli', '-X', '-a', $inputExtra, '-a', $inputPattern)
$inputPlanRun = Invoke-Application $compiler ($inputArgs + @('--runtime', 'installed', '--explain-link'))
if ($inputPlanRun.ExitCode -ne 0) { throw ($inputPlanRun.Output + $inputPlanRun.Error) }
$inputPlan = $inputPlanRun.Output | ConvertFrom-Json
if (-not $inputPlan.complete -or $inputPlan.dynamicFallback) { throw 'Incomplete explicit input plan.' }
if ($inputPlan.files.Count -ne 6 -or $inputPlan.directories.Count -ne 5) { throw 'Unexpected explicit input membership.' }
$inputDistribution = Join-Path $root 'input-distribution'
foreach ($mode in @('bundled', 'installed')) {
    $exe = Join-Path $inputDistribution ($mode + '.exe')
    $build = Invoke-Application $compiler ($inputArgs + @('--runtime', $mode, '-o', $exe))
    if ($build.ExitCode -ne 0) { throw ($build.Output + $build.Error) }
    $archive = Read-EmbeddedApplication $exe
    $expected = @('application.json', 'roots/1/input_entry.nbc', 'roots/2/nested/nested_value.nbc',
        'roots/2/nested/private/private_value.nbc', 'roots/2/+inputpkg/value.nbc',
        'roots/2/nested/nested-data.txt', 'roots/3/data[1]+(a).txt')
    if (Compare-Object ($archive.Names | Where-Object { -not $_.EndsWith('/') }) $expected) { throw 'Unexpected input archive.' }
    if ($archive.Names -notcontains 'roots/2/empty/') { throw 'Archive lost an empty directory.' }
    if ($archive.Manifest.paths -notcontains 'roots/2/nested' -or $archive.Manifest.paths -contains 'roots/2/nested/private') { throw 'Invalid application lookup paths.' }
    if (($archive.Manifest | ConvertTo-Json -Depth 100) -match [regex]::Escape($inputSource)) { throw 'Build-host path leaked into application manifest.' }
}
$inputApiPlan = Join-Path $root 'input-api-plan.json'
$commonEntry = Join-Path $inputSource 'common/code/common_entry.m'
$commonBuild = Invoke-Application $compiler @($commonEntry, '--cli', '--runtime', 'installed', '-a', (Join-Path $inputSource 'common'), '-o', (Join-Path $inputDistribution 'common.exe'))
if ($commonBuild.ExitCode -ne 0) { throw ($commonBuild.Output + $commonBuild.Error) }
$commonArchive = Read-EmbeddedApplication (Join-Path $inputDistribution 'common.exe')
if ($commonArchive.Manifest.entry -ne 'roots/1/code/common_entry.nbc' -or $commonArchive.Names -notcontains 'roots/1/data/sibling.txt') { throw 'Selected ancestor lost sibling layout.' }
$inputApiExe = Join-Path $inputDistribution 'api.exe'
$inputExpression = @"
try;
ncc('--help');
beforePath = path(); beforeDirectory = pwd();
plan = ncc('$($inputEntry.Replace('\','/').Replace("'","''"))', '--cli', '-X', '-a', '$($inputExtra.Replace('\','/').Replace("'","''"))', '-a', '$($inputPattern.Replace('\','/').Replace("'","''"))', '--runtime', 'installed', '--explain-link');
filewrite('$($inputApiPlan.Replace('\','/').Replace("'","''"))', jsonencode(plan));
ncc('$($inputEntry.Replace('\','/').Replace("'","''"))', '--cli', '-X', '-a', '$($inputExtra.Replace('\','/').Replace("'","''"))', '-a', '$($inputPattern.Replace('\','/').Replace("'","''"))', '--runtime', 'installed', '-o', '$($inputApiExe.Replace('\','/').Replace("'","''"))');
asserts.isequal(path(), beforePath); asserts.isequal(pwd(), beforeDirectory);
disp('INPUT_API_OK'); exit(0);
catch ex; disp(ex.message); exit(1); end
"@
$api = Invoke-Application (Join-Path $runtimePath 'NelSon-cli.exe') @('--quiet', '--no-user-startup', '--no-user-modules', '--no-ipc', '--execute', $inputExpression)
if ($api.ExitCode -ne 0 -or $api.Output -notmatch 'INPUT_API_OK') { throw ($api.Output + $api.Error) }
$apiPlan = Get-Content -LiteralPath $inputApiPlan -Raw | ConvertFrom-Json
foreach ($field in @('files', 'directories', 'pathEntries', 'searchRoots', 'runtime')) {
    if (($apiPlan.$field | ConvertTo-Json -Depth 100 -Compress) -cne ($inputPlan.$field | ConvertTo-Json -Depth 100 -Compress)) { throw "Input CLI/API mismatch: $field" }
}
$inputRepeat = Join-Path $root 'input-repeat/installed.exe'
$repeat = Invoke-Application $compiler ($inputArgs + @('--runtime', 'installed', '-a', $inputExtra, '-o', $inputRepeat))
if ($repeat.ExitCode -ne 0) { throw ($repeat.Output + $repeat.Error) }
if ((Get-FileHash -LiteralPath $inputRepeat).Hash -ne (Get-FileHash -LiteralPath (Join-Path $inputDistribution 'installed.exe')).Hash) { throw 'Duplicate inputs changed executable bytes.' }
# A directory junction must be rejected before traversing its target.
$junction = Join-Path $root 'input-junction'
$null = New-Item -ItemType Junction -Path $junction -Target $inputExtra
$rejected = Invoke-Application $compiler @($inputEntry, '--cli', '-a', $junction, '--explain-link')
if ($rejected.ExitCode -eq 0 -or ($rejected.Output + $rejected.Error) -notmatch 'reparse') { throw 'Explicit directory junction was not rejected.' }
$resolvedJunction = [IO.Path]::GetFullPath($junction)
if (-not $resolvedJunction.StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid junction cleanup path.' }
Remove-Item -LiteralPath $resolvedJunction -Force
$inputRelocated = [IO.Path]::GetFullPath((Join-Path $root 'relocated inputs'))
foreach ($path in @($inputSource, $inputDistribution, $inputRelocated)) {
    if (-not [IO.Path]::GetFullPath($path).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid input test path.' }
}
Remove-Item -LiteralPath $inputSource -Recurse -Force
Move-Item -LiteralPath $inputDistribution -Destination $inputRelocated
$inputRuntime = Join-Path $inputRelocated 'bundled.runtime'
$null = Read-CheckedRuntime $inputRuntime
$inputBefore = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
foreach ($mode in @('bundled', 'installed', 'api', 'common')) {
    $environment = @{ PATH = '' }
    if ($mode -ne 'bundled') { $environment.NELSONC_RUNTIME_ROOT = $inputRuntime }
    $run = Invoke-Application (Join-Path $inputRelocated ($mode + '.exe')) @() 30000 '' $environment
    $run | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $root ('inputs-' + $mode + '.json'))
    if ($run.ExitCode -ne 0 -or $run.Output -notmatch 'INPUT_TREE_OK 42') { throw ($run.Output + $run.Error) }
}
if (Compare-Object $inputBefore @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)) { throw 'Input application left extraction data.' }
$null = Read-CheckedRuntime $inputRuntime
Write-Output 'PASS recursive directories, empty folders, filename patterns, private/package lookup, CLI/API parity, duplicate reproducibility, junction rejection and relocated source-free execution'
Write-Output "INPUT_ARTIFACT=$inputRelocated"
