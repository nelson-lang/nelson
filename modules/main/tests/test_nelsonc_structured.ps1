# Invoked by test_nelsonc.ps1 with its isolated process helpers.
$structuredSource = Join-Path $root 'structured source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_structured') -Destination $structuredSource -Recurse
New-Item -ItemType Directory -Path (Join-Path $structuredSource 'assets/empty') | Out-Null
$structuredDistribution = Join-Path $root 'structured distribution'
$structuredReports = @{}
foreach ($structuredMode in @('bundled', 'installed')) {
    $reportFile = Join-Path $root ("structured-$structuredMode.json")
    $expression = @"
try;
asserts.isfalse(ismodule('compiler'));
cd('$($structuredSource.Replace('\', '/').Replace("'", "''"))');
opts = ncc('options', 'structured_entry.m', 'ExecutableName', 'structured_app', ...
  'OutputDir', '../structured distribution/$structuredMode', 'RuntimeMode', '$structuredMode', ...
  'AutoDetectDataFiles', false, 'AdditionalFiles', 'assets', 'CustomHelpTextFile', 'help.txt');
asserts.istrue(ismodule('compiler'));
loadedPath = path();
frozenOptions = struct(opts);
cd('$($root.Replace('\', '/').Replace("'", "''"))');
initialDirectory = pwd();
plan = nelson.compiler.analyze(opts);
asserts.isequal(ncc(opts, '--explain-link'), plan);
asserts.isfalse(isfolder(opts.OutputDir));
buildText = evalc('result = nelson.compiler.build(opts);');
asserts.isequal(buildText, '');
asserts.isequal(struct(opts), frozenOptions);
asserts.isequal(struct(result.Options), frozenOptions);
asserts.isequal(result.RuntimePlan, plan.runtime);
asserts.isequal(result.DependencyPlan.files, plan.files);
asserts.isequal(result.SHA256, sha256(result.Executable, '-file'));
opts.OutputDir = [opts.OutputDir, '-repeat'];
repeat = ncc(opts);
asserts.isequal(result.SHA256, repeat.SHA256);
asserts.isequal(struct(result.Options), frozenOptions);
asserts.isequal(pwd(), initialDirectory);
asserts.isequal(path(), loadedPath);
filewrite('$($reportFile.Replace('\', '/').Replace("'", "''"))', ...
  jsonencode(struct('result', struct(result), 'repeat', struct(repeat), 'plan', plan)));
disp('STRUCTURED_BUILD_OK');
exit(0);
catch ex; disp(ex.message); exit(1); end
"@
    $build = Invoke-Application (Join-Path $runtimePath 'NelSon-adv-cli.exe') @(
        '-q', '--language', 'en_US', '--no-user-startup', '--no-user-modules', '--no-ipc', '--timeout', '180', '-e', $expression)
    if ($build.ExitCode -ne 0 -or $build.Output -notmatch 'STRUCTURED_BUILD_OK') {
        throw ('Structured build failed: ' + $build.Output + $build.Error)
    }
    $report = Get-Content -LiteralPath $reportFile -Raw | ConvertFrom-Json
    $structuredReports[$structuredMode] = $report
    $application = Join-Path $structuredDistribution "$structuredMode/structured_app.exe"
    if ([IO.Path]::GetFullPath($report.result.Executable) -ine [IO.Path]::GetFullPath($application)) {
        throw 'Incorrect executable reported by structured API.'
    }
    if ($report.result.SHA256 -ine (Get-FileHash -LiteralPath $application).Hash -or
        $report.repeat.SHA256 -cne $report.result.SHA256) { throw 'Incorrect result digest or nonreproducible structured build.' }
    $expectedFiles = @($report.result.Executable)
    $deployedRuntime = Join-Path (Split-Path -Parent $application) 'structured_app.runtime'
    if ($structuredMode -eq 'bundled') {
        if ([IO.Path]::GetFullPath($report.result.RuntimeDirectory) -ine [IO.Path]::GetFullPath($deployedRuntime)) { throw 'Incorrect bundled runtime directory.' }
        $null = Read-CheckedRuntime $deployedRuntime
        $expectedFiles += $report.result.RuntimeDirectory
    } elseif ($report.result.RuntimeDirectory -cne '' -or (Test-Path -LiteralPath $deployedRuntime)) {
        throw 'Structured installed-mode build copied or reported a runtime directory.'
    }
    if (Compare-Object $expectedFiles @($report.result.Files)) { throw 'Incomplete structured distribution paths.' }
    $archive = Read-EmbeddedApplication $application
    if (@($archive.Names | Where-Object { $_.EndsWith('.nbc') }).Count -ne 2 -or
        @($archive.Names | Where-Object { $_.EndsWith('.m') }).Count -ne 0 -or
        @($archive.Names | Where-Object { $_.EndsWith('/assets/empty/') }).Count -ne 1) {
        throw 'Structured application archive differs from its selected inputs.'
    }
    if (($archive.Manifest | ConvertTo-Json -Depth 100 -Compress) -cne
        ($report.result.Manifest | ConvertTo-Json -Depth 100 -Compress)) { throw 'Result manifest differs from executable manifest.' }
    $cli = Invoke-Application $compiler @((Join-Path $structuredSource 'structured_entry.m'),
        '--runtime', $structuredMode, '-X', '-a', (Join-Path $structuredSource 'assets'),
        '--help-file', (Join-Path $structuredSource 'help.txt'), '--explain-link')
    if ($cli.ExitCode -ne 0) { throw ('Structured CLI comparison failed: ' + $cli.Output + $cli.Error) }
    $cliPlan = $cli.Output | ConvertFrom-Json
    foreach ($field in @('files', 'edges', 'modules', 'builtinCatalog', 'builtinCatalogDigest', 'runtime', 'helpText')) {
        if (($report.plan.$field | ConvertTo-Json -Depth 100 -Compress) -cne
            ($cliPlan.$field | ConvertTo-Json -Depth 100 -Compress)) { throw "Structured CLI plan differs: $field" }
    }
}
$junction = Join-Path $root 'structured-junction'
$null = New-Item -ItemType Junction -Path $junction -Target (Join-Path $structuredSource 'assets')
$expression = @"
try;
opts = ncc('options', '$($structuredSource.Replace('\', '/').Replace("'", "''"))/structured_entry.m', ...
  'AdditionalFiles', '$($junction.Replace('\', '/').Replace("'", "''"))', 'RuntimeMode', 'installed');
asserts.istrue(contains(opts.AdditionalFiles{1}, 'structured-junction'));
rejected = false;
try; nelson.compiler.analyze(opts); catch ex; rejected = contains(ex.message, 'reparse'); end;
asserts.istrue(rejected);
disp('STRUCTURED_LINK_REJECTED'); exit(0);
catch ex; disp(ex.message); exit(1); end
"@
$linkCheck = Invoke-Application (Join-Path $runtimePath 'NelSon-adv-cli.exe') @(
    '-q', '--language', 'en_US', '--no-user-startup', '--no-user-modules', '--no-ipc', '-e', $expression)
if ($linkCheck.ExitCode -ne 0 -or $linkCheck.Output -notmatch 'STRUCTURED_LINK_REJECTED') {
    throw ('Structured API bypassed link rejection: ' + $linkCheck.Output + $linkCheck.Error)
}
$junctionFull = [IO.Path]::GetFullPath($junction)
if (-not $junctionFull.StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid structured junction cleanup path.' }
Remove-Item -LiteralPath $junctionFull -Force
$sourceFull = [IO.Path]::GetFullPath($structuredSource)
$distributionFull = [IO.Path]::GetFullPath($structuredDistribution)
$relocated = [IO.Path]::GetFullPath((Join-Path $root 'relocated structured distribution'))
foreach ($owned in @($sourceFull, $distributionFull, $relocated)) {
    if (-not $owned.StartsWith($root + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase)) {
        throw 'Invalid structured test cleanup or relocation path.'
    }
}
Remove-Item -LiteralPath $sourceFull -Recurse -Force
Move-Item -LiteralPath $distributionFull -Destination $relocated
foreach ($structuredMode in @('bundled', 'installed')) {
    $environment = @{ PATH = '' }
    if ($structuredMode -eq 'installed') {
        $environment.NELSONC_RUNTIME_ROOT = Split-Path -Parent (Split-Path -Parent $runtimePath)
    }
    foreach ($suffix in @('', '-repeat')) {
        $application = Join-Path $relocated ("$structuredMode$suffix/structured_app.exe")
        $run = Invoke-Application $application @('value with spaces') 30000 '' $environment -CaptureModules
        if ($run.ExitCode -ne 0 -or $run.Output -notmatch 'STRUCTURED_VALUE=42' -or
            $run.Output -notmatch 'STRUCTURED_ARGUMENT=value with spaces') {
            throw ('Source-free structured application failed: ' + $run.Output + $run.Error)
        }
        if (@($run.Modules | Where-Object { [IO.Path]::GetFileName($_) -like 'libnlsCompiler*.dll' }).Count) {
            throw 'Structured application loaded the build-only module.'
        }
        $help = Invoke-Application $application @('--help') 30000 '' $environment
        if ($help.ExitCode -ne 0 -or $help.Output -notmatch 'STRUCTURED_HELP' -or $help.Output -match 'STRUCTURED_VALUE') {
            throw ('Structured application help failed: ' + $help.Output + $help.Error)
        }
        if ((Get-FileHash -LiteralPath $application).Hash -ine $structuredReports[$structuredMode].result.SHA256) {
            throw 'Structured application changed after relocation or execution.'
        }
    }
}
if (@(Get-ChildItem -LiteralPath $root -Directory -Filter 'nelsonc-*').Count) { throw 'Structured application extraction was not cleaned.' }
Write-Output 'PASS structured options/results, lazy factory, frozen paths, CLI parity, repeatable builds, exact runtimes and source-free relocation in both modes'
Write-Output "STRUCTURED_ARTIFACTS=$root"
