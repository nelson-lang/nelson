# Invoked by test_nelsonc.ps1 with its isolated process helpers.
$apiSource = Join-Path $root 'api-source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_api') -Destination $apiSource -Recurse
$apiEntry = Join-Path $apiSource 'api_entry.m'
$apiOutput = Join-Path $root 'api-output/application.exe'
$apiPlanFile = Join-Path $root 'api-plan.json'
$apiExpression = @"
try;
asserts.isfalse(ismodule('compiler'));
cd('$($root.Replace('\', '/').Replace("'", "''"))');
initialDirectory = pwd();
initialEnvironment = getenv('NELSONC_INVOKING_DIRECTORY');
usage = ncc('--help');
asserts.istrue(ismodule('compiler'));
loadedPath = path();
plan = ncc('api-source/api_entry.m', '--runtime', 'installed', '--explain-link');
filewrite('$($apiPlanFile.Replace('\', '/').Replace("'", "''"))', jsonencode(plan));
output = ncc('api-source/api_entry.m', '-o', 'api-output/application.exe', '--runtime', 'installed');
asserts.isequal(strrep(output, '\', '/'), '$($apiOutput.Replace('\', '/').Replace("'", "''"))');
asserts.isequal(path(), loadedPath);
asserts.isequal(pwd(), initialDirectory);
asserts.isequal(getenv('NELSONC_INVOKING_DIRECTORY'), initialEnvironment);
fprintf('API_SESSION_CONTINUED\n');
exit(0);
catch ex; disp(ex.message); exit(1); end
"@
$apiResult = Invoke-Application (Join-Path $runtimePath 'NelSon-adv-cli.exe') @(
    '-q', '--language', 'en_US', '--no-user-startup', '--no-user-modules', '--no-ipc', '--timeout', '90', '-e', $apiExpression)
if ($apiResult.ExitCode -ne 0 -or $apiResult.Output -notmatch 'API_SESSION_CONTINUED' -or
    -not (Test-Path -LiteralPath $apiOutput)) { throw ('In-session packaging failed: ' + $apiResult.Output + $apiResult.Error) }
$apiPlan = Get-Content -LiteralPath $apiPlanFile -Raw | ConvertFrom-Json
$apiCli = Invoke-Application $compiler @($apiEntry, '--runtime', 'installed', '--explain-link')
if ($apiCli.ExitCode -ne 0) { throw ('API comparison CLI failed: ' + $apiCli.Output + $apiCli.Error) }
$apiCliPlan = $apiCli.Output | ConvertFrom-Json
foreach ($field in @('files', 'modules', 'builtinCatalog', 'builtinCatalogDigest', 'runtime')) {
    if (($apiPlan.$field | ConvertTo-Json -Depth 100 -Compress) -cne
        ($apiCliPlan.$field | ConvertTo-Json -Depth 100 -Compress)) { throw "CLI/API plans differ: $field" }
}
$apiArchive = Read-EmbeddedApplication $apiOutput
if (@($apiArchive.Names | Where-Object { $_.EndsWith('.nbc') }).Count -ne 2 -or
    @($apiArchive.Names | Where-Object { $_.EndsWith('.m') }).Count -ne 0) { throw 'Direct API did not embed both source files as bytecode.' }
if (Test-Path -LiteralPath (Join-Path $root 'api-output/application.runtime')) { throw 'Direct installed-mode API copied runtime.' }
$apiSourceFull = [IO.Path]::GetFullPath($apiSource)
if (-not $apiSourceFull.StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid API test source path.' }
Remove-Item -LiteralPath $apiSourceFull -Recurse -Force
$apiRuntimeRoot = Split-Path -Parent (Split-Path -Parent $runtimePath)
$apiRun = Invoke-Application $apiOutput @() 30000 '' @{ NELSONC_RUNTIME_ROOT = $apiRuntimeRoot; PATH = '' } -CaptureModules
if ($apiRun.ExitCode -ne 0 -or $apiRun.Output -notmatch 'API_VALUE=42') { throw ('API-built source-free execution failed: ' + $apiRun.Output + $apiRun.Error) }
if (@($apiRun.Modules | Where-Object { [IO.Path]::GetFileName($_) -like 'libnlsCompiler*.dll' }).Count) {
    throw 'Build-only library was loaded by a deployed application.'
}
Write-Output 'PASS public ncc API, same-session return, nelsonc CLI plan parity, lazy build module and two embedded source-free functions'
Write-Output "API_ARTIFACT=$apiOutput"
