#Requires -Version 7.0
param([Parameter(Mandatory = $true)][string]$Runtime, [string]$NativeLibraries = '', [switch]$NativeOnly, [switch]$CallbacksOnly, [switch]$ApiOnly,
    [switch]$PrefixOnly, [string]$PrefixLaunchers = '', [string]$PrefixDataDirectory = 'share/Nelson', [string]$PrefixBinaryDirectory = 'bin', [switch]$ReproducibleOnly, [switch]$HelpOnly, [switch]$DirectivesOnly, [switch]$AutoDataOnly, [switch]$InputsOnly, [switch]$StructuredOnly, [switch]$WindowedOnly, [switch]$VersionOnly)
$ErrorActionPreference = 'Stop'
if (([int]$NativeOnly.IsPresent + [int]$CallbacksOnly.IsPresent + [int]$ApiOnly.IsPresent + [int]$PrefixOnly.IsPresent + [int]$ReproducibleOnly.IsPresent + [int]$HelpOnly.IsPresent + [int]$DirectivesOnly.IsPresent + [int]$AutoDataOnly.IsPresent + [int]$InputsOnly.IsPresent + [int]$StructuredOnly.IsPresent + [int]$WindowedOnly.IsPresent + [int]$VersionOnly.IsPresent) -gt 1) {
    throw 'The Only switches are mutually exclusive.'
}
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$root = Join-Path $repo ('temp/nelsonc-integration-' + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $root | Out-Null
$runtimePath = (Resolve-Path -LiteralPath $Runtime).Path
$compiler = Join-Path $runtimePath 'nelsonc.exe'
if (-not (Test-Path -LiteralPath $compiler)) { throw 'Build nelsonc first.' }

function Invoke-Application([string]$Executable, [string[]]$Arguments, [int]$Timeout = 180000, [string]$UserPath = '', [hashtable]$Environment = @{}, [switch]$CaptureModules, [switch]$AllowConsole) {
    $info = [Diagnostics.ProcessStartInfo]::new()
    $info.FileName = $Executable
    $info.WorkingDirectory = $root
    $info.UseShellExecute = $false
    $info.CreateNoWindow = -not $AllowConsole
    $info.WindowStyle = [Diagnostics.ProcessWindowStyle]::Hidden
    $info.RedirectStandardOutput = $true
    $info.RedirectStandardError = $true
    $info.StandardOutputEncoding = [Text.Encoding]::UTF8
    $info.StandardErrorEncoding = [Text.Encoding]::UTF8
    foreach ($key in @($info.Environment.Keys)) {
        if ($key.StartsWith('NELSON')) { $info.Environment.Remove($key) | Out-Null }
    }
    $info.Environment['TEMP'] = $root
    $info.Environment['TMP'] = $root
    $info.Environment['NELSON_PREFERENCES_PATH'] = Join-Path $root 'preferences'
    if ($UserPath) { $info.Environment['NELSON_USERPATH'] = $UserPath }
    foreach ($key in $Environment.Keys) { $info.Environment[$key] = $Environment[$key] }
    foreach ($argument in $Arguments) { $info.ArgumentList.Add($argument) }
    $process = [Diagnostics.Process]::Start($info)
    $stdout = $process.StandardOutput.ReadToEndAsync()
    $stderr = $process.StandardError.ReadToEndAsync()
    $loaded = [Collections.Generic.HashSet[string]]::new([StringComparer]::OrdinalIgnoreCase)
    $elapsed = [Diagnostics.Stopwatch]::StartNew()
    while ($CaptureModules -and -not $process.HasExited -and $elapsed.ElapsedMilliseconds -lt $Timeout) {
        try {
            $process.Refresh()
            foreach ($module in $process.Modules) { $null = $loaded.Add($module.FileName) }
        } catch {
            if (-not $process.HasExited) { throw }
        }
        $null = $process.WaitForExit(25)
    }
    $remaining = [Math]::Max(0, $Timeout - [int]$elapsed.ElapsedMilliseconds)
    if (-not $process.WaitForExit($remaining)) {
        $process.Kill($true)
        $process.WaitForExit()
        throw "Application timeout: $Executable"
    }
    $result = [pscustomobject]@{ ExitCode = $process.ExitCode; Output = $stdout.Result; Error = $stderr.Result; Modules = @($loaded) }
    $process.Dispose()
    return $result
}

function Read-CheckedRuntime([string]$deployedRuntime) {
    $inventory = Get-Content -LiteralPath (Join-Path $deployedRuntime 'runtime.json') -Raw | ConvertFrom-Json
    if ($inventory.modules -contains 'compiler' -or
        @($inventory.libraries | Where-Object { $_.filename -like 'libnlsCompiler*' }).Count) {
        throw 'Build-only module leaked into the deployed runtime.'
    }
    if ($inventory.PSObject.Properties.Name -contains 'inputs' -or $inventory.PSObject.Properties.Name -contains 'bootstrapFiles') {
        throw 'Build-host input paths leaked into the runtime inventory.'
    }
    $expectedRuntimeFiles = @('runtime.json')
    foreach ($file in @($inventory.files) + @($inventory.generated)) {
        if ($file.PSObject.Properties.Name -contains 'source') { throw 'Build-host source path leaked into runtime inventory.' }
        $target = [IO.Path]::GetFullPath((Join-Path $deployedRuntime $file.relative))
        if (-not $target.StartsWith($deployedRuntime + [IO.Path]::DirectorySeparatorChar)) { throw 'Runtime inventory path escapes its root.' }
        if ((Get-FileHash -LiteralPath $target -Algorithm SHA256).Hash -ine $file.sha256) {
            throw "Runtime snapshot digest differs: $($file.relative)"
        }
        $expectedRuntimeFiles += $file.relative
    }
    $actualRuntimeFiles = @(Get-ChildItem -LiteralPath $deployedRuntime -Recurse -File | ForEach-Object {
        [IO.Path]::GetRelativePath($deployedRuntime, $_.FullName).Replace('\', '/')
    })
    if (Compare-Object $expectedRuntimeFiles $actualRuntimeFiles) { throw 'Runtime snapshot file inventory is not exact.' }
    return $inventory
}

function Read-EmbeddedApplication([string]$Executable) {
    $bytes = [IO.File]::ReadAllBytes($Executable)
    $trailer = $bytes.Length - 352
    if ([Text.Encoding]::ASCII.GetString($bytes, $trailer, 8) -ne 'NLSAPP01') { throw 'Missing archive.' }
    $offset = [BitConverter]::ToInt64($bytes, $trailer + 16)
    $length = [BitConverter]::ToInt64($bytes, $trailer + 24)
    $payload = [IO.MemoryStream]::new($bytes, [int]$offset, [int]$length, $false)
    $zip = [IO.Compression.ZipArchive]::new($payload, [IO.Compression.ZipArchiveMode]::Read)
    $names = @($zip.Entries | ForEach-Object { $_.FullName })
    $reader = [IO.StreamReader]::new($zip.GetEntry('application.json').Open())
    $manifest = $reader.ReadToEnd() | ConvertFrom-Json
    $reader.Dispose()
    $zip.Dispose()
    $payload.Dispose()
    return [pscustomobject]@{ Names = $names; Manifest = $manifest }
}

function Assert-ConsoleLibraries([string[]]$Names) {
    $unexpected = @($Names | Where-Object {
        [IO.Path]::GetFileName($_) -match '^(?i:Qt[0-9].*|libnlsGui|libnlsWeb_gui|libnlsWebview|WebView2Loader)\.dll$'
    })
    if ($unexpected.Count) { throw "Unneeded graphical library in numerical runtime: $($unexpected -join ', ')" }
}

if ($VersionOnly) {
    . (Join-Path $PSScriptRoot 'test_nelsonc_version.ps1')
    exit 0
}
if ($WindowedOnly) {
    . (Join-Path $PSScriptRoot 'test_nelsonc_windowed.ps1')
    exit 0
}
if ($StructuredOnly) {
    . (Join-Path $PSScriptRoot 'test_nelsonc_structured.ps1')
    exit 0
}
if ($InputsOnly) {
    . (Join-Path $PSScriptRoot 'test_nelsonc_inputs.ps1')
    exit 0
}
if ($AutoDataOnly) {
    . (Join-Path $PSScriptRoot 'test_nelsonc_auto_data.ps1')
    exit 0
}
if ($DirectivesOnly) {
    . (Join-Path $PSScriptRoot 'test_nelsonc_directives.ps1')
    exit 0
}
if ($HelpOnly) {
    . (Join-Path $PSScriptRoot 'test_nelsonc_help.ps1')
    exit 0
}
if ($ReproducibleOnly) {
    . (Join-Path $PSScriptRoot 'test_nelsonc_reproducible.ps1')
    exit 0
}
if ($PrefixOnly) {
    . (Join-Path $PSScriptRoot 'test_nelsonc_prefix.ps1')
    exit 0
}
if ($CallbacksOnly) {
    . (Join-Path $PSScriptRoot 'test_nelsonc_callbacks.ps1')
    exit 0
}
if ($NativeOnly) {
    if (-not $NativeLibraries) { throw 'NativeOnly requires NativeLibraries.' }
    . (Join-Path $PSScriptRoot 'test_nelsonc_native.ps1')
    return
}

. (Join-Path $PSScriptRoot 'test_nelsonc_api.ps1')
if ($ApiOnly) { exit 0 }
. (Join-Path $PSScriptRoot 'test_nelsonc_structured.ps1')
. (Join-Path $PSScriptRoot 'test_nelsonc_windowed.ps1')
. (Join-Path $PSScriptRoot 'test_nelsonc_version.ps1')
. (Join-Path $PSScriptRoot 'test_nelsonc_reproducible.ps1')
. (Join-Path $PSScriptRoot 'test_nelsonc_help.ps1')
. (Join-Path $PSScriptRoot 'test_nelsonc_directives.ps1')
. (Join-Path $PSScriptRoot 'test_nelsonc_auto_data.ps1')
. (Join-Path $PSScriptRoot 'test_nelsonc_inputs.ps1')
. (Join-Path $PSScriptRoot 'test_nelsonc_prefix.ps1')
$source = Join-Path $root 'source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc') -Destination $source -Recurse
$entry = Join-Path $source 'app_entry.m'
$resource = Join-Path $source 'data.txt'
$distribution = Join-Path $root 'distribution'
$application = Join-Path $distribution 'application.exe'
$help = Invoke-Application $compiler @('--help')
if ($help.ExitCode -ne 0 -or $help.Output -notmatch 'usage: nelsonc') { throw 'CLI help failed.' }
if ($help.Output -notmatch '--cli') { throw 'Missing explicit CLI mode help.' }
foreach ($modeArguments in @(@('--cli'), @('--gui', '--cli'))) {
    $modeResult = Invoke-Application $compiler (@($entry, '-a', $resource, '--explain-link') + $modeArguments)
    if ($modeArguments.Count -eq 1) {
        if ($modeResult.ExitCode -ne 0) { throw ($modeResult.Output + $modeResult.Error) }
        $modePlan = $modeResult.Output | ConvertFrom-Json
        if ($modePlan.runtime.gui) { throw 'Explicit CLI mode selected graphics.' }
    } elseif ($modeResult.ExitCode -eq 0 -or $modeResult.Output -notmatch 'mutually exclusive') {
        throw 'Conflicting GUI and CLI options accepted.'
    }
}
$explain = Invoke-Application $compiler @($entry, '-a', $resource, '--explain-link')
if ($explain.ExitCode -ne 0) { throw $explain.Output }
$plan = $explain.Output | ConvertFrom-Json
if (-not $plan.complete -or $plan.files.Count -ne 5) { throw 'Unexpected dependency plan.' }
$build = Invoke-Application $compiler @($entry, '-a', $resource, '-o', $application)
if ($build.ExitCode -ne 0 -or -not (Test-Path -LiteralPath $application)) {
    throw ("Packaging failed: " + $build.Output + $build.Error)
}
$hash = (Get-FileHash -LiteralPath $application -Algorithm SHA256).Hash
$duplicate = Invoke-Application $compiler @($entry, '-a', $resource, '-o', $application)
if ($duplicate.ExitCode -eq 0 -or (Get-FileHash -LiteralPath $application).Hash -ne $hash) {
    throw 'Existing output was not preserved.'
}

$embedded = Read-EmbeddedApplication $application
$names = $embedded.Names
if (@($names | Where-Object { $_.EndsWith('.nbc') }).Count -ne 4) { throw 'Missing embedded bytecode.' }
if (@($names | Where-Object { $_.EndsWith('.m') }).Count -ne 0) { throw 'Application source leaked.' }
$installedApplication = Join-Path $distribution 'installed.exe'
$installedBuild = Invoke-Application $compiler @($entry, '-a', $resource, '-o', $installedApplication, '--runtime', 'installed')
if ($installedBuild.ExitCode -ne 0 -or -not (Test-Path -LiteralPath $installedApplication)) {
    throw ("Installed-runtime packaging failed: " + $installedBuild.Output + $installedBuild.Error)
}
if (Test-Path -LiteralPath (Join-Path $distribution 'installed.runtime')) { throw 'Installed mode copied a runtime.' }
$installedEmbedded = Read-EmbeddedApplication $installedApplication
if ($installedEmbedded.Manifest.runtimeMode -cne 'installed' -or (Compare-Object $names $installedEmbedded.Names)) {
    throw 'Installed mode changed the embedded application payload.'
}
$installedPlanResult = Invoke-Application $compiler @($entry, '--runtime', 'installed', '--explain-link', '-a', $resource)
if ($installedPlanResult.ExitCode -ne 0) { throw $installedPlanResult.Output }
$installedPlan = $installedPlanResult.Output | ConvertFrom-Json
if ($installedPlan.runtime.mode -cne 'installed' -or $installedPlan.runtime.PSObject.Properties.Name -contains 'files') {
    throw 'Installed runtime explanation still contains a copy inventory.'
}
foreach ($invalidArguments in @(@($entry, '--runtime'), @($entry, '--runtime', 'unknown'))) {
    $invalidOption = Invoke-Application $compiler $invalidArguments
    if ($invalidOption.ExitCode -eq 0 -or ($invalidOption.Output + $invalidOption.Error) -notmatch 'runtime|Runtime') {
        throw 'Invalid runtime option accepted.'
    }
}
$installedHash = (Get-FileHash -LiteralPath $installedApplication).Hash
$installedDuplicate = Invoke-Application $compiler @($entry, '-a', $resource, '-o', $installedApplication, '--runtime', 'installed')
if ($installedDuplicate.ExitCode -eq 0 -or (Get-FileHash -LiteralPath $installedApplication).Hash -ne $installedHash) {
    throw 'Installed-runtime mode overwrote its existing executable.'
}
$links = @(Get-ChildItem -LiteralPath $distribution -Recurse -Attributes ReparsePoint)
if ($links.Count -ne 0) { throw 'Distribution still refers to the build tree.' }
$deployedRuntime = Join-Path $distribution 'application.runtime'
$inventory = Read-CheckedRuntime $deployedRuntime
$platform = Split-Path -Leaf $runtimePath
$binaryDirectory = Join-Path $deployedRuntime ('bin/' + $platform)
$dlls = @(Get-ChildItem -LiteralPath $binaryDirectory -File -Filter '*.dll')
if ($dlls.Count -ne $inventory.libraries.Count) { throw 'Native inventory differs from distributed files.' }
Assert-ConsoleLibraries @($dlls | ForEach-Object Name)
$sourceDlls = @(Get-ChildItem -LiteralPath $runtimePath -File -Filter '*.dll')
if ($dlls.Count -ge $sourceDlls.Count) { throw 'Runtime was not reduced.' }
foreach ($library in $plan.runtime.libraries) {
    $copied = Join-Path $binaryDirectory $library.filename
    if ((Get-FileHash -LiteralPath $copied).Hash -ne (Get-FileHash -LiteralPath $library.source).Hash) {
        throw "Native library changed during packaging: $($library.filename)"
    }
}
$moduleNames = @(Get-ChildItem -LiteralPath (Join-Path $deployedRuntime 'modules') -Directory | ForEach-Object Name)
if (Compare-Object $moduleNames $inventory.modules) { throw 'Runtime module inventory differs.' }
foreach ($excluded in @('graphics', 'gui', 'webview', 'audio', 'tests_manager')) {
    if ($moduleNames -contains $excluded) { throw "Unneeded module was distributed: $excluded" }
}
foreach ($license in @('gpl-3.0.md', 'lgpl-3.0.md', 'THIRDPARTY.md', 'TRADEMARKS.md')) {
    if (-not (Test-Path -LiteralPath (Join-Path $deployedRuntime $license))) { throw "Missing notice: $license" }
}
if ((Test-Path -LiteralPath (Join-Path $runtimePath 'mkl_license.txt')) -and
    -not (Test-Path -LiteralPath (Join-Path $binaryDirectory 'mkl_license.txt'))) { throw 'Missing numerical backend notice.' }
$sourceFull = [IO.Path]::GetFullPath($source)
if (-not $sourceFull.StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid test source path.' }
Remove-Item -LiteralPath $sourceFull -Recurse -Force
$relocated = Join-Path $root 'relocated app'
$distributionFull = [IO.Path]::GetFullPath($distribution)
if (-not $distributionFull.StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid test distribution path.' }
Move-Item -LiteralPath $distributionFull -Destination $relocated
$application = Join-Path $relocated 'application.exe'

function Assert-PersistentOutput([string]$Name) {
    $path = Join-Path $root $Name
    if (-not (Test-Path -LiteralPath $path)) { throw "Output was not preserved in invocation directory: $Name" }
    $workingDirectory = [IO.Path]::GetFullPath((Get-Content -LiteralPath $path -Raw))
    if ($workingDirectory -ne $root) { throw "Application changed working directory: $workingDirectory" }
}
$shadow = Join-Path $root 'external-user-path'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_shadow') -Destination $shadow -Recurse
Get-ChildItem -LiteralPath $shadow -File | Copy-Item -Destination $root
[IO.File]::WriteAllText((Join-Path $root 'invocation-input.txt'), 'caller input')
[IO.File]::WriteAllText((Join-Path $root 'data.txt'), 'not the embedded resource')

$unicode = [string][char]0x00e9 + [string][char]0x03bb + [string][char]0x4e2d
$arguments = @('un argument', '--option', 'a"b', '', 'C:\path with space\', $unicode)
$initialDirectories = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
$run = Invoke-Application $application $arguments 30000 $shadow -CaptureModules
if ($run.ExitCode -ne 0 -or $run.Output -notmatch 'VALUE=18' -or $run.Output -notmatch 'RESOURCE=source-free resource' -or
    $run.Output -notmatch 'INPUT=caller input') {
    throw ("Source-free execution failed: " + $run.Output + $run.Error)
}
Assert-ConsoleLibraries $run.Modules
Assert-PersistentOutput 'function-output.txt'
$argumentLine = @($run.Output -split '\r?\n' | Where-Object { $_.StartsWith('ARGUMENTS=') })[0]
$received = $argumentLine.Substring('ARGUMENTS='.Length) | ConvertFrom-Json
if ($received.Count -ne $arguments.Count) { throw 'Argument count changed.' }
for ($i = 0; $i -lt $arguments.Count; $i++) {
    if ($received[$i] -cne $arguments[$i]) { throw "Argument $i changed." }
}
$exit = Invoke-Application $application @('--exit-seven') 30000
if ($exit.ExitCode -ne 7) { throw "Exit status was $($exit.ExitCode), expected 7." }
$failure = Invoke-Application $application @('--fail') 30000
if ($failure.ExitCode -eq 0 -or $failure.Output -notmatch 'Deliberate application error') { throw 'Application error was not propagated.' }
$finalDirectories = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
if (Compare-Object $initialDirectories $finalDirectories) { throw 'Normal return, exit or error left an extraction directory.' }
Write-Output $run.Output
Write-Output 'PASS native packaging, embedded bytecode, relocation, source-free execution, arguments, exit status, errors'
Write-Output "ARTIFACT=$application"

$installedApplication = Join-Path $relocated 'installed.exe'
$sharedRuntime = Join-Path $relocated 'application.runtime'
$sharedBinaries = Join-Path $sharedRuntime ('bin/' + $platform)
$installedRoot = Split-Path -Parent (Split-Path -Parent $runtimePath)
$badInstallation = Join-Path $root 'incompatible installation'
$badBinaries = Join-Path $badInstallation ('bin/' + $platform)
$badFunctions = Join-Path $badInstallation 'modules/interpreter/functions'
New-Item -ItemType Directory -Path $badBinaries, $badFunctions | Out-Null
[IO.File]::WriteAllText((Join-Path $badInstallation 'modules/modules.m'), 'error(''must not execute'');')
[IO.File]::WriteAllText((Join-Path $badFunctions '__nelsonc_run__.m'), 'error(''must not execute'');')
[IO.File]::WriteAllText((Join-Path $badBinaries 'libnlsEngine.dll'), 'must not load')
[IO.File]::WriteAllText((Join-Path $badBinaries 'libnlsInterpreter.dll'), 'different engine build')
$beforeInstalled = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
$searchCases = @(
    @{ NELSONC_RUNTIME_ROOT = $installedRoot; PATH = '' },
    @{ NELSONC_RUNTIME_ROOT = $sharedRuntime; PATH = '' },
    @{ NELSON_RUNTIME_PATH = $sharedBinaries; PATH = '' },
    @{ NELSON_RUNTIME_PATH = ''; PATH = $sharedBinaries + '\' },
    @{ NELSON_RUNTIME_PATH = $badBinaries; PATH = $badBinaries + ';"' + $sharedBinaries + '"' }
)
foreach ($environment in $searchCases) {
    $installedRun = Invoke-Application $installedApplication $arguments 180000 $shadow $environment
    if ($installedRun.ExitCode -ne 0 -or $installedRun.Output -notmatch 'VALUE=18' -or
        $installedRun.Output -notmatch 'RESOURCE=source-free resource' -or $installedRun.Output -notmatch 'INPUT=caller input') {
        throw ("Installed runtime resolution failed for " + ($environment | ConvertTo-Json -Compress) + ': ' + $installedRun.Output + $installedRun.Error)
    }
    $argumentLine = @($installedRun.Output -split '\r?\n' | Where-Object { $_.StartsWith('ARGUMENTS=') })[0]
    $received = $argumentLine.Substring('ARGUMENTS='.Length) | ConvertFrom-Json
    if ($received.Count -ne $arguments.Count) { throw 'Installed-runtime argument count changed.' }
    for ($i = 0; $i -lt $arguments.Count; $i++) {
        if ($received[$i] -cne $arguments[$i]) { throw "Installed-runtime argument $i changed." }
    }
    Assert-PersistentOutput 'function-output.txt'
}
$failureCases = @(
    @{ Environment = @{ PATH = '' }; Message = 'No compatible installed Nelson runtime found' },
    @{ Environment = @{ PATH = (Join-Path $sharedRuntime 'nonexistent') }; Message = 'No compatible installed Nelson runtime found' },
    @{ Environment = @{ PATH = [IO.Path]::GetRelativePath($root, $sharedBinaries) }; Message = 'No compatible installed Nelson runtime found' },
    @{ Environment = @{ NELSONC_RUNTIME_ROOT = (Join-Path $root 'absent'); PATH = $sharedBinaries }; Message = 'installation not found' },
    @{ Environment = @{ NELSONC_RUNTIME_ROOT = '.'; PATH = $sharedBinaries }; Message = 'must be an absolute' },
    @{ Environment = @{ NELSONC_RUNTIME_ROOT = $badInstallation; NELSON_RUNTIME_PATH = $sharedBinaries }; Message = 'Incompatible installed Nelson runtime' }
)
foreach ($case in $failureCases) {
    $rejected = Invoke-Application $installedApplication @() 30000 '' $case.Environment
    if ($rejected.ExitCode -ne 2 -or $rejected.Error -notmatch $case.Message -or $rejected.Output -match 'VALUE=') {
        throw ("Installed-runtime rejection failed: " + $rejected.Output + $rejected.Error)
    }
}
$corruptApplication = Join-Path $relocated 'incompatible.exe'
$corruptBytes = [IO.File]::ReadAllBytes($installedApplication)
$fingerprintOffset = $corruptBytes.Length - 352 + 96
$corruptBytes[$fingerprintOffset] = if ($corruptBytes[$fingerprintOffset] -eq 97) { 98 } else { 97 }
[IO.File]::WriteAllBytes($corruptApplication, $corruptBytes)
$rejected = Invoke-Application $corruptApplication @() 30000 '' @{ NELSONC_RUNTIME_ROOT = $sharedRuntime }
if ($rejected.ExitCode -ne 2 -or $rejected.Error -notmatch 'Incompatible installed Nelson runtime') {
    throw 'Installed runtime engine fingerprint was not enforced before loading.'
}
$installedExit = Invoke-Application $installedApplication @('--exit-seven') 30000 '' @{ NELSONC_RUNTIME_ROOT = $sharedRuntime }
$installedFailure = Invoke-Application $installedApplication @('--fail') 30000 '' @{ NELSONC_RUNTIME_ROOT = $sharedRuntime }
if ($installedExit.ExitCode -ne 7 -or $installedFailure.ExitCode -eq 0 -or $installedFailure.Output -notmatch 'Deliberate application error') {
    throw 'Installed-runtime exit status or error was lost.'
}
$bundledIgnore = Invoke-Application $application @('--exit-seven') 30000 '' @{ NELSONC_RUNTIME_ROOT = $badInstallation }
if ($bundledIgnore.ExitCode -ne 7) { throw 'Installed runtime selection changed bundled mode.' }
$afterInstalled = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
if (Compare-Object $beforeInstalled $afterInstalled) { throw 'Installed-runtime launch left an extraction directory.' }
$null = Read-CheckedRuntime $sharedRuntime
Write-Output 'PASS installed runtime without redistributed libraries, explicit root, installer environment, PATH, exact engine rejection and unchanged shared runtime'
Write-Output "INSTALLED_ARTIFACT=$installedApplication"

$scriptSource = Join-Path $root 'script-source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_script') -Destination $scriptSource -Recurse
$scriptEntry = Join-Path $scriptSource 'script_entry.m'
$scriptApplication = Join-Path $relocated 'script.exe'
$scriptBuild = Invoke-Application $compiler @($scriptEntry, '-o', $scriptApplication)
if ($scriptBuild.ExitCode -ne 0) { throw ("Script packaging failed: " + $scriptBuild.Output + $scriptBuild.Error) }
$null = Read-CheckedRuntime (Join-Path $relocated 'script.runtime')
$scriptSourceFull = [IO.Path]::GetFullPath($scriptSource)
if (-not $scriptSourceFull.StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid script source path.' }
Remove-Item -LiteralPath $scriptSourceFull -Recurse -Force
$beforeDirectories = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
$scriptRun = Invoke-Application $scriptApplication @('script argument', '--script-option') 180000 $shadow
$afterDirectories = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
if ($scriptRun.ExitCode -ne 0 -or $scriptRun.Output -notmatch 'SCRIPT_VALUE=36') {
    throw ("Script execution failed: " + $scriptRun.Output + $scriptRun.Error)
}
$scriptArgumentLine = @($scriptRun.Output -split '\r?\n' | Where-Object { $_.StartsWith('SCRIPT_ARGUMENTS=') })[0]
$scriptArguments = $scriptArgumentLine.Substring('SCRIPT_ARGUMENTS='.Length) | ConvertFrom-Json
if ($scriptArguments.Count -ne 2 -or $scriptArguments[0] -cne 'script argument' -or $scriptArguments[1] -cne '--script-option') {
    throw 'Script arguments changed.'
}
if (Compare-Object $beforeDirectories $afterDirectories) { throw 'Script clear bypassed extraction cleanup.' }
Assert-PersistentOutput 'script-output.txt'
Write-Output 'PASS minimal runtime inventory, unchanged native libraries, notices, script entry and clear isolation'
Write-Output "SCRIPT_ARTIFACT=$scriptApplication"

$packageSource = Join-Path $root 'package-source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_package') -Destination $packageSource -Recurse
$packageEntry = Join-Path $packageSource '+ncouter/+ncinner/package_entry.m'
$packageDistribution = Join-Path $root 'package-distribution'
$packageApplication = Join-Path $packageDistribution 'package.exe'
$packageBuild = Invoke-Application $compiler @($packageEntry, '-o', $packageApplication)
if ($packageBuild.ExitCode -ne 0) { throw ("Package entry build failed: " + $packageBuild.Output + $packageBuild.Error) }
$null = Read-CheckedRuntime (Join-Path $packageDistribution 'package.runtime')
$packageArchive = Read-EmbeddedApplication $packageApplication
if ($packageArchive.Manifest.entryName -cne 'ncouter.ncinner.package_entry') { throw 'Qualified entry name lost.' }
if (@($packageArchive.Names | Where-Object { $_.EndsWith('.nbc') }).Count -ne 2 -or
    @($packageArchive.Names | Where-Object { $_.EndsWith('.m') }).Count -ne 0) { throw 'Unexpected package archive.' }
$packageSourceFull = [IO.Path]::GetFullPath($packageSource)
$packageDistributionFull = [IO.Path]::GetFullPath($packageDistribution)
if (-not $packageSourceFull.StartsWith($root + [IO.Path]::DirectorySeparatorChar) -or
    -not $packageDistributionFull.StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid package test paths.' }
Remove-Item -LiteralPath $packageSourceFull -Recurse -Force
$packageRelocated = Join-Path $root 'relocated package'
Move-Item -LiteralPath $packageDistributionFull -Destination $packageRelocated
$packageApplication = Join-Path $packageRelocated 'package.exe'
$packageRun = Invoke-Application $packageApplication @('package argument', '--package-option') 180000 $shadow
if ($packageRun.ExitCode -ne 0 -or $packageRun.Output -notmatch 'PACKAGE_VALUE=28') {
    throw ("Qualified entry execution failed: " + $packageRun.Output + $packageRun.Error)
}
$packageArgumentLine = @($packageRun.Output -split '\r?\n' | Where-Object { $_.StartsWith('PACKAGE_ARGUMENTS=') })[0]
$packageArguments = $packageArgumentLine.Substring('PACKAGE_ARGUMENTS='.Length) | ConvertFrom-Json
if ($packageArguments.Count -ne 2 -or $packageArguments[0] -cne 'package argument' -or
    $packageArguments[1] -cne '--package-option') { throw 'Package entry arguments changed.' }
Write-Output 'PASS nested package entry, embedded bytecode and relocated source-free execution'
Assert-PersistentOutput 'package-output.txt'
Write-Output 'PASS preserved invocation directory, persistent outputs, embedded resources and implicit source isolation'
Write-Output "PACKAGE_ARTIFACT=$packageApplication"

$classSource = Join-Path $root 'class-source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_class') -Destination $classSource -Recurse
$classEntry = Join-Path $classSource 'class_entry.m'
$classExplain = Invoke-Application $compiler @($classEntry, '--explain-link')
if ($classExplain.ExitCode -ne 0) { throw ("Class plan failed: " + $classExplain.Output + $classExplain.Error) }
$classPlan = $classExplain.Output | ConvertFrom-Json
if (-not $classPlan.complete -or $classPlan.dynamicFallback -or $classPlan.files.Count -ne 8) {
    throw 'Class dependency closure was not resolved precisely.'
}
$classDistribution = Join-Path $root 'class-distribution'
$classApplication = Join-Path $classDistribution 'classes.exe'
$legacyApplication = Join-Path $classDistribution 'legacy.exe'
$legacyEntry = Join-Path $classSource 'legacy_entry.m'
$legacyBuild = Invoke-Application $compiler @($legacyEntry, '-o', $legacyApplication)
if ($legacyBuild.ExitCode -ne 0) { throw ("Legacy class build failed: " + $legacyBuild.Output + $legacyBuild.Error) }
$null = Read-CheckedRuntime (Join-Path $classDistribution 'legacy.runtime')
$legacyArchive = Read-EmbeddedApplication $legacyApplication
if (@($legacyArchive.Names | Where-Object { $_.EndsWith('.nbc') }).Count -ne 4 -or
    @($legacyArchive.Names | Where-Object { $_.EndsWith('.m') }).Count -ne 0) { throw 'Unexpected legacy class archive.' }
$classBuild = Invoke-Application $compiler @($classEntry, '-o', $classApplication)
if ($classBuild.ExitCode -ne 0) { throw ("Class application build failed: " + $classBuild.Output + $classBuild.Error) }
$null = Read-CheckedRuntime (Join-Path $classDistribution 'classes.runtime')
$classArchive = Read-EmbeddedApplication $classApplication
if (@($classArchive.Names | Where-Object { $_.EndsWith('.nbc') }).Count -ne 5 -or
    @($classArchive.Names | Where-Object { $_.EndsWith('.m') }).Count -ne 3) { throw 'Unexpected class application archive.' }
foreach ($required in @('+ncclasses/NcFactory.m', '+ncclasses/NcBase.m', '@NcDerived/NcDerived.m',
    '@NcDerived/externalValue.nbc', '@NcDerived/private/nc_class_private.nbc')) {
    if (@($classArchive.Names | Where-Object { $_.EndsWith('/' + $required) }).Count -ne 1) {
        throw "Missing embedded class dependency: $required"
    }
}
$classSourceFull = [IO.Path]::GetFullPath($classSource)
$classDistributionFull = [IO.Path]::GetFullPath($classDistribution)
$classRelocated = [IO.Path]::GetFullPath((Join-Path $root 'relocated classes'))
foreach ($checked in @($classSourceFull, $classDistributionFull, $classRelocated)) {
    if (-not $checked.StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid class test path.' }
}
Remove-Item -LiteralPath $classSourceFull -Recurse -Force
Move-Item -LiteralPath $classDistributionFull -Destination $classRelocated
$classApplication = Join-Path $classRelocated 'classes.exe'
$beforeDirectories = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
$classRun = Invoke-Application $classApplication @() 180000 $shadow
$legacyApplication = Join-Path $classRelocated 'legacy.exe'
$legacyRun = Invoke-Application $legacyApplication @() 180000 $shadow
$afterDirectories = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
if ($classRun.ExitCode -ne 0 -or $classRun.Output -notmatch 'CLASS_VALUE=20') {
    throw ("Relocated class application failed: " + $classRun.Output + $classRun.Error)
}
if ($legacyRun.ExitCode -ne 0 -or $legacyRun.Output -notmatch 'LEGACY_VALUE=21') {
    throw ("Relocated legacy class failed: " + $legacyRun.Output + $legacyRun.Error)
}
if (Compare-Object $beforeDirectories $afterDirectories) { throw 'Class application left an extraction directory.' }
Write-Output 'PASS packaged classes, static constructor factory, inheritance, external bytecode methods and private helpers'
Write-Output "CLASS_ARTIFACT=$classApplication"
Write-Output 'PASS legacy class constructor, method and private helper entirely embedded as bytecode'
Write-Output "LEGACY_ARTIFACT=$legacyApplication"
Write-Output 'PASS exact runtime inventories and SHA-256 digests for all five applications'

$graphicsSource = Join-Path $root 'graphics-source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_graphics') -Destination $graphicsSource -Recurse
$graphicsEntry = Join-Path $graphicsSource 'graphics_entry.m'
$graphicsDistribution = Join-Path $root 'graphics-distribution'
$graphicsApplication = Join-Path $graphicsDistribution 'graphics.exe'
$graphicsBuild = Invoke-Application $compiler @($graphicsEntry, '-o', $graphicsApplication)
if ($graphicsBuild.ExitCode -ne 0) { throw ("Automatic GUI packaging failed: " + $graphicsBuild.Output + $graphicsBuild.Error) }
$graphicsArchive = Read-EmbeddedApplication $graphicsApplication
if (-not $graphicsArchive.Manifest.gui -or @($graphicsArchive.Names | Where-Object { $_.EndsWith('.m') }).Count -ne 0) {
    throw 'Graphical application was not embedded as GUI bytecode.'
}
$graphicsRuntime = Join-Path $graphicsDistribution 'graphics.runtime'
$graphicsInventory = Read-CheckedRuntime $graphicsRuntime
foreach ($required in @('graphics', 'gui')) {
    if ($graphicsInventory.modules -notcontains $required) { throw "Missing graphical module: $required" }
}
foreach ($excluded in @('qml_engine', 'text_editor')) {
    if ($graphicsInventory.modules -contains $excluded) { throw "Unneeded graphical module: $excluded" }
}
$graphicsBinaries = Join-Path $graphicsRuntime ('bin/' + $platform)
if (-not (Test-Path -LiteralPath (Join-Path $graphicsBinaries 'plugins/platforms/qwindows.dll'))) { throw 'Missing Qt platform plugin.' }
foreach ($excluded in @('plugins/designer', 'qml', 'kernel32.dll', 'kernelbase.dll', 'msvcrt.dll')) {
    if (Test-Path -LiteralPath (Join-Path $graphicsBinaries $excluded)) { throw "Non-runtime dependency copied: $excluded" }
}
$graphicsInstalled = Join-Path $graphicsDistribution 'graphics-installed.exe'
$graphicsInstalledBuild = Invoke-Application $compiler @($graphicsEntry, '-o', $graphicsInstalled, '--runtime', 'installed')
if ($graphicsInstalledBuild.ExitCode -ne 0) { throw ($graphicsInstalledBuild.Output + $graphicsInstalledBuild.Error) }
if (Test-Path -LiteralPath (Join-Path $graphicsDistribution 'graphics-installed.runtime')) { throw 'Installed GUI mode copied runtime.' }
$graphicsInstalledArchive = Read-EmbeddedApplication $graphicsInstalled
if (-not $graphicsInstalledArchive.Manifest.gui) { throw 'Installed GUI mode lost its window lifetime.' }
$graphicsRejected = Invoke-Application $compiler @($graphicsEntry, '--cli', '-o', (Join-Path $root 'rejected-graphics.exe'))
if ($graphicsRejected.ExitCode -eq 0) { throw 'Explicit CLI mode accepted unresolved graphical functions.' }
[IO.File]::WriteAllText((Join-Path $root '--cli'), 'resource with an option-like name')
$graphicsOptionValue = Invoke-Application $compiler @($graphicsEntry, '-a', '--cli', '--runtime', 'installed', '--explain-link')
if ($graphicsOptionValue.ExitCode -ne 0 -or -not ($graphicsOptionValue.Output | ConvertFrom-Json).runtime.gui) {
    throw ('An option value changed compiler mode: ' + $graphicsOptionValue.Output + $graphicsOptionValue.Error)
}
$graphicsRelocated = [IO.Path]::GetFullPath((Join-Path $root 'relocated graphics'))
foreach ($checked in @($graphicsSource, $graphicsDistribution, $graphicsRelocated)) {
    if (-not [IO.Path]::GetFullPath($checked).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid graphical test path.' }
}
Remove-Item -LiteralPath $graphicsSource -Recurse -Force
Move-Item -LiteralPath $graphicsDistribution -Destination $graphicsRelocated
$graphicsApplication = Join-Path $graphicsRelocated 'graphics.exe'
$graphicsInstalled = Join-Path $graphicsRelocated 'graphics-installed.exe'
$graphicsRuntime = Join-Path $graphicsRelocated 'graphics.runtime'
$graphicsBinaries = Join-Path $graphicsRuntime ('bin/' + $platform)
$beforeGraphics = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
foreach ($mode in @('batch', 'offscreen', 'hide', 'lifetime')) {
    $graphicsRun = Invoke-Application $graphicsApplication @($mode) 30000 '' @{ PATH = ''; QT_PLUGIN_PATH = 'invalid' } -CaptureModules
    if ($graphicsRun.ExitCode -ne 0 -or $graphicsRun.Output -notmatch 'GUI_RENDER_OK') {
        throw ("Relocated graphics failed ($mode): " + $graphicsRun.Output + $graphicsRun.Error)
    }
    $marker = switch ($mode) {
        'offscreen' { 'GUI_OFFSCREEN_RETURN' }
        'hide' { 'GUI_ENTRY_RETURN[\s\S]*GUI_HIDDEN_OK' }
        'lifetime' { 'GUI_ENTRY_RETURN[\s\S]*GUI_NEXT_WINDOW[\s\S]*GUI_LIFETIME_OK' }
        default { 'GUI_RENDER_OK' }
    }
    if ($graphicsRun.Output -notmatch $marker) { throw "Graphical window lifetime failed ($mode): $($graphicsRun.Output)" }
    foreach ($required in @('Qt6Core.dll', 'Qt6Gui.dll', 'Qt6Widgets.dll', 'plugins/platforms/qwindows.dll')) {
        if ($graphicsRun.Modules -notcontains (Join-Path $graphicsBinaries $required)) { throw "Relocated library was not observed: $required" }
    }
    $foreignQt = @($graphicsRun.Modules | Where-Object { [IO.Path]::GetFileName($_) -like 'Qt6*.dll' -and -not $_.StartsWith($graphicsBinaries + '\', [StringComparison]::OrdinalIgnoreCase) })
    if ($foreignQt.Count) { throw "Host Qt libraries leaked into application: $foreignQt" }
}
foreach ($selectedRuntime in @($graphicsRuntime, $installedRoot)) {
    $installedGuiRun = Invoke-Application $graphicsInstalled @('lifetime') 30000 '' @{ NELSONC_RUNTIME_ROOT = $selectedRuntime; PATH = '' }
    if ($installedGuiRun.ExitCode -ne 0 -or $installedGuiRun.Output -notmatch 'GUI_ENTRY_RETURN[\s\S]*GUI_NEXT_WINDOW[\s\S]*GUI_LIFETIME_OK') {
        throw ('Installed graphical runtime failed: ' + $installedGuiRun.Output + $installedGuiRun.Error)
    }
}
$afterGraphics = @(Get-ChildItem -LiteralPath $root -Directory | ForEach-Object FullName)
if (Compare-Object $beforeGraphics $afterGraphics) { throw 'Graphical application left an extraction directory.' }
if (-not (Test-Path -LiteralPath (Join-Path $root 'packaged-graphics.png'))) { throw 'Missing graphical output.' }
$null = Read-CheckedRuntime $graphicsRuntime
Write-Output 'PASS automatic GUI selection, explicit CLI mode, runtime-only Qt plugins and relocated native rendering'
Write-Output 'PASS invisible and hidden-handle windows, timer callbacks, successive windows and installed GUI runtime'
Write-Output "GRAPHICS_ARTIFACT=$graphicsApplication"
Write-Output "GRAPHICS_INSTALLED_ARTIFACT=$graphicsInstalled"
. (Join-Path $PSScriptRoot 'test_nelsonc_callbacks.ps1')
if ($NativeLibraries) { . (Join-Path $PSScriptRoot 'test_nelsonc_native.ps1') }
