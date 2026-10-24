#Requires -Version 7.0
param([Parameter(Mandatory = $true)][string]$Runtime)
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$runtimePath = (Resolve-Path -LiteralPath $Runtime).Path
$runtimeRoot = [IO.Path]::GetFullPath((Join-Path $runtimePath '../..'))
$root = Join-Path $repo ('temp/engine-web-backend-' + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $root | Out-Null
$common = @('--quiet', '--language', 'en_US', '--no-ipc', '--no-user-startup', '--no-user-modules', '--timeout', '60')

function Invoke-Engine([string]$Directory, [string]$Launcher, [string[]]$Arguments, [string]$Name) {
    $info = [Diagnostics.ProcessStartInfo]::new()
    $info.FileName = Join-Path $Directory $Launcher
    $info.WorkingDirectory = $root
    $info.UseShellExecute = $false
    $info.CreateNoWindow = $true
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
    $info.Environment['PATH'] = "$Directory;$env:SystemRoot/System32;$env:SystemRoot"
    foreach ($argument in $common + $Arguments) { $info.ArgumentList.Add($argument) }
    $loaded = [Collections.Generic.HashSet[string]]::new([StringComparer]::OrdinalIgnoreCase)
    $watch = [Diagnostics.Stopwatch]::StartNew()
    $process = [Diagnostics.Process]::Start($info)
    try {
        $stdout = $process.StandardOutput.ReadToEndAsync()
        $stderr = $process.StandardError.ReadToEndAsync()
        while (-not $process.HasExited -and $watch.ElapsedMilliseconds -lt 90000) {
            try {
                $process.Refresh()
                foreach ($module in $process.Modules) { $null = $loaded.Add($module.FileName) }
            } catch {
                if (-not $process.HasExited) { throw }
            }
            $null = $process.WaitForExit(25)
        }
        if (-not $process.HasExited) { throw "Engine timeout: $Name" }
        $result = [pscustomobject]@{
            Name = $Name; ExitCode = $process.ExitCode; Output = $stdout.Result
            Error = $stderr.Result; Modules = @($loaded | Sort-Object)
        }
        $result | ConvertTo-Json -Depth 4 | Set-Content -LiteralPath (Join-Path $root "$Name.json")
        return $result
    } finally {
        if (-not $process.HasExited) {
            $process.Kill($true)
            $process.WaitForExit()
        }
        $process.Dispose()
    }
}

function Assert-Exit($Result, [int]$Expected) {
    if ($Result.ExitCode -ne $Expected) {
        throw "$($Result.Name): exit $($Result.ExitCode), expected $Expected. $($Result.Output) $($Result.Error)"
    }
    if ($Expected -eq 42 -and $Result.Error.Trim().Length) {
        throw "$($Result.Name): unexpected startup or shutdown diagnostic: $($Result.Error)"
    }
}

function Assert-Backend($Result, [bool]$Expected) {
    $backend = @($Result.Modules | Where-Object { [IO.Path]::GetFileName($_) -ieq 'libnlsWeb_gui.dll' })
    if ([bool]$backend.Count -ne $Expected) {
        throw "$($Result.Name): unexpected Web GUI backend load state."
    }
}

$script = (Join-Path $PSScriptRoot 'test_engine_optional_web_backend.m').Replace('\', '/').Replace("'", "''")
$cli = Invoke-Engine $runtimePath 'NelSon-cli.exe' @('-e', "try; run('$script'); pause(0.5); exit(42); catch ex; disp(ex.message); exit(1); end") 'console'
Assert-Exit $cli 42
Assert-Backend $cli $false
Write-Output 'PASS console: no engine Web imports or loaded Web GUI backend'

# These binaries and libraries are independent copies, never the installed runtime.
$minimal = Join-Path $root 'minimal'
$minimalBin = Join-Path $minimal 'bin/x64'
New-Item -ItemType Directory -Path $minimalBin -Force | Out-Null
foreach ($path in $cli.Modules) {
    if ($path.StartsWith($runtimePath + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase)) {
        Copy-Item -LiteralPath $path -Destination $minimalBin
    }
}
foreach ($name in @('NelSon-cli.exe', 'nelson-webview.exe')) {
    Copy-Item -LiteralPath (Join-Path $runtimePath $name) -Destination $minimalBin -Force
}
foreach ($name in @('etc', 'modules', 'resources', 'locale')) {
    $source = Join-Path $runtimeRoot $name
    if (Test-Path -LiteralPath $source) {
        New-Item -ItemType Junction -Path (Join-Path $minimal $name) -Target $source | Out-Null
    }
}
$missing = Join-Path $minimalBin 'libnlsWeb_gui.dll'
if (Test-Path -LiteralPath $missing) { throw 'Minimal fixture unexpectedly contains the backend.' }
$stripped = Invoke-Engine $minimalBin 'NelSon-cli.exe' @('-e', 'assert(2 + 3 == 5); pause(0.5); exit(42)') 'console-without-backend'
Assert-Exit $stripped 42
Assert-Backend $stripped $false
foreach ($path in $stripped.Modules) {
    if ($path.StartsWith($runtimePath + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase)) {
        throw "Minimal runtime escaped to the build runtime: $path"
    }
}
Write-Output 'PASS isolated console without Web backend files'

$unavailable = Invoke-Engine $minimalBin 'nelson-webview.exe' @('--web', '--url', '127.0.0.1', '-e', 'exit(42)') 'missing-backend'
Assert-Exit $unavailable 1
if (($unavailable.Output + $unavailable.Error) -notmatch 'Cannot load Web GUI backend') {
    throw 'Missing Web backend diagnostic not reported.'
}
Copy-Item -LiteralPath (Join-Path $minimalBin 'libnlsCommons.dll') -Destination $missing
$incompatible = Invoke-Engine $minimalBin 'nelson-webview.exe' @('--web', '--url', '127.0.0.1', '-e', 'exit(42)') 'missing-symbol'
Assert-Exit $incompatible 1
if (($incompatible.Output + $incompatible.Error) -notmatch 'Missing Web GUI backend function: NelsonWebGuiCreateEvaluator') {
    throw 'Missing Web backend symbol diagnostic not reported.'
}
Write-Output 'PASS explicit Web launch: missing backend and incompatible export diagnostics'

foreach ($launcher in @('NelSon-adv-cli.exe', 'NelSon-gui.exe')) {
    $name = [IO.Path]::GetFileNameWithoutExtension($launcher)
    $marker = (Join-Path $root "$name.marker").Replace('\', '/').Replace("'", "''")
    $command = "f = figure('visible', 'off'); h = uicontrol(f, 'style', 'pushbutton', 'string', 'Ready'); assert(isgraphics(h)); drawnow; close(f); filewrite('$marker', 'ready'); pause(0.5); exit(42)"
    $gui = Invoke-Engine $runtimePath $launcher @('-e', $command) $name
    Assert-Exit $gui 42
    Assert-Backend $gui $false
    if (-not (Test-Path -LiteralPath $marker)) { throw "Missing graphical completion: $name" }
    Write-Output "PASS ${name}: figure and uicontrol without Web backend"
}

$marker = (Join-Path $root 'web-figures.marker').Replace('\', '/').Replace("'", "''")
$command = "f = figure('visible', 'off'); h = uicontrol(f, 'style', 'pushbutton', 'string', 'Ready'); assert(isgraphics(h)); drawnow; close(f); filewrite('$marker', 'ready'); pause(0.5); exit(42)"
$figures = Invoke-Engine $runtimePath 'NelSon-adv-cli.exe' @('--webview', '-e', $command) 'web-figures'
Assert-Exit $figures 42
Assert-Backend $figures $true
if (-not (Test-Path -LiteralPath $marker)) { throw 'Web figures evaluator did not execute its command.' }
Write-Output 'PASS advanced terminal Web graphics, offscreen figure and uicontrol'

foreach ($index in 1..3) {
    $marker = (Join-Path $root "web-$index.marker").Replace('\', '/').Replace("'", "''")
    $command = "assert(strcmp(getwebmode(), 'server')); [u,p] = getweburl(); assert(p > 0); filewrite('$marker', 'ready'); pause(0.5); exit(42)"
    $web = Invoke-Engine $runtimePath 'nelson-webview.exe' @('--web', '--url', '127.0.0.1', '-e', $command) "web-$index"
    Assert-Exit $web 42
    Assert-Backend $web $true
    if (-not (Test-Path -LiteralPath $marker)) { throw 'Web evaluator did not execute its command.' }
}
Write-Output 'PASS repeated Web server startup, command execution, event pumping and exit'
Write-Output "Evidence: $root"
