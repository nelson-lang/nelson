#Requires -Version 7.0
param(
    [Parameter(Mandatory = $true)][string]$Runtime,
    [string]$BeforeRuntime = '',
    [ValidateRange(1, 30)][int]$Rounds = 9,
    [ValidateRange(1, 20)][int]$Samples = 3,
    [ValidateRange(1, 64)][int]$Threads = 1,
    [long]$Affinity = 1,
    [ValidateSet('scalar','crossfile','builtin','matrix','matrixwide')][string[]]$Kinds = @('scalar','crossfile','builtin','matrix'),
    [switch]$IncludeInstalled,
    [switch]$IncludeControl,
    [string]$InstalledRuntimeRoot = ''
)
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'nelsonc_performance_provenance.ps1')
if ($Affinity -ne 0) {
    $processors = @([Convert]::ToString($Affinity, 2).ToCharArray() | Where-Object { $_ -eq '1' }).Count
    if ($processors -lt $Threads) { throw 'The affinity mask must allow at least Threads logical processors; use Affinity 0 for an unrestricted run.' }
}
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$root = Join-Path $repo ('temp/nelsonc-performance-' + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $root | Out-Null
$current = (Resolve-Path -LiteralPath $Runtime).Path
$source = Join-Path $root 'source'
Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_performance') -Destination $source -Recurse
$entry = Join-Path $source 'nc_perf_entry.m'
$application = Join-Path $root 'distribution/performance.exe'
$installedApplication = Join-Path $root 'distribution/installed.exe'
$installedRoot = Split-Path -Parent (Split-Path -Parent $current)
if ($InstalledRuntimeRoot) {
    if (-not $IncludeInstalled) { throw 'InstalledRuntimeRoot requires IncludeInstalled.' }
    $installedRoot = (Resolve-Path -LiteralPath $InstalledRuntimeRoot).Path
}
$script:processNumber = 0
$records = [Collections.Generic.List[object]]::new()

function Median([double[]]$Values) {
    $ordered = @($Values | Sort-Object)
    if ($ordered.Count -eq 0) { throw 'Empty measurement.' }
    $middle = [int][Math]::Floor($ordered.Count / 2)
    if ($ordered.Count % 2) { return $ordered[$middle] }
    return ($ordered[$middle - 1] + $ordered[$middle]) / 2
}

function Invoke-MeasuredProcess([string]$Executable, [string[]]$Arguments, [string]$Variant) {
    $info = [Diagnostics.ProcessStartInfo]::new()
    $info.FileName = $Executable
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
    $info.Environment['NELSON_PREFERENCES_PATH'] = Join-Path $root ('preferences/' + $Variant)
    if ($Variant -eq 'installed') { $info.Environment['NELSONC_RUNTIME_ROOT'] = $installedRoot }
    foreach ($name in @('OMP_NUM_THREADS','MKL_NUM_THREADS','OPENBLAS_NUM_THREADS')) { $info.Environment[$name] = [string]$Threads }
    $info.Environment['OMP_DYNAMIC'] = 'FALSE'
    $info.Environment['MKL_DYNAMIC'] = 'FALSE'
    foreach ($argument in $Arguments) { $info.ArgumentList.Add($argument) }
    $clock = [Diagnostics.Stopwatch]::StartNew()
    $process = [Diagnostics.Process]::Start($info)
    if ($Affinity -ne 0) { $process.ProcessorAffinity = [IntPtr]$Affinity }
    $stdout = $process.StandardOutput.ReadToEndAsync()
    $stderr = $process.StandardError.ReadToEndAsync()
    if (-not $process.WaitForExit(180000)) {
        $process.Kill($true)
        $process.WaitForExit()
        throw "Benchmark timeout: $Variant"
    }
    $clock.Stop()
    $result = [pscustomobject]@{
        ExitCode=$process.ExitCode
        Output=$stdout.Result
        Error=$stderr.Result
        Seconds=$clock.Elapsed.TotalSeconds
        ProcessCPU=$process.TotalProcessorTime.TotalSeconds
    }
    $process.Dispose()
    $script:processNumber++
    $prefix = Join-Path $root ('{0:D3}-{1}' -f $script:processNumber, $Variant)
    $result.Output | Set-Content -LiteralPath ($prefix + '.out.log') -Encoding utf8
    $result.Error | Set-Content -LiteralPath ($prefix + '.err.log') -Encoding utf8
    if ($result.ExitCode -ne 0 -or $result.Output -match 'Main startup.m failed|Main finish.m failed|Nelson cannot load base modules') {
        throw ("Benchmark process failed: " + $result.Output + $result.Error)
    }
    return $result
}

function Invoke-Workload([object]$Variant, [string]$Kind, [int]$Count) {
    if ($Variant.Name -eq 'packaged') {
        return Invoke-MeasuredProcess $application @($Kind, [string]$Count, [string]$Samples, [string]$Threads) $Variant.Name
    }
    if ($Variant.Name -eq 'installed') {
        return Invoke-MeasuredProcess $installedApplication @($Kind, [string]$Count, [string]$Samples, [string]$Threads) $Variant.Name
    }
    $path = $source.Replace('\','/').Replace("'","''")
    $command = "try; addpath('$path'); nc_perf_entry('$Kind','$Count','$Samples','$Threads'); exit(0); catch ex; disp(ex.message); exit(1); end"
    $arguments = @('--quiet','--no-user-startup','--no-user-modules','--no-ipc','--without-file-watcher','--execute',$command)
    return Invoke-MeasuredProcess (Join-Path $Variant.Runtime 'NelSon-cli.exe') $arguments $Variant.Name
}

function Paired-Summary([string]$Kind, [string]$Numerator, [string]$Denominator, [string]$Metric) {
    $ratios = @()
    foreach ($round in 1..$Rounds) {
        $top = @($records | Where-Object { $_.Kind -eq $Kind -and $_.Variant -eq $Numerator -and $_.Round -eq $round })[0]
        $bottom = @($records | Where-Object { $_.Kind -eq $Kind -and $_.Variant -eq $Denominator -and $_.Round -eq $round })[0]
        $a = if ($Metric -eq 'process') { $top.ProcessSeconds } else { Median $top.Measurement.$Metric }
        $b = if ($Metric -eq 'process') { $bottom.ProcessSeconds } else { Median $bottom.Measurement.$Metric }
        if ($a -le 0 -or $b -le 0) { throw 'Invalid timing measurement.' }
        $ratios += $a / $b
    }
    $logs = @($ratios | ForEach-Object { [Math]::Log($_) })
    $mean = ($logs | Measure-Object -Average).Average
    $random = [Random]::new(18437)
    $bootstrap = [double[]]::new(10000)
    for ($sample = 0; $sample -lt $bootstrap.Length; $sample++) {
        $sum = 0.0
        for ($k = 0; $k -lt $logs.Count; $k++) { $sum += $logs[$random.Next($logs.Count)] }
        $bootstrap[$sample] = [Math]::Exp($sum / $logs.Count)
    }
    [Array]::Sort($bootstrap)
    return [pscustomobject]@{
        Kind=$Kind; Numerator=$Numerator; Denominator=$Denominator; Metric=$Metric
        PairedRatios=$ratios; GeometricMean=[Math]::Exp($mean)
        Lower95=$bootstrap[250]; Upper95=$bootstrap[9749]; Exploratory=($Rounds -lt 7)
    }
}

function Get-PerformanceVariants([string]$Current, [string]$Before, [string]$Installed, [bool]$Control) {
    if ($Before) { [pscustomobject]@{ Name='before'; Runtime=(Resolve-Path -LiteralPath $Before).Path } }
    [pscustomobject]@{ Name='source'; Runtime=$Current }
    [pscustomobject]@{ Name='packaged'; Runtime='' }
    if ($Installed) { [pscustomobject]@{ Name='installed'; Runtime=$Installed } }
    if ($Control) { [pscustomobject]@{ Name='control'; Runtime=$Current } }
}
$installedBinary = if ($IncludeInstalled) { Join-Path $installedRoot ('bin/' + (Split-Path -Leaf $current)) } else { '' }
$variants = @(Get-PerformanceVariants $current $BeforeRuntime $installedBinary $IncludeControl.IsPresent)
$provenance = @()
$buildProvenance = @()
foreach ($variant in $variants | Where-Object Name -ne 'packaged') {
    $buildFile = [IO.Path]::GetFullPath((Join-Path $variant.Runtime '../../../build-provenance.json'))
    if (Test-Path -LiteralPath $buildFile) {
        $buildProvenance += [pscustomobject]@{ Variant=$variant.Name; Details=(Get-Content -LiteralPath $buildFile -Raw | ConvertFrom-Json) }
    }
    $hashes = @(Get-NelsoncPerformanceRuntimeLibraries $variant.Runtime)
    $provenance += [pscustomobject]@{ Variant=$variant.Name; Runtime=$variant.Runtime; Libraries=$hashes }
}
$runtimeDifferences = @()
if ($BeforeRuntime) {
    $beforeProof = @($buildProvenance | Where-Object Variant -eq 'before')
    $afterProof = @($buildProvenance | Where-Object Variant -eq 'source')
    $runtimeDifferences = @(Assert-NelsoncPerformanceRuntimePair `
        ($provenance | Where-Object Variant -eq 'before').Libraries `
        ($provenance | Where-Object Variant -eq 'source').Libraries `
        $(if ($beforeProof.Count) { $beforeProof[0].Details } else { $null }) `
        $(if ($afterProof.Count) { $afterProof[0].Details } else { $null }))
}
$compiler = Join-Path $current 'nelsonc.exe'
$explain = Invoke-MeasuredProcess $compiler @($entry, '--explain-link') 'compiler'
$plan = $explain.Output | ConvertFrom-Json
if (-not $plan.complete -or $plan.dynamicFallback) { throw 'Performance fixture dependency analysis is incomplete or unnecessarily dynamic.' }
$build = Invoke-MeasuredProcess $compiler @($entry, '-o', $application) 'compiler'
if ($IncludeInstalled) {
    $installedBuild = Invoke-MeasuredProcess $compiler @($entry, '-o', $installedApplication, '--runtime', 'installed') 'compiler'
    if (Test-Path -LiteralPath (Join-Path $root 'distribution/installed.runtime')) { throw 'Installed mode copied a runtime.' }
}
$inventoryFile = Join-Path $root 'distribution/performance.runtime/runtime.json'
$inventory = Get-Content -LiteralPath $inventoryFile -Raw | ConvertFrom-Json
$installedMatchesBundled = $false
if ($IncludeInstalled -and (Test-Path -LiteralPath (Join-Path $installedRoot 'runtime.json'))) {
    $externalInventory = Get-Content -LiteralPath (Join-Path $installedRoot 'runtime.json') -Raw | ConvertFrom-Json
    $expectedFiles = @($inventory.files) + @($inventory.generated)
    $externalFiles = @($externalInventory.files) + @($externalInventory.generated)
    $expectedKeys = @($expectedFiles | ForEach-Object { $_.relative + ':' + $_.sha256 })
    $externalKeys = @($externalFiles | ForEach-Object { $_.relative + ':' + $_.sha256 })
    if (-not (Compare-Object $expectedKeys $externalKeys)) {
        foreach ($file in $expectedFiles) {
            $target = [IO.Path]::GetFullPath((Join-Path $installedRoot $file.relative))
            if (-not $target.StartsWith($installedRoot + [IO.Path]::DirectorySeparatorChar) -or
                (Get-FileHash -LiteralPath $target).Hash -ine $file.sha256) { throw 'Installed runtime control file differs.' }
        }
        $actualFiles = @(Get-ChildItem -LiteralPath $installedRoot -Recurse -File | ForEach-Object {
            [IO.Path]::GetRelativePath($installedRoot, $_.FullName).Replace('\', '/')
        })
        if (Compare-Object (@($expectedFiles.relative) + @('runtime.json')) $actualFiles) { throw 'Installed runtime control inventory differs.' }
        $installedMatchesBundled = $true
    }
}
foreach ($library in $plan.runtime.libraries) {
    $copied = Join-Path $root ('distribution/performance.runtime/bin/x64/' + $library.filename)
    if ((Get-FileHash -LiteralPath $copied).Hash -ne (Get-FileHash -LiteralPath $library.source).Hash) {
        throw "Packaged library changed: $($library.filename)"
    }
}
$workloads = @(
    [pscustomobject]@{ Kind='scalar'; Count=6000000 },
    [pscustomobject]@{ Kind='crossfile'; Count=1000000 },
    [pscustomobject]@{ Kind='builtin'; Count=1500000 },
    [pscustomobject]@{ Kind='matrix'; Count=2000 },
    [pscustomobject]@{ Kind='matrixwide'; Count=48 }
)
$packagedRuntime = Join-Path $root 'distribution/performance.runtime/bin/x64'
$provenance += [pscustomobject]@{ Variant='packaged'; Runtime=$packagedRuntime; Libraries=@(Get-NelsoncPerformanceRuntimeLibraries $packagedRuntime) }
$workloads = @($workloads | Where-Object { $_.Kind -in $Kinds })
foreach ($round in 0..$Rounds) {
    for ($offset = 0; $offset -lt $variants.Count; $offset++) {
        $variant = $variants[($round + $offset) % $variants.Count]
        $run = Invoke-Workload $variant 'startup' 1
        if ($run.Output -notmatch 'NELSONC_STARTUP_OK') { throw ("Startup failure: " + $run.Output + $run.Error) }
        $record = [pscustomobject]@{ Round=$round; Kind='startup'; Variant=$variant.Name; ProcessSeconds=$run.Seconds; ProcessCPU=$run.ProcessCPU; Measurement=$null }
        $records.Add($record)
        $record | ConvertTo-Json -Compress -Depth 6 | Add-Content -LiteralPath (Join-Path $root 'measurements.jsonl') -Encoding utf8
    }
}
foreach ($round in 1..$Rounds) {
    foreach ($workload in $workloads) {
        $expected = $null
        for ($offset = 0; $offset -lt $variants.Count; $offset++) {
            $variant = $variants[($round + $offset) % $variants.Count]
            $run = Invoke-Workload $variant $workload.Kind $workload.Count
            $line = @($run.Output -split '\r?\n' | Where-Object { $_.StartsWith('NELSONC_PERF=') })[0]
            if (-not $line) { throw ("Missing workload result: " + $run.Output + $run.Error) }
            $measurement = $line.Substring('NELSONC_PERF='.Length) | ConvertFrom-Json
            if ($measurement.threads -ne $Threads -or $measurement.wall.Count -ne $Samples -or $measurement.cpu.Count -ne $Samples) { throw 'Invalid benchmark configuration.' }
            if ($null -eq $expected) { $expected = $measurement.result }
            if ($measurement.result -ne $expected) { throw 'Variants returned different numerical results.' }
            $record = [pscustomobject]@{ Round=$round; Kind=$workload.Kind; Variant=$variant.Name; ProcessSeconds=$run.Seconds; ProcessCPU=$run.ProcessCPU; Measurement=$measurement }
            $records.Add($record)
            $record | ConvertTo-Json -Compress -Depth 6 | Add-Content -LiteralPath (Join-Path $root 'measurements.jsonl') -Encoding utf8
            Write-Output ("ROUND={0} KIND={1} VARIANT={2} WALL={3:F6} CPU={4:F6}" -f $round,$workload.Kind,$variant.Name,(Median $measurement.wall),(Median $measurement.cpu))
        }
    }
}
foreach ($runtimeRecord in $provenance) {
    Assert-NelsoncPerformanceRuntimeUnchanged $runtimeRecord.Libraries @(Get-NelsoncPerformanceRuntimeLibraries $runtimeRecord.Runtime)
}
$summaries = @()
foreach ($kind in @('startup') + @($workloads.Kind)) {
    $metrics = if ($kind -eq 'startup') { @('process') } else { @('wall','cpu') }
    foreach ($metric in $metrics) {
        $summaries += Paired-Summary $kind 'packaged' 'source' $metric
        if ($IncludeInstalled) { $summaries += Paired-Summary $kind 'installed' 'packaged' $metric }
        if ($BeforeRuntime) { $summaries += Paired-Summary $kind 'source' 'before' $metric }
        if ($IncludeControl) { $summaries += Paired-Summary $kind 'control' 'source' $metric }
    }
}
$parallelism = foreach ($workload in $workloads) {
    foreach ($variant in $variants) {
        $selected = @($records | Where-Object { $_.Kind -eq $workload.Kind -and $_.Variant -eq $variant.Name })
        $wall = ($selected | ForEach-Object { $_.Measurement.wall } | Measure-Object -Sum).Sum
        $cpu = ($selected | ForEach-Object { $_.Measurement.cpu } | Measure-Object -Sum).Sum
        if ($wall -le 0) { throw 'Invalid wall time for parallelism diagnostic.' }
        [pscustomobject]@{ Kind=$workload.Kind; Variant=$variant.Name; AggregateCPUPerWall=$cpu/$wall }
    }
}
$report = [pscustomobject]@{
    Rounds=$Rounds; Samples=$Samples; Threads=$Threads; Affinity=$Affinity; TimestampUTC=[DateTime]::UtcNow.ToString('o')
    IdenticalRuntimeControl=([bool]$BeforeRuntime -and (Resolve-Path -LiteralPath $BeforeRuntime).Path -eq $current)
    InterleavedSourceControl=$IncludeControl.IsPresent
    Processor=$env:PROCESSOR_IDENTIFIER; RuntimeProvenance=$provenance; BuildProvenance=$buildProvenance; Inventory=$inventory
    VerifiedRuntimeDifferences=$runtimeDifferences
    WorkloadSource=@(Get-ChildItem -LiteralPath $source -File | ForEach-Object { [pscustomobject]@{ Name=$_.Name; SHA256=(Get-FileHash -LiteralPath $_.FullName).Hash } })
    ExecutableSHA256=(Get-FileHash -LiteralPath $application).Hash
    InstalledRuntimeRoot=$(if ($IncludeInstalled) { $installedRoot } else { $null })
    InstalledRuntimeMatchesBundled=$installedMatchesBundled
    InstalledExecutableSHA256=$(if ($IncludeInstalled) { (Get-FileHash -LiteralPath $installedApplication).Hash } else { $null })
    Workloads=$workloads; Records=$records; Summaries=$summaries; Parallelism=@($parallelism)
    StartupDefinition='New process to exit with warm filesystem cache; round zero has a fresh gateway cache, later launches retain it. Not an OS-cold disk benchmark.'
    WarmDefinition='One full unmeasured workload before each process-level sample set; process startup excluded.'
    UncertaintyDefinition='Paired process-round log ratios; deterministic percentile bootstrap of the geometric mean, 10000 resamples.'
}
$reportPath = Join-Path $root 'report.json'
$report | ConvertTo-Json -Depth 12 | Set-Content -LiteralPath $reportPath -Encoding utf8
$summaries | Select-Object Kind,Numerator,Denominator,Metric,GeometricMean,Lower95,Upper95,Exploratory | Format-Table -AutoSize
Write-Output "REPORT=$reportPath"
