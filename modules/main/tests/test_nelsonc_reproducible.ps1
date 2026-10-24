# Dot-sourced by test_nelsonc.ps1; each build uses an independent compiler process.
$reproSource = @()
$reproDistribution = Join-Path $root 'reproducible-distribution'
$reproRelocated = Join-Path $root 'relocated reproducible'
$reproRecords = @()
for ($index = 1; $index -le 2; $index++) {
    $source = Join-Path $root ('reproducible-source-' + $index)
    Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'fixtures/nelsonc_reproducible') -Destination $source -Recurse
    if ($index -eq 2) {
        Get-ChildItem -LiteralPath $source -File -Recurse | ForEach-Object { $_.LastWriteTime = [datetime]'2001-01-02T03:04:06' }
    }
    $reproSource += $source
    foreach ($mode in @('bundled', 'installed')) {
        $application = Join-Path $reproDistribution "$mode/$index/application.exe"
        $environment = @{ TZ = $(if ($index -eq 1) { 'UTC0' } else { 'PST8PDT' }) }
        $arguments = @((Join-Path $source 'repro_entry.m'), '--cli', '--runtime', $mode,
            '-a', (Join-Path $source 'resource.txt'), '--help-file', (Join-Path $source 'resource.txt'), '-o', $application)
        $build = Invoke-Application $compiler $arguments 180000 '' $environment
        if ($build.ExitCode -ne 0) { throw ('Reproducible build failed: ' + $build.Output + $build.Error) }
        $archive = Read-EmbeddedApplication $application
        foreach ($name in @('roots/1/repro_entry.nbc','roots/1/repro_helper.nbc','roots/1/resource.txt')) {
            if ($name -notin $archive.Names) { throw "Missing reproducibility input: $name" }
        }
        if (@($archive.Names | Where-Object { $_.EndsWith('.m') }).Count) { throw 'Reproducibility source leaked into the archive.' }
        $record = [pscustomobject]@{ Mode=$mode; Index=$index; Relative="$mode/$index/application.exe"; SHA256=(Get-FileHash -LiteralPath $application).Hash; TZ=$environment.TZ }
        $reproRecords += $record
    }
}
foreach ($mode in @('bundled','installed')) {
    $records = @($reproRecords | Where-Object Mode -EQ $mode)
    if ($records.Count -ne 2 -or $records[0].SHA256 -ne $records[1].SHA256) { throw "Independent $mode builds differ." }
}
foreach ($path in @($reproSource) + @($reproDistribution, $reproRelocated)) {
    if (-not [IO.Path]::GetFullPath($path).StartsWith($root + [IO.Path]::DirectorySeparatorChar)) { throw 'Invalid reproducibility test path.' }
}
foreach ($source in $reproSource) { Remove-Item -LiteralPath $source -Recurse -Force }
Move-Item -LiteralPath $reproDistribution -Destination $reproRelocated
foreach ($record in $reproRecords) {
    $application = Join-Path $reproRelocated $record.Relative
    $environment = @{ PATH=''; NELSONC_RUNTIME_ROOT=(Join-Path $reproRelocated 'bundled/1/application.runtime') }
    $run = Invoke-Application $application @() 30000 '' $environment
    if ($run.ExitCode -ne 0 -or $run.Output -notmatch '42:REPRO_PROCESS_OK') { throw ('Reproducible output failed after relocation: ' + $run.Output + $run.Error) }
    if ((Get-FileHash -LiteralPath $application).Hash -ne $record.SHA256) { throw 'Executable changed after deployment.' }
}
foreach ($index in @(1, 2)) { $null = Read-CheckedRuntime (Join-Path $reproRelocated "bundled/$index/application.runtime") }
$reproRecords | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $root 'reproducibility.json')
Write-Output 'PASS byte-identical independent builds, changed source timestamps and time zones, anonymous closure, both runtime modes and source-free relocation'
