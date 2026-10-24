param(
    [ValidateSet("Debug", "Release")]
    [string]$Configuration = "Release",

    [ValidateSet("x64", "ARM64")]
    [string]$Platform = "x64",

    [string]$StampPath = "",

    [switch]$ListOnly,

    [switch]$Parallel,

    [int]$MaxParallel = 3,

    [string]$HarnessName = ""
)

$ErrorActionPreference = "Stop"

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..\..")
$cdoctestTempRoot = Join-Path $repoRoot "temp\cdoctest"
$runtimeBin = Join-Path $repoRoot "bin\$Platform"
$cmake = Join-Path $repoRoot "tools\cmake\bin\cmake.exe"
$ctest = Join-Path $repoRoot "tools\cmake\bin\ctest.exe"

if (-not (Test-Path $cmake)) {
    throw "CMake not found: $cmake"
}
if (-not (Test-Path $ctest)) {
    throw "CTest not found: $ctest"
}

$harnesses = @(
    @{
        Name = "nflow_block_handlers_tests"
        Source = "modules\nflow_blocks\tests\cpp"
    },
    @{
        Name = "nflow_engine_tests"
        Source = "modules\nflow_engine\tests\cpp"
    },
    @{
        Name = "ode_solvers_tests"
        Source = "modules\ode_solvers\tests\cpp"
    },
    @{
        Name = "nflow_gui_tests"
        Source = "modules\nflow_gui\tests\cpp"
    },
    @{
        Name = "interpreter_tests"
        Source = "modules\interpreter\tests\cpp"
    },
    @{
        Name = "ipc_file_association_tests"
        Source = "modules\ipc\tests\cpp"
    },
    @{
        Name = "webview_tests"
        Source = "modules\webview\tests\cpp"
    },
    @{
        Name = "web_gui_tests"
        Source = "modules\web_gui\tests\cpp"
    },
    @{
        Name = "gui_tests"
        Source = "modules\gui\tests\cpp"
        RequiresQt = $true
    }
)

function Resolve-QtDir {
    if ($env:QTDIR -and (Test-Path $env:QTDIR)) {
        return (Resolve-Path $env:QTDIR).Path
    }
    if ($Platform -eq "ARM64") {
        $candidate = Join-Path $repoRoot "..\qt_woa64\6.11.0\msvc2022_arm64"
    } else {
        $candidate = Join-Path $repoRoot "..\qt_windows_x64\6.11.0\msvc2022_64"
    }
    if (Test-Path $candidate) {
        return (Resolve-Path $candidate).Path
    }
    throw "Qt not found: set QTDIR or install Qt at $candidate"
}

if ($ListOnly) {
    foreach ($harness in $harnesses) {
        $sourceDir = Join-Path $repoRoot $harness.Source
        if (-not (Test-Path (Join-Path $sourceDir "CMakeLists.txt"))) {
            throw "CMakeLists.txt not found for $($harness.Name): $sourceDir"
        }
        Write-Host "$($harness.Name): $sourceDir"
    }
    exit 0
}

function Invoke-CdoctestHarness {
    param(
        [hashtable]$Harness
    )

    $harness = $Harness
    $sourceDir = Join-Path $repoRoot $harness.Source
    $buildDir = Join-Path $repoRoot "temp\cdoctest\$Platform\$Configuration\$($harness.Name)"
    $harnessStamp = Join-Path $buildDir ".cdoctest.stamp"

    if ((Test-Path $buildDir) -and -not (Test-Path $harnessStamp)) {
        if (-not $buildDir.StartsWith($cdoctestTempRoot, [System.StringComparison]::OrdinalIgnoreCase)) {
            throw "Refusing to remove unexpected build directory: $buildDir"
        }
        Write-Host "==> Removing incomplete $($harness.Name) build directory"
        try {
            Remove-Item -LiteralPath $buildDir -Recurse -Force
        } catch {
            $buildDir = Join-Path $repoRoot "temp\cdoctest\$Platform\$Configuration\$($harness.Name)-retry-$PID"
            $harnessStamp = Join-Path $buildDir ".cdoctest.stamp"
            Write-Host "==> Incomplete build directory is locked, using: $buildDir"
        }
    }

    Write-Host "==> Configuring $($harness.Name)"
    $configureArgs = @("-S", $sourceDir, "-B", $buildDir, "-A", $Platform)
    if ($harness.RequiresQt) {
        $qtDir = Resolve-QtDir
        Write-Host "==> Using Qt: $qtDir"
        $configureArgs += "-DCMAKE_PREFIX_PATH=$qtDir"
    }
    & $cmake @configureArgs
    if ($LASTEXITCODE -ne 0) {
        exit $LASTEXITCODE
    }

    Write-Host "==> Building $($harness.Name) [$Configuration|$Platform]"
    & $cmake --build $buildDir --config $Configuration
    if ($LASTEXITCODE -ne 0) {
        exit $LASTEXITCODE
    }

    Write-Host "==> Running $($harness.Name)"
    $oldPath = $env:PATH
    $oldQtPluginPath = $env:QT_QPA_PLATFORM_PATH
    if (Test-Path $runtimeBin) {
        Write-Host "==> Adding runtime DLL directory to PATH: $runtimeBin"
        $env:PATH = "$runtimeBin$([IO.Path]::PathSeparator)$oldPath"
    } else {
        Write-Host "==> Runtime DLL directory not found: $runtimeBin"
    }
    if ($harness.RequiresQt) {
        $qtDir = Resolve-QtDir
        $env:PATH = "$qtDir\bin$([IO.Path]::PathSeparator)$env:PATH"
        $env:QT_QPA_PLATFORM_PATH = "$qtDir\plugins\platforms"
    }
    try {
        & $ctest --test-dir $buildDir -C $Configuration --output-on-failure
        if ($LASTEXITCODE -ne 0) {
            exit $LASTEXITCODE
        }
    } finally {
        $env:PATH = $oldPath
    }

    Set-Content -Path $harnessStamp -Value "$($harness.Name) $Configuration|$Platform $(Get-Date -Format o)"
}

if ($HarnessName -ne "") {
    $selectedHarness = $harnesses | Where-Object { $_.Name -eq $HarnessName } | Select-Object -First 1
    if ($null -eq $selectedHarness) {
        throw "Unknown cdoctest harness: $HarnessName"
    }
    Invoke-CdoctestHarness -Harness $selectedHarness
    exit 0
}

if ($Parallel) {
    if ($MaxParallel -lt 1) {
        $MaxParallel = 1
    }

    $pending = New-Object System.Collections.Queue
    foreach ($harness in $harnesses) {
        $pending.Enqueue($harness)
    }

    $running = @()
    while ($pending.Count -gt 0 -or $running.Count -gt 0) {
        while ($pending.Count -gt 0 -and $running.Count -lt $MaxParallel) {
            $harness = $pending.Dequeue()
            Write-Host "==> Starting $($harness.Name)"
            $job = Start-Job -Name $harness.Name -ScriptBlock {
                param($scriptPath, $configuration, $platform, $harnessName)
                & powershell -NoProfile -ExecutionPolicy Bypass -File $scriptPath `
                    -Configuration $configuration `
                    -Platform $platform `
                    -HarnessName $harnessName 2>&1
                if ($LASTEXITCODE -ne 0) {
                    throw "$harnessName failed with exit code $LASTEXITCODE"
                }
            } -ArgumentList $PSCommandPath, $Configuration, $Platform, $harness.Name
            $running += $job
        }

        $completed = Wait-Job -Job $running -Any
        $running = @($running | Where-Object { $_.Id -ne $completed.Id })

        Write-Host "==> Output from $($completed.Name)"
        Receive-Job -Job $completed

        if ($completed.State -ne "Completed") {
            if ($running.Count -gt 0) {
                Stop-Job -Job $running
                Remove-Job -Job $running -Force
            }
            Remove-Job -Job $completed -Force
            exit 1
        }

        Remove-Job -Job $completed
    }
} else {
    foreach ($harness in $harnesses) {
        Invoke-CdoctestHarness -Harness $harness
    }
}

if ($StampPath -ne "") {
    $stampDir = Split-Path -Parent $StampPath
    if ($stampDir -ne "" -and -not (Test-Path $stampDir)) {
        New-Item -ItemType Directory -Path $stampDir -Force | Out-Null
    }
    Set-Content -Path $StampPath -Value "cdoctest $Configuration|$Platform $(Get-Date -Format o)"
}
