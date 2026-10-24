# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
$ErrorActionPreference = 'Stop'
$tokens = $null
$errors = $null
$ast = [Management.Automation.Language.Parser]::ParseFile((Join-Path $PSScriptRoot 'bench_nelsonc.ps1'), [ref]$tokens, [ref]$errors)
if ($errors.Count) { throw ($errors | Out-String) }
foreach ($name in @('Get-PerformanceVariants', 'Invoke-Workload', 'Median', 'Paired-Summary')) {
    $definitions = @($ast.FindAll({ param($node) $node -is [Management.Automation.Language.FunctionDefinitionAst] -and $node.Name -eq $name }, $true))
    if ($definitions.Count -ne 1) { throw "Expected one function: $name" }
    . ([scriptblock]::Create($definitions[0].Extent.Text))
}
function Check([bool]$Condition) { if (-not $Condition) { throw 'Performance control assertion failed.' } }
foreach ($hasBefore in @($false, $true)) {
    foreach ($hasInstalled in @($false, $true)) {
        foreach ($hasControl in @($false, $true)) {
            $before = if ($hasBefore) { $PSScriptRoot } else { '' }
            $installed = if ($hasInstalled) { Join-Path $PSScriptRoot 'installed' } else { '' }
            $variants = @(Get-PerformanceVariants $PSScriptRoot $before $installed $hasControl)
            Check ($variants.Count -eq 2 + [int]$hasBefore + [int]$hasInstalled + [int]$hasControl)
            Check (@($variants.Name | Sort-Object -Unique).Count -eq $variants.Count)
            Check (@($variants | Where-Object Name -EQ before).Count -eq [int]$hasBefore)
            Check (@($variants | Where-Object Name -EQ installed).Count -eq [int]$hasInstalled)
            Check (@($variants | Where-Object Name -EQ control).Count -eq [int]$hasControl)
            if ($hasControl) {
                Check (($variants | Where-Object Name -EQ control).Runtime -ceq ($variants | Where-Object Name -EQ source).Runtime)
            }
        }
    }
}
function Invoke-MeasuredProcess([string]$Executable, [string[]]$Arguments, [string]$Variant) {
    return [pscustomobject]@{ Executable=$Executable; Arguments=$Arguments; Variant=$Variant }
}
$source = Join-Path $PSScriptRoot 'source fixture'
$Samples = 3
$Threads = 1
$variants = @(Get-PerformanceVariants $PSScriptRoot '' '' $true)
foreach ($kind in @('scalar', 'startup')) {
    $currentCall = Invoke-Workload ($variants | Where-Object Name -EQ source) $kind 5
    $controlCall = Invoke-Workload ($variants | Where-Object Name -EQ control) $kind 5
    Check ($currentCall.Executable -ceq $controlCall.Executable)
    Check (-not (Compare-Object $currentCall.Arguments $controlCall.Arguments -SyncWindow 0))
    Check ($currentCall.Variant -eq 'source' -and $controlCall.Variant -eq 'control')
}
$Rounds = 10
$records = @()
foreach ($round in 1..$Rounds) {
    foreach ($item in @(@('before', 9.0), @('source', 18.0), @('control', 20.0))) {
        $records += [pscustomobject]@{ Round=$round; Kind='scalar'; Variant=$item[0]; Measurement=[pscustomobject]@{ cpu=@($item[1], $item[1]) } }
    }
}
$actual = Paired-Summary 'scalar' 'source' 'before' 'cpu'
$control = Paired-Summary 'scalar' 'control' 'source' 'cpu'
Check ([Math]::Abs($actual.GeometricMean - 2.0) -lt 1e-12)
Check ([Math]::Abs($control.GeometricMean - 20.0 / 18.0) -lt 1e-12)
Check ($control.Numerator -eq 'control' -and $control.Denominator -eq 'source')
Check ([Math]::Abs($actual.Lower95 - 2.0) -lt 1e-12 -and [Math]::Abs($actual.Upper95 - 2.0) -lt 1e-12)
Write-Output 'PASS interleaved performance control'
