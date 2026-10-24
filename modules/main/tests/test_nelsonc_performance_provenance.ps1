# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
$ErrorActionPreference = 'Stop'
. (Join-Path $PSScriptRoot 'nelsonc_performance_provenance.ps1')

function Library([string]$Name, [string]$Hash = ('a' * 64)) {
    return [pscustomobject]@{ Filename=$Name; SHA256=$Hash }
}
function Proof([string]$Variant, [string]$Name, [string]$Project, [string]$Hash) {
    return [pscustomobject]@{ MsbuildVersion='18.8.2'; Builds=@([pscustomobject]@{
        Variant=$Variant; Output=$Name; Project=$Project; SHA256=$Hash
        Arguments=@('/p:Configuration=Release','/p:Platform=x64')
    }) }
}
function Reject([scriptblock]$Operation, [string]$Expected) {
    try { & $Operation | Out-Null } catch {
        if ($_.Exception.Message -notlike "*$Expected*") { throw }
        return
    }
    throw "Expected rejection: $Expected"
}
function Check([bool]$Condition) { if (-not $Condition) { throw 'Provenance assertion failed.' } }

$same = @(Library 'numerical.dll')
Assert-NelsoncPerformanceRuntimeUnchanged $same $same
Reject { Assert-NelsoncPerformanceRuntimeUnchanged $same @() } 'changed during benchmark'
Reject { Assert-NelsoncPerformanceRuntimeUnchanged @() $same } 'changed during benchmark'
Reject { Assert-NelsoncPerformanceRuntimeUnchanged $same @(Library 'numerical.dll' ('b' * 64)) } 'changed during benchmark'
Check (@(Assert-NelsoncPerformanceRuntimePair $same $same $null $null).Count -eq 0)
Reject { Assert-NelsoncPerformanceRuntimePair $same @() $null $null } 'removed'
Reject { Assert-NelsoncPerformanceRuntimePair @() $same $null $null } 'added'
Reject { Assert-NelsoncPerformanceRuntimePair $same @(Library 'numerical.dll' ('b' * 64)) $null $null } 'difference'
Reject { Get-NelsoncPerformanceLibraryMap @($same[0], (Library 'NUMERICAL.DLL')) } 'duplicate'
Reject { Get-NelsoncPerformanceLibraryMap @(Library '../outside.dll') } 'Invalid'
Reject { Get-NelsoncPerformanceLibraryMap @(Library 'numerical.dll' 'truncated') } 'digest'

$name = 'libnlsEngine.dll'
$project = 'engine/src/c/nlsEngine.vcxproj'
$before = @(Library $name)
$after = @(Library $name ('b' * 64))
$beforeBuild = Proof 'before' $name $project ('a' * 64)
$afterBuild = Proof 'after' $name $project ('b' * 64)
$differences = @(Assert-NelsoncPerformanceRuntimePair $before $after $beforeBuild $afterBuild)
Check ($differences.Count -eq 1 -and $differences[0].Change -eq 'RebuiltPair')
Reject { Assert-NelsoncPerformanceRuntimePair $before $after $null $afterBuild } 'tool version'
Reject { Assert-NelsoncPerformanceRuntimePair $before $after $beforeBuild $null } 'tool version'
$afterBuild.MsbuildVersion = 'different'
Reject { Assert-NelsoncPerformanceRuntimePair $before $after $beforeBuild $afterBuild } 'tool version'
$afterBuild.MsbuildVersion = $beforeBuild.MsbuildVersion
$afterBuild.Builds[0].SHA256 = 'c' * 64
Reject { Assert-NelsoncPerformanceRuntimePair $before $after $beforeBuild $afterBuild } 'output proof'
$afterBuild.Builds[0].SHA256 = 'b' * 64
$beforeBuild.Builds[0].SHA256 = 'c' * 64
Reject { Assert-NelsoncPerformanceRuntimePair $before $after $beforeBuild $afterBuild } 'output proof'
$beforeBuild.Builds[0].SHA256 = 'a' * 64
$afterBuild.Builds[0].Project = 'another.vcxproj'
Reject { Assert-NelsoncPerformanceRuntimePair $before $after $beforeBuild $afterBuild } 'output proof'
$afterBuild.Builds[0].Project = $project
$afterBuild.Builds[0].Arguments = @('/p:Configuration=Debug','/p:Platform=x64')
Reject { Assert-NelsoncPerformanceRuntimePair $before $after $beforeBuild $afterBuild } 'configuration'
$afterBuild.Builds[0].Arguments = @('/p:Configuration=Release','/p:Platform=x64')
$afterBuild.Builds += $afterBuild.Builds[0]
Reject { Assert-NelsoncPerformanceRuntimePair $before $after $beforeBuild $afterBuild } 'output proof'

$name = 'libnlsCompiler_builtin.dll'
$after = @($same[0], (Library $name))
$proof = Proof 'after' $name 'compiler/builtin/c/nlsCompiler_builtin.vcxproj' ('a' * 64)
$differences = @(Assert-NelsoncPerformanceRuntimePair $same $after $null $proof)
Check ($differences.Count -eq 1 -and $differences[0].Change -eq 'BuildOnlyAdded')
Reject { Assert-NelsoncPerformanceRuntimePair $same $after $null $null } 'output proof'
Write-Output 'PASS performance runtime provenance controls'
