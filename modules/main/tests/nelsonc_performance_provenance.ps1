# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later

function Get-NelsoncPerformanceRuntimeLibraries([string]$Runtime) {
    foreach ($file in Get-ChildItem -LiteralPath $Runtime -File -Filter '*.dll') {
        [pscustomobject]@{ Filename=$file.Name; SHA256=(Get-FileHash -LiteralPath $file.FullName).Hash }
    }
}

function Get-NelsoncPerformanceLibraryMap([object[]]$Libraries) {
    $map = @{}
    foreach ($library in $Libraries) {
        $name = [string]$library.Filename
        if (-not $name -or $name -ne [IO.Path]::GetFileName($name) -or $map.ContainsKey($name)) {
            throw "Invalid or duplicate runtime library: $name"
        }
        if ([string]$library.SHA256 -notmatch '^[0-9a-fA-F]{64}$') { throw "Invalid runtime digest: $name" }
        $map[$name] = [string]$library.SHA256
    }
    return $map
}

function Assert-NelsoncPerformanceBuildOutput($Build, [string]$Variant, [string]$Project, [string]$Name, [string]$Hash) {
    $outputs = @($Build.Builds | Where-Object { $_.Variant -eq $Variant -and $_.Output -eq $Name })
    if ($outputs.Count -ne 1 -or $outputs[0].SHA256 -ine $Hash -or $outputs[0].Project -ne $Project) {
        throw "Missing or mismatched build output proof: $Variant $Name"
    }
    foreach ($argument in @('/p:Configuration=Release', '/p:Platform=x64')) {
        if ($outputs[0].Arguments -notcontains $argument) { throw "Unmatched build configuration: $Variant $Name" }
    }
}

function Assert-NelsoncPerformanceRuntimeUnchanged([object[]]$Expected, [object[]]$Actual) {
    $expectedMap = Get-NelsoncPerformanceLibraryMap $Expected
    $actualMap = Get-NelsoncPerformanceLibraryMap $Actual
    foreach ($name in @(@($expectedMap.Keys) + @($actualMap.Keys) | Sort-Object -Unique)) {
        if (-not $expectedMap.ContainsKey($name) -or -not $actualMap.ContainsKey($name) -or
            $expectedMap[$name] -ine $actualMap[$name]) { throw "Runtime changed during benchmark: $name" }
    }
}

function Assert-NelsoncPerformanceRuntimePair([object[]]$Before, [object[]]$After, $BeforeBuild, $AfterBuild) {
    $beforeMap = Get-NelsoncPerformanceLibraryMap $Before
    $afterMap = Get-NelsoncPerformanceLibraryMap $After
    $projects = @{
        'libnlsInterpreter.dll' = 'interpreter/src/c/nlsInterpreter.vcxproj'
        'libnlsCore.dll' = 'core/src/c/nlsCore.vcxproj'
        'libnlsInterpreter_builtin.dll' = 'interpreter/builtin/c/nlsInterpreter_builtin.vcxproj'
        'libnlsDynamic_link.dll' = 'dynamic_link/src/c/nlsDynamic_link.vcxproj'
        'libnlsDynamic_link_builtin.dll' = 'dynamic_link/builtin/c/nlsDynamic_link_builtin.vcxproj'
        'libnlsFile_archiver.dll' = 'file_archiver/src/c/nlsFile_archiver.vcxproj'
        'libnlsFile_archiver_builtin.dll' = 'file_archiver/builtin/c/nlsFile_archiver_builtin.vcxproj'
        'libnlsWeb_gui.dll' = 'web_gui/src/c/nlsWeb_gui.vcxproj'
        'libnlsEngine.dll' = 'engine/src/c/nlsEngine.vcxproj'
    }
    $buildOnly = @{
        'libnlsCompiler.dll' = 'compiler/src/c/nlsCompiler.vcxproj'
        'libnlsCompiler_builtin.dll' = 'compiler/builtin/c/nlsCompiler_builtin.vcxproj'
    }
    $names = @(@($beforeMap.Keys) + @($afterMap.Keys) | Sort-Object -Unique)
    foreach ($name in $names) {
        if (-not $beforeMap.ContainsKey($name)) {
            if (-not $buildOnly.ContainsKey($name)) { throw "Uncontrolled added runtime library: $name" }
            Assert-NelsoncPerformanceBuildOutput $AfterBuild 'after' $buildOnly[$name] $name $afterMap[$name]
            [pscustomobject]@{ Filename=$name; Change='BuildOnlyAdded'; BeforeSHA256=$null; AfterSHA256=$afterMap[$name] }
            continue
        }
        if (-not $afterMap.ContainsKey($name)) { throw "Uncontrolled removed runtime library: $name" }
        if ($beforeMap[$name] -ieq $afterMap[$name]) { continue }
        if (-not $projects.ContainsKey($name)) { throw "Uncontrolled library difference: $name" }
        if (-not $BeforeBuild.MsbuildVersion -or $BeforeBuild.MsbuildVersion -ne $AfterBuild.MsbuildVersion) {
            throw "Unmatched build tool version: $name"
        }
        Assert-NelsoncPerformanceBuildOutput $BeforeBuild 'before' $projects[$name] $name $beforeMap[$name]
        Assert-NelsoncPerformanceBuildOutput $AfterBuild 'after' $projects[$name] $name $afterMap[$name]
        [pscustomobject]@{ Filename=$name; Change='RebuiltPair'; BeforeSHA256=$beforeMap[$name]; AfterSHA256=$afterMap[$name] }
    }
}
