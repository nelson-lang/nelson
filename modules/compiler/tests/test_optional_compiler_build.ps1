#Requires -Version 7.0
# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param(
    [string]$CMake = 'cmake',
    [Parameter(Mandatory)][string]$MSBuild
)
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
& $CMake -P (Join-Path $PSScriptRoot 'test_optional_compiler_build.cmake')
if ($LASTEXITCODE -ne 0) { throw 'Compiler CMake selection failed.' }
$project = Join-Path $repo 'modules/main/nelson_cli/NelSon-cli.vcxproj'
foreach ($platform in @('x64', 'ARM64')) {
    foreach ($configuration in @('Release', 'Debug')) {
        foreach ($disabled in @('false', 'true')) {
            $result = & $MSBuild $project -nologo -getItem:ProjectReference `
                "/p:Platform=$platform" "/p:Configuration=$configuration" `
                "/p:WithoutCompilerModule=$disabled" "/p:SolutionDir=$repo/"
            if ($LASTEXITCODE -ne 0) { throw 'MSBuild evaluation failed.' }
            $references = ($result | ConvertFrom-Json).Items.ProjectReference.Identity
            $launchers = @($references | Where-Object { $_ -like '*\nelsonc\*.vcxproj' })
            $expected = if ($disabled -eq 'true') { 0 } else { 3 }
            if ($launchers.Count -ne $expected) {
                throw "Unexpected compiler references: $configuration $platform $disabled"
            }
            if (@($references | Where-Object { $_ -like '*\nlsEngine.vcxproj' }).Count -ne 1) {
                throw 'Compiler exclusion changed the engine dependency.'
            }
        }
    }
}
$gateway = Join-Path $repo 'modules/compiler/builtin/c/nlsCompiler_builtin.vcxproj'
$result = & $MSBuild $gateway -nologo -getItem:ProjectReference /p:Platform=x64 /p:Configuration=Release "/p:SolutionDir=$repo/"
if ($LASTEXITCODE -ne 0) { throw 'Compiler gateway evaluation failed.' }
$helper = @(($result | ConvertFrom-Json).Items.ProjectReference | Where-Object { $_.Identity -like '*\nelson-installer.vcxproj' })
if ($helper.Count -ne 1 -or $helper[0].LinkLibraryDependencies -ne 'false') {
    throw 'The optional compiler gateway must build the standalone installer template without linking it.'
}
'PASS optional compiler MSBuild selection: 8 combinations'

$projects = @(
    'modules/main/nelsonc/nelsonc.vcxproj',
    'modules/compiler/builtin/c/nlsCompiler_builtin.vcxproj',
    'modules/compiler/src/c/nlsCompiler.vcxproj'
)
foreach ($project in $projects) {
    foreach ($platform in @('x64', 'ARM64')) {
        foreach ($configuration in @('Release', 'Debug')) {
            foreach ($context in @('Project', 'Solution', 'VisualStudio')) {
                $arguments = @(
                    '-nologo', '/nr:false', '-target:AssignProjectConfiguration',
                    '-getItem:_ProjectReferenceWithConfiguration',
                    "/p:Platform=$platform", "/p:Configuration=$configuration",
                    "/p:SolutionDir=$repo/",
                    "/p:BuildingSolutionFile=$($context -eq 'Solution')",
                    "/p:BuildingInsideVisualStudio=$($context -eq 'VisualStudio')"
                )
                $result = & $MSBuild (Join-Path $repo $project) @arguments
                if ($LASTEXITCODE -ne 0) { throw 'Project configuration assignment failed.' }
                $references = @(($result | ConvertFrom-Json).Items._ProjectReferenceWithConfiguration)
                if ($references.Count -eq 0) { throw "Missing references in $project" }
                foreach ($reference in $references) {
                    $removed = $reference.GlobalPropertiesToRemove -split ';'
                    if ('Configuration' -in $removed -or 'Platform' -in $removed) {
                        throw "Lost inherited configuration: $project $configuration $platform $context"
                    }
                }

                if ($context -ne 'Solution') { continue }
                # Explicit solution mappings must still take precedence over inheritance.
                $mappedConfiguration = if ($configuration -eq 'Release') { 'Debug' } else { 'Release' }
                $mappedPlatform = if ($platform -eq 'x64') { 'ARM64' } else { 'x64' }
                $solution = [xml]'<SolutionConfiguration />'
                foreach ($reference in $references) {
                    $entry = $solution.CreateElement('ProjectConfiguration')
                    $entry.SetAttribute('Project', $reference.Project)
                    $entry.SetAttribute('AbsolutePath', $reference.FullPath)
                    $entry.SetAttribute('BuildProjectInSolution', 'True')
                    $entry.InnerText = "$mappedConfiguration|$mappedPlatform"
                    [void]$solution.DocumentElement.AppendChild($entry)
                }
                $xml = $solution.OuterXml.Replace('%', '%25').Replace('"', '%22').Replace(';', '%3B')
                $result = & $MSBuild (Join-Path $repo $project) @arguments "/p:CurrentSolutionConfigurationContents=$xml"
                if ($LASTEXITCODE -ne 0) { throw 'Explicit solution mapping failed.' }
                $mapped = @(($result | ConvertFrom-Json).Items._ProjectReferenceWithConfiguration)
                if ($mapped.Count -ne $references.Count) { throw 'Solution mapping lost project references.' }
                foreach ($reference in $mapped) {
                    if ($reference.SetConfiguration -ne "Configuration=$mappedConfiguration" -or
                        $reference.SetPlatform -ne "Platform=$mappedPlatform") {
                        throw "Ignored explicit solution mapping: $project $configuration $platform"
                    }
                }
            }
        }
    }
}
'PASS compiler configuration inheritance: 36 combinations and 12 explicit solution mappings'
