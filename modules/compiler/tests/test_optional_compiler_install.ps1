#Requires -Version 7.0
# Copyright (c) 2016-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param(
    [Parameter(Mandatory)][string]$Runtime,
    [string]$ISCC = "${env:ProgramFiles(x86)}/Inno Setup 6/ISCC.exe"
)
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$root = Join-Path $repo ('temp/compiler-installer-' + [Guid]::NewGuid().ToString('N'))
$stage = Join-Path $root 'source with spaces'
$binary = Join-Path $stage 'bin/x64'
$module = Join-Path $stage 'modules/compiler'
New-Item -ItemType Directory -Path $binary, $module | Out-Null
$native = @('libnlsCompiler.dll', 'libnlsCompiler_builtin.dll', 'nelsonc.exe', 'nelson-app.exe', 'nelson-app-windowed.exe', 'nelson-installer.exe')
foreach ($name in $native) {
    Copy-Item -LiteralPath (Join-Path $Runtime $name) -Destination $binary
}
$source = Join-Path $repo 'modules/compiler'
foreach ($name in @('loader.m', 'etc', 'functions', 'tests', 'help', 'examples', 'resources')) {
    Copy-Item -LiteralPath (Join-Path $source $name) -Destination $module -Recurse
}
$fixture = Join-Path $PSScriptRoot 'fixtures/optional_compiler_install.iss'

function Copy-ModuleManager([string]$Destination) {
    $folder = Join-Path $Destination 'modules/modules_manager'
    $bin = Join-Path $Destination 'bin/x64'
    New-Item -ItemType Directory -Path $folder, $bin -Force | Out-Null
    foreach ($name in @('loader.m', 'etc', 'functions', 'tests', 'help')) {
        Copy-Item -LiteralPath (Join-Path $repo "modules/modules_manager/$name") -Destination $folder -Recurse
    }
    foreach ($name in @('libnlsModules_manager.dll', 'libnlsModules_manager_builtin.dll')) {
        Copy-Item -LiteralPath (Join-Path $Runtime $name) -Destination $bin
    }
}
Copy-ModuleManager $stage

function Build-Installer([string]$Name, [string]$SourceRoot, [string[]]$Defines) {
    $output = Join-Path $root $Name
    New-Item -ItemType Directory -Path $output | Out-Null
    & $ISCC /Qp "/DRepositoryRoot=$repo" "/DStageRoot=$SourceRoot" "/O$output" @Defines $fixture `
        *> (Join-Path $output 'compile.log')
    if ($LASTEXITCODE -ne 0) { throw "Installer compile failed: $output" }
    return Join-Path $output 'compiler-test.exe'
}

function Invoke-Installer([string]$Executable, [string]$Name, [string[]]$Selection, [bool]$Success) {
    $destination = Join-Path $root $Name
    $log = Join-Path $root ($Name + '.log')
    $info = [Diagnostics.ProcessStartInfo]::new($Executable)
    $info.UseShellExecute = $false
    $info.CreateNoWindow = $true
    $info.WindowStyle = [Diagnostics.ProcessWindowStyle]::Hidden
    $info.Environment['TEMP'] = $root
    $info.Environment['TMP'] = $root
    if (-not ($Selection -like '/LANG=*')) { $info.ArgumentList.Add('/LANG=english') }
    foreach ($argument in @('/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART', '/SP-', "/DIR=$destination", "/LOG=$log") + $Selection) {
        $info.ArgumentList.Add($argument)
    }
    $process = [Diagnostics.Process]::Start($info)
    try {
        if (-not $process.WaitForExit(120000)) {
            $process.Kill($true)
            $process.WaitForExit()
            throw "Isolated installer timed out: $Name"
        }
        if (($process.ExitCode -eq 0) -ne $Success) { throw "Unexpected installer result: $Name; see $log" }
    } finally { $process.Dispose() }
    return $destination
}

function Assert-CompilerFiles([string]$Destination, [bool]$Present, [bool]$Tests, [bool]$Help = $false, [bool]$Base = $true) {
    foreach ($name in @('__load_compiler__', 'ncc', 'deploytool', 'standaloneApplicationCompiler')) {
        $relative = "modules/modules_manager/functions/$name.m"
        $installed = Join-Path $Destination $relative
        if ((Test-Path -LiteralPath $installed) -ne $Base) { throw "Unexpected compiler command: $name" }
        if ($Base -and (Get-FileHash -LiteralPath $installed).Hash -ne (Get-FileHash -LiteralPath (Join-Path $repo $relative)).Hash) {
            throw "Installed compiler command differs: $name"
        }
    }
    foreach ($name in $native) {
        $installed = Join-Path $Destination ('bin/x64/' + $name)
        if ((Test-Path -LiteralPath $installed) -ne $Present) { throw "Unexpected installed binary: $installed" }
        if ($Present -and (Get-FileHash -LiteralPath $installed).Hash -ne (Get-FileHash -LiteralPath (Join-Path $binary $name)).Hash) {
            throw "Installed binary differs: $name"
        }
    }
    $installedModule = Join-Path $Destination 'modules/compiler'
    if ((Test-Path -LiteralPath $installedModule) -ne $Present) { throw 'Unexpected compiler module directory.' }
    if ($Present) {
        foreach ($name in @('loader.m', 'etc/startup.m', 'functions/__nelsonc_command__.m',
            'functions/+nelson/+compiler/BuildOptions.m', 'functions/+nelson/+compiler/BuildResult.m',
            'functions/+nelson/+compiler/build.m', 'functions/+nelson/+compiler/analyze.m',
            'functions/+compiler/+build/StandaloneApplicationOptions.m', 'functions/+compiler/+build/Results.m',
            'functions/+compiler/+build/standaloneApplication.m', 'functions/+compiler/+build/standaloneWindowsApplication.m',
            'functions/+compiler/+runtime/Dependencies.m', 'functions/__nelsonc_standalone__.m',
            'functions/__nelsonc_option_pairs__.m', 'functions/__nelsonc_included_packages__.m',
            'functions/__nelsonc_build_report__.m', 'functions/__nelsonc_distribution_plan__.m',
            'functions/__nelsonc_project__.m', 'functions/__nelsonc_editor__.m',
            'functions/__nelsonc_project_sources__.m', 'functions/__nelsonc_source_fingerprint__.m',
            'functions/__nelsonc_editor_action__.m', 'functions/__nelsonc_editor_icon__.m',
            'resources/editor/action-folder.png', 'resources/editor/document-new.png',
            'resources/runtime-install.iss', 'resources/runtime-recovery.iss',
            'resources/installer-shortcuts.iss', 'functions/__nelsonc_inno_build__.m',
            'resources/installer-paths.iss', 'resources/application-runtime.iss',
            'functions/__nelsonc_windows_runtime_package__.m', 'functions/__nelsonc_windows_runtime_script__.m',
            'functions/__nelsonc_linux_installer__.m', 'resources/linux/build.sh',
            'resources/linux/common.sh', 'resources/linux/install.sh', 'resources/linux/uninstall.sh',
            'resources/linux/options.sh', 'resources/linux/build-runtime.sh', 'resources/linux/runtime.sh',
            'functions/__nelsonc_linux_runtime_installer__.m',
            'functions/__nelsonc_linux_runtime_package__.m',
            'resources/linux/runtime-location.sh', 'resources/linux/shared-runtime.sh',
            'examples/standalone/app_entry.m', 'examples/standalone/helper_value.m', 'examples/standalone/factor.txt',
            'examples/embedded_data/embedded_data_entry.m')) {
            $installed = Join-Path $installedModule $name
            if ((Get-FileHash -LiteralPath $installed).Hash -ne (Get-FileHash -LiteralPath (Join-Path $module $name)).Hash) {
                throw "Installed module data differs: $name"
            }
        }
    }
    if ((Test-Path -LiteralPath (Join-Path $installedModule 'tests')) -ne $Tests) { throw 'Unexpected compiler tests.' }
    if ($Tests) {
        foreach ($name in @('test_compiler_project_sources.m', 'test_deploytool_sources.m',
            'test_compiler_source_timestamps.ps1', 'test_application_republish_lock.ps1',
            'test_application_publication_recovery.m', 'test_compiler_publication_recovery.m',
            'test_publication_process_recovery.ps1', 'test_runtime_recovery_process.ps1',
            'runtime_recovery_script.m', 'test_compiler_runtime_recovery.m',
            'test_compiler_installer_windows_cli.m',
            'test_compiler_runtime_installer_windows_cli.m',
            'test_runtime_installer_cli_registration.ps1',
            'test_compiler_installer_windows_shortcuts.m',
            'test_compiler_installer_runtime_folder_windows.m', 'test_installer_runtime_folder_windows.ps1',
            'test_compiler_windows_runtime_script.m',
            'test_compiler_runtime_recovery_application.m')) {
            $relative = "tests/$name"
            if ((Get-FileHash -LiteralPath (Join-Path $installedModule $relative)).Hash -ne
                (Get-FileHash -LiteralPath (Join-Path $module $relative)).Hash) {
                throw "Installed test helper differs: $name"
            }
        }
    }
    if ((Test-Path -LiteralPath (Join-Path $installedModule 'help')) -ne $Help) { throw 'Unexpected compiler help.' }
    if ($Help) {
        foreach ($language in @('en_US', 'fr_FR')) {
            $archives = @(Get-ChildItem -LiteralPath (Join-Path $module 'help') -Filter "*.modules.compiler.help.$language.nhz" -File)
            if ($archives.Count -ne 1) { throw "Expected one localized help archive: $language" }
            $relative = "help/" + $archives[0].Name
            $installed = Join-Path $installedModule $relative
            if ((Get-FileHash -LiteralPath $installed).Hash -ne (Get-FileHash -LiteralPath (Join-Path $module $relative)).Hash) {
                throw "Installed help differs: $relative"
            }
            if (Test-Path -LiteralPath (Join-Path $installedModule "help/$language/xml")) {
                throw 'Help sources should not be installed.'
            }
        }
    }
}

$installer = Build-Installer 'enabled' $stage @()
$destination = Invoke-Installer $installer 'full' @('/TYPE=full') $true
Assert-CompilerFiles $destination $true $true $true
$destination = Invoke-Installer $installer 'minimal' @('/TYPE=minimal') $true
Assert-CompilerFiles $destination $false $false
$dependencies = 'Nelson,FILE_ARCHIVER,DYNAMIC_LINK,JSON'
$destination = Invoke-Installer $installer 'dependencies-only' @("/COMPONENTS=$dependencies") $true
Assert-CompilerFiles $destination $false $false
$destination = Invoke-Installer $installer 'selected' @("/COMPONENTS=$dependencies,COMPILER") $true
Assert-CompilerFiles $destination $true $false
$destination = Invoke-Installer $installer 'selected-help' @("/COMPONENTS=$dependencies,COMPILER,HELP_FILES") $true
Assert-CompilerFiles $destination $true $false $true
foreach ($missing in @('FILE_ARCHIVER', 'DYNAMIC_LINK', 'JSON')) {
    $selection = @('Nelson', 'FILE_ARCHIVER', 'DYNAMIC_LINK', 'JSON', 'COMPILER') | Where-Object { $_ -ne $missing }
    $destination = Invoke-Installer $installer ('missing-' + $missing) @('/COMPONENTS=' + ($selection -join ',')) $false
    Assert-CompilerFiles $destination $false $false $false $false
    if ((Get-Content -LiteralPath (Join-Path $root ('missing-' + $missing + '.log')) -Raw) -notlike '*compiler module requires*') {
        throw 'Expected compiler dependency diagnostic was not logged.'
    }
}
$destination = Invoke-Installer $installer 'missing-french' @('/LANG=french', '/COMPONENTS=Nelson,COMPILER') $false
Assert-CompilerFiles $destination $false $false $false $false
if ((Get-Content -LiteralPath (Join-Path $root 'missing-french.log') -Raw) -notlike '*Le module compiler requiert*') {
    throw 'Expected French compiler dependency diagnostic was not logged.'
}
$empty = Join-Path $root 'no compiler files'
New-Item -ItemType Directory -Path $empty | Out-Null
Copy-ModuleManager $empty
$without = Build-Installer 'excluded' $empty @('/DWITHOUT_COMPILER_MODULE')
$destination = Invoke-Installer $without 'excluded-full' @('/TYPE=full') $true
Assert-CompilerFiles $destination $false $false
# Compile the real installer event handlers, without ever executing them.
$null = Build-Installer 'production-code-enabled' $stage @('/DPRODUCTION_CODE')
$null = Build-Installer 'production-code-excluded' $empty @('/DPRODUCTION_CODE', '/DWITHOUT_COMPILER_MODULE')
'PASS optional compiler installer: 10 isolated installations, 4 installer builds, localized help and tutorial assets'
"COMPILER_INSTALL_TEST=$root"
