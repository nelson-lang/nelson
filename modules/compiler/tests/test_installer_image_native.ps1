# Copyright (c) 2026-present Allan CORNET (Nelson)
# SPDX-License-Identifier: LGPL-3.0-or-later
param(
    [Parameter(Mandatory)][ValidateSet('icon', 'bitmap', 'registered', 'unregistered')][string]$Mode,
    [Parameter(Mandatory)][string]$Target,
    [string]$ApplicationName = '',
    [ValidateSet('red', 'blue')][string]$Color = 'red'
)
$ErrorActionPreference = 'Stop'
if ($Mode -eq 'registered' -or $Mode -eq 'unregistered') {
    if ([string]::IsNullOrEmpty($ApplicationName)) { throw 'Expected the unique fixture application name.' }
    $root = [Microsoft.Win32.RegistryKey]::OpenBaseKey('CurrentUser', 'Registry64')
    $uninstall = $root.OpenSubKey('Software\Microsoft\Windows\CurrentVersion\Uninstall')
    $matches = @()
    try {
        # Inno Setup shortens long application IDs in the registry key name.
        if ($null -ne $uninstall) {
            foreach ($name in $uninstall.GetSubKeyNames()) {
                if (-not $name.StartsWith('Nelson-')) { continue }
                $key = $uninstall.OpenSubKey($name)
                try {
                    $displayName = if ($null -ne $key) { [string]$key.GetValue('DisplayName') } else { '' }
                    if ($displayName.StartsWith($ApplicationName, [StringComparison]::Ordinal)) {
                        $matches += $key.GetValue('DisplayIcon')
                    }
                } finally { if ($null -ne $key) { $key.Dispose() } }
            }
        }
        if ($Mode -eq 'unregistered') {
            if ($matches.Count -ne 0) { throw 'Unexpected installer registration.' }
        } else {
            if ($matches.Count -ne 1) { throw 'Expected exactly one installer registration.' }
            $expected = [IO.Path]::GetFullPath((Join-Path $Target 'nelson-installer-assets/AddRemoveProgramsIcon.ico'))
            if ($matches[0] -ne $expected) { throw 'Incorrect installed application-list icon.' }
        }
    } finally {
        if ($null -ne $uninstall) { $uninstall.Dispose() }
        $root.Dispose()
    }
    exit 0
}
Add-Type -AssemblyName System.Drawing
if ($Mode -eq 'bitmap') {
    $bitmap = [Drawing.Bitmap]::new($Target)
    try {
        if ($bitmap.Width -ne 112 -or $bitmap.Height -ne 290) { throw 'Wrong installer logo dimensions.' }
        if ($bitmap.GetPixel(56, 72).ToArgb() -ne [Drawing.Color]::FromArgb(255, 127, 255, 127).ToArgb()) {
            throw 'Incorrect transparent logo compositing.'
        }
        if ($bitmap.GetPixel(56, 218).ToArgb() -ne [Drawing.Color]::Yellow.ToArgb()) {
            throw 'Incorrect installer logo pixels.'
        }
    } finally { $bitmap.Dispose() }
    exit 0
}
Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public static class InstallerIconReader {
 [DllImport("shell32.dll", CharSet=CharSet.Unicode)]
 public static extern uint ExtractIconEx(string file, int index, [Out] IntPtr[] large, [Out] IntPtr[] small, uint count);
 [DllImport("user32.dll")] public static extern bool DestroyIcon(IntPtr icon);
}
'@
$large = [IntPtr[]]::new(1)
$small = [IntPtr[]]::new(1)
try {
    $count = [InstallerIconReader]::ExtractIconEx($Target, 0, $large, $small, 1)
    if ($count -eq 0 -or $count -eq [uint32]::MaxValue -or $large[0] -eq [IntPtr]::Zero -or $small[0] -eq [IntPtr]::Zero) {
        throw "Windows cannot extract the installer icon: $Target (count=$count)."
    }
    foreach ($handle in @($large[0], $small[0])) {
        $icon = [Drawing.Icon]::FromHandle($handle)
        $bitmap = $icon.ToBitmap()
        try {
            $pixel = $bitmap.GetPixel([int]($bitmap.Width / 2), [int]($bitmap.Height / 2))
            $expected = if ($Color -eq 'red') { [Drawing.Color]::Red } else { [Drawing.Color]::Blue }
            if ($pixel.ToArgb() -ne $expected.ToArgb()) { throw 'Windows displays the wrong icon pixels.' }
        } finally { $bitmap.Dispose(); $icon.Dispose() }
    }
} finally {
    if ($large[0] -ne [IntPtr]::Zero) { $null = [InstallerIconReader]::DestroyIcon($large[0]) }
    if ($small[0] -ne [IntPtr]::Zero) { $null = [InstallerIconReader]::DestroyIcon($small[0]) }
}
